use crate::{
    diagnostics::{error::Error, Diagnostics},
    parser::{
        self,
        ast::{self, BinaryOpKind, Expression, FunctionId, Program, Type, UnaryOpKind, VariableId},
    },
    SourceProgram,
};
use rustc_hash::FxHashMap;
use typed_ast::{ExpressionData, Function, FunctionParameter, TypedExpression, TypedProgram};

pub mod typed_ast;
pub mod validation;

#[salsa::tracked]
pub fn typecheck(db: &dyn crate::Db, source_program: SourceProgram) -> Option<TypedProgram<'_>> {
    let program = parser::parse(db, source_program)?;

    let functions = program
        .functions(db)
        .iter()
        .map(|function| typecheck_function(db, *function, program))
        .collect();

    let typechecked = Some(TypedProgram::new(db, functions));

    typechecked
        .inspect(|program| validation::validate_main_function(db, *program))
        .inspect(|program| validation::validate_unique_function_names(db, *program))
}

#[salsa::tracked]
pub fn typecheck_function<'db>(
    db: &'db dyn crate::Db,
    function: ast::Function<'db>,
    program: Program<'db>,
) -> Function<'db> {
    let names_in_scope = function
        .params(db)
        .iter()
        .map(|arg| (arg.name, arg.type_))
        .collect();

    let body = CheckExpression::new(db, program, names_in_scope).check(function.body(db));

    match (function.return_type(db), body.ty) {
        (Type::Error, _) | (_, Type::Error) => {}
        (expected, found) if expected != found => Diagnostics::push(
            db,
            Error::ExpectedReturnType {
                expected,
                expected_span: function.return_type_span(db),
                found,
                found_span: body.span,
            },
        ),
        _ => {}
    }

    let params = function
        .params(db)
        .iter()
        .map(|arg| FunctionParameter {
            name: arg.name,
            name_span: arg.name_span,
            type_: arg.type_,
            type_span: arg.type_span,
        })
        .collect();

    Function::new(
        db,
        function.name(db),
        function.name_span(db),
        function.return_type(db),
        function.return_type_span(db),
        params,
        function.params_span(db),
        body,
    )
}

#[salsa::tracked]
pub fn find_function_untyped<'db>(
    db: &'db dyn crate::Db,
    program: Program<'db>,
    name: FunctionId<'db>,
) -> Option<ast::Function<'db>> {
    program
        .functions(db)
        .iter()
        .find(|function| function.name(db) == name)
        .copied()
}

#[salsa::tracked]
pub fn find_function_typed<'db>(
    db: &'db dyn crate::Db,
    program: TypedProgram<'db>,
    name: FunctionId<'db>,
) -> Option<Function<'db>> {
    program
        .functions(db)
        .iter()
        .find(|function| function.name(db) == name)
        .copied()
}

struct CheckExpression<'db> {
    db: &'db dyn crate::Db,
    program: Program<'db>,
    names_in_scope: FxHashMap<VariableId<'db>, Type>,
}

impl<'db> CheckExpression<'db> {
    fn new(
        db: &'db dyn crate::Db,
        program: Program<'db>,
        names_in_scope: FxHashMap<VariableId<'db>, Type>,
    ) -> Self {
        Self {
            db,
            program,
            names_in_scope,
        }
    }

    fn check(&self, expression: &Expression<'db>) -> TypedExpression<'db> {
        match &expression.data {
            ast::ExpressionData::Integer(value) => TypedExpression {
                span: expression.span,
                ty: Type::Integer,
                data: ExpressionData::Integer(*value),
            },
            ast::ExpressionData::Float(value) => TypedExpression {
                span: expression.span,
                ty: Type::Float,
                data: ExpressionData::Float(*value),
            },
            ast::ExpressionData::Variable(name) => {
                let ty = self.names_in_scope.get(name).copied().unwrap_or_else(|| {
                    Diagnostics::push(
                        self.db,
                        Error::UndefinedVariable {
                            name: name.text(self.db).to_owned(),
                            span: expression.span,
                        },
                    );

                    Type::Error
                });

                TypedExpression {
                    span: expression.span,
                    ty,
                    data: ExpressionData::Variable(*name),
                }
            }
            ast::ExpressionData::Call(function_id, call_args) => {
                let callee = find_function_untyped(self.db, self.program, *function_id);

                TypedExpression {
                    span: expression.span,
                    ty: callee.map_or(Type::Error, |callee| callee.return_type(self.db)),
                    data: ExpressionData::Call(
                        *function_id,
                        call_args
                            .iter()
                            .map(|arg| self.check(arg))
                            .collect::<Vec<_>>(),
                    ),
                }
            }
            ast::ExpressionData::UnaryOp(op, rhs) => {
                let rhs = self.check(rhs);

                let ty = unary_op_type(op.data, rhs.ty);

                TypedExpression {
                    span: expression.span,
                    ty,
                    data: ExpressionData::UnaryOp(*op, Box::new(rhs)),
                }
            }
            ast::ExpressionData::BinaryOp(op, lhs, rhs) => {
                let lhs = self.check(lhs);
                let rhs = self.check(rhs);

                let ty = bin_op_type(op.data, lhs.ty, rhs.ty).unwrap_or_else(|| {
                    Diagnostics::push(
                        self.db,
                        Error::BinaryOpTypeMismatch {
                            op: *op,
                            lhs_ty: lhs.ty,
                            rhs_ty: rhs.ty,
                            lhs_span: lhs.span,
                            rhs_span: rhs.span,
                        },
                    );

                    Type::Error
                });

                TypedExpression {
                    span: expression.span,
                    ty,
                    data: ExpressionData::BinaryOp(*op, Box::new(lhs), Box::new(rhs)),
                }
            }
        }
    }
}

const fn unary_op_type(op: UnaryOpKind, rhs: Type) -> Type {
    match (op, rhs) {
        (UnaryOpKind::Negate, Type::Integer) => Type::Integer,
        (UnaryOpKind::Negate, Type::Float) => Type::Float,

        (_, Type::Error) => Type::Error,
    }
}

const fn bin_op_type(op: BinaryOpKind, lhs: Type, rhs: Type) -> Option<Type> {
    match (op, (lhs, rhs)) {
        (
            BinaryOpKind::Add
            | BinaryOpKind::Subtract
            | BinaryOpKind::Multiply
            | BinaryOpKind::Divide,
            (Type::Integer, Type::Integer),
        ) => Some(Type::Integer),
        (
            BinaryOpKind::Add
            | BinaryOpKind::Subtract
            | BinaryOpKind::Multiply
            | BinaryOpKind::Divide,
            (Type::Float, Type::Float),
        ) => Some(Type::Float),

        (_, (Type::Error, _) | (_, Type::Error)) => Some(Type::Error),

        _ => None,
    }
}
