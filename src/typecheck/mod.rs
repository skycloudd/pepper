use crate::{
    diagnostics::error::Error,
    parser::ast::{self, Ast, BinaryOp},
    scopes::Scopes,
    span::{Span, Spanned},
};
use chumsky::span::Span as _;
use engine::{Engine, TypeId, TypeInfo};
use typed_ast::{
    Expression, Function, FunctionParam, PrimitiveType, Statement, Type, TypedAst, TypedExpression,
};

pub mod engine;
pub mod typed_ast;

#[must_use]
pub fn typecheck(ast: Ast) -> (Option<TypedAst>, Vec<Error>) {
    let mut errors = vec![];

    (Some(Typechecker::new(&mut errors).typecheck(ast)), errors)
}

struct Typechecker<'a> {
    errors: &'a mut Vec<Error>,
    variables: Scopes<&'static str, TypeId>,
    types: Scopes<&'static str, Type>,
    function_signatures: Scopes<&'static str, FunctionSignature>,
    engine: Engine,
}

impl<'a> Typechecker<'a> {
    fn new(errors: &'a mut Vec<Error>) -> Self {
        Self {
            errors,
            variables: Scopes::default(),
            types: Scopes::default(),
            function_signatures: Scopes::default(),
            engine: Engine::default(),
        }
    }

    fn typecheck(&mut self, ast: Ast) -> TypedAst {
        self.add_primitive_types();

        self.add_function_signatures(&ast.functions);

        let functions = ast
            .functions
            .into_iter()
            .map(|function| function.map(|function| self.typecheck_function(function)))
            .collect();

        TypedAst { functions }
    }

    fn add_function_signatures(&mut self, functions: &[Spanned<ast::Function>]) {
        for function in functions {
            let sig = FunctionSignature {
                params: function
                    .0
                    .params
                    .0
                    .clone()
                    .into_iter()
                    .map(|param| param.map(|param| self.lower_type_error(&param.ty.0, param.ty.1)))
                    .collect(),
                return_ty: function.0.return_ty.as_ref().map_or_else(
                    || {
                        let span = function.0.params.1.to_end();
                        Spanned(Type::Primitive(PrimitiveType::Unit), span)
                    },
                    |ty| ty.map_with(|ty, ty_span| self.lower_type_error(ty, ty_span)),
                ),
            };

            self.function_signatures
                .insert(function.0.name.0.resolve(), sig);
        }
    }

    fn typecheck_function(&mut self, function: ast::Function) -> Function {
        let params = function.params.map(|params| {
            params
                .into_iter()
                .map(|param| {
                    let param_ty = param
                        .0
                        .ty
                        .map_with(|ty, ty_span| self.lower_type_error(ty, ty_span));

                    let param_ty_id = self.engine.insert_type(param_ty);

                    self.variables.insert(param.0.name.0.resolve(), param_ty_id);

                    param.map(|param| self.lower_function_param(param))
                })
                .collect()
        });

        let return_ty = function.return_ty.as_ref().map_or_else(
            || {
                let span = params.1.to_end();
                Spanned(Type::Primitive(PrimitiveType::Unit), span)
            },
            |ty| ty.map_with(|ty, ty_span| self.lower_type_error(ty, ty_span)),
        );

        let body = function.body.map(|body| {
            body.into_iter()
                .map(|stmt| stmt.map(|stmt| self.typecheck_statement(stmt)))
                .collect()
        });

        let return_expr = function
            .return_expr
            .map(|expr| expr.map(|expr| self.typecheck_expression(expr)));

        if let Some(return_expr) = &return_expr {
            let return_expr_ty_id = self
                .engine
                .insert_type(return_expr.as_ref().map(|return_expr| return_expr.ty));

            let return_ty_ty_id = self.engine.insert_type(return_ty);

            self.engine
                .unify(return_expr_ty_id, return_ty_ty_id)
                .unwrap_or_else(|err| {
                    let expected = err.a;
                    let found = err.b;

                    self.errors.push(Error::TypeMismatch { expected, found });
                });
        }

        Function {
            name: function.name,
            params,
            return_ty,
            body,
            return_expr,
        }
    }

    fn typecheck_statement(&mut self, stmt: ast::Statement) -> Statement {
        match stmt {
            ast::Statement::Expression(expr) => {
                Statement::Expression(expr.map(|expr| self.typecheck_expression(expr)))
            }
            ast::Statement::Block(stmts) => Statement::Block(stmts.map(|stmts| {
                stmts
                    .into_iter()
                    .map(|stmt| stmt.map(|stmt| self.typecheck_statement(stmt)))
                    .collect()
            })),
            ast::Statement::Let { name, ty, value } => {
                let ty = ty.map(|ty| ty.map_with(|ty, ty_span| self.lower_type_error(ty, ty_span)));

                let ty_ty_id = match ty {
                    Some(ty) => self.engine.insert_type(ty),
                    None => self.engine.insert(Spanned(TypeInfo::Unknown, name.1)),
                };

                let value = value.map(|value| self.typecheck_expression(value));

                let value_ty_id = self
                    .engine
                    .insert_type(value.as_ref().map(|value| value.ty));

                self.engine
                    .unify(ty_ty_id, value_ty_id)
                    .unwrap_or_else(|err| {
                        let expected = err.a;
                        let found = err.b;

                        self.errors.push(Error::TypeMismatch { expected, found });
                    });

                self.variables.insert(name.0.resolve(), ty_ty_id);

                Statement::Let {
                    name,
                    ty: ty.unwrap_or_else(|| {
                        self.engine.reconstruct(ty_ty_id).unwrap_or_else(|err| {
                            self.errors.push(err);

                            Spanned(Type::Error, name.1)
                        })
                    }),
                    value,
                }
            }
            ast::Statement::Assign { name, value } => {
                let value = value.map(|value| self.typecheck_expression(value));

                let name_ty_id = self
                    .variables
                    .get(&name.0.resolve())
                    .copied()
                    .unwrap_or_else(|| {
                        self.errors.push(Error::UndefinedVariable {
                            name: name.0.resolve().to_string(),
                            span: name.1,
                        });

                        self.engine.insert(Spanned(TypeInfo::Unknown, name.1))
                    });

                let value_ty_id = self
                    .engine
                    .insert_type(value.as_ref().map(|value| value.ty));

                self.engine
                    .unify(name_ty_id, value_ty_id)
                    .unwrap_or_else(|err| {
                        let expected = err.a.map_span(|_| name.1);
                        let found = err.b;

                        self.errors.push(Error::TypeMismatch { expected, found });
                    });

                Statement::Assign { name, value }
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn typecheck_expression(&mut self, expr: ast::Expression) -> TypedExpression {
        match expr {
            ast::Expression::Integer(value) => TypedExpression {
                // TODO: Handle different integer sizes
                ty: Type::Primitive(PrimitiveType::Int32),
                expr: Expression::Integer(value),
            },
            ast::Expression::Float(value) => TypedExpression {
                // TODO: Handle different float sizes
                ty: Type::Primitive(PrimitiveType::Float32),
                expr: Expression::Float(value),
            },
            ast::Expression::Bool(value) => TypedExpression {
                ty: Type::Primitive(PrimitiveType::Bool),
                expr: Expression::Bool(value),
            },
            ast::Expression::Variable(name) => {
                let ty = if let Some(ty) = self.variables.get(&name.0.resolve()) {
                    self.engine.reconstruct(*ty).map_or_else(
                        |err| {
                            self.errors.push(err);
                            Type::Error
                        },
                        |ty| ty.0,
                    )
                } else {
                    self.errors.push(Error::UndefinedVariable {
                        name: name.0.resolve().to_string(),
                        span: name.1,
                    });

                    Type::Error
                };

                TypedExpression {
                    ty,
                    expr: Expression::Variable(name),
                }
            }
            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = lhs.map(|lhs| self.typecheck_expression(*lhs));
                let rhs = rhs.map(|rhs| self.typecheck_expression(*rhs));

                let lhs_ty_id = self.engine.insert_type(lhs.as_ref().map(|lhs| lhs.ty));
                let rhs_ty_id = self.engine.insert_type(rhs.as_ref().map(|rhs| rhs.ty));

                self.engine
                    .unify(lhs_ty_id, rhs_ty_id)
                    .unwrap_or_else(|err| {
                        let expected = err.a.map(|expected| {
                            self.engine.info_to_type(expected).unwrap_or_else(|err| {
                                self.errors.push(err);
                                Type::Error
                            })
                        });

                        let found = err.b.map(|found| {
                            self.engine.info_to_type(found).unwrap_or_else(|err| {
                                self.errors.push(err);
                                Type::Error
                            })
                        });

                        self.errors.push(Error::BinaryOpTypeMismatch {
                            expected,
                            found,
                            op,
                        });
                    });

                let lhs_ty = self.engine.reconstruct(lhs_ty_id).map_or_else(
                    |err| {
                        self.errors.push(err);
                        Type::Error
                    },
                    |ty| ty.0,
                );

                let rhs_ty = self.engine.reconstruct(rhs_ty_id).map_or_else(
                    |err| {
                        self.errors.push(err);
                        Type::Error
                    },
                    |ty| ty.0,
                );

                let final_expr_ty = {
                    use BinaryOp::{Add, Div, Mul, Sub};

                    bin_op_type!(lhs_ty, rhs_ty, op.0,
                        Add | Sub | Mul | Div => Int8, Int8 => Int8,
                        Add | Sub | Mul | Div => Int16, Int16 => Int16,
                        Add | Sub | Mul | Div => Int32, Int32 => Int32,
                        Add | Sub | Mul | Div => Int64, Int64 => Int64,
                        Add | Sub | Mul | Div => Uint8, Uint8 => Uint8,
                        Add | Sub | Mul | Div => Uint16, Uint16 => Uint16,
                        Add | Sub | Mul | Div => Uint32, Uint32 => Uint32,
                        Add | Sub | Mul | Div => Uint64, Uint64 => Uint64,
                        Add | Sub | Mul | Div => Float32, Float32 => Float32,
                        Add | Sub | Mul | Div => Float64, Float64 => Float64,
                    )
                };

                TypedExpression {
                    ty: final_expr_ty,
                    expr: Expression::BinaryOp {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                }
            }
            ast::Expression::UnaryOp { op, expr } => {
                let expr = expr.map(|expr| self.typecheck_expression(*expr));

                TypedExpression {
                    ty: expr.0.ty,
                    expr: Expression::UnaryOp {
                        op,
                        expr: expr.boxed(),
                    },
                }
            }
            ast::Expression::Call { name, args } => {
                let args = args.map(|args| {
                    args.into_iter()
                        .map(|arg| arg.map(|arg| self.typecheck_expression(arg)))
                        .collect::<Vec<_>>()
                });

                let ty = if let Some(sig) = self.get_function_signature(name.0.resolve()) {
                    for (arg_expr, param_ty) in args.0.clone().into_iter().zip(sig.params) {
                        let arg_expr_ty_id = self
                            .engine
                            .insert_type(arg_expr.map(|arg_expr| arg_expr.ty));

                        let param_ty_id = self.engine.insert_type(param_ty);

                        self.engine
                            .unify(arg_expr_ty_id, param_ty_id)
                            .unwrap_or_else(|err| {
                                let expected = err.b;
                                let found = err.a;

                                self.errors.push(Error::TypeMismatch { expected, found });
                            });
                    }

                    sig.return_ty.0
                } else {
                    self.errors.push(Error::UndefinedFunction {
                        name: name.0.resolve().to_string(),
                        span: name.1,
                    });

                    Type::Error
                };

                TypedExpression {
                    ty,
                    expr: Expression::Call { name, args },
                }
            }
        }
    }

    fn get_function_signature(&self, name: &'static str) -> Option<FunctionSignature> {
        self.function_signatures.get(&name).cloned()
    }

    fn lower_function_param(&mut self, param: ast::FunctionParam) -> FunctionParam {
        FunctionParam {
            name: param.name,
            ty: param
                .ty
                .as_ref()
                .map_with(|ty, ty_span| self.lower_type_error(ty, ty_span)),
        }
    }

    fn lower_type_error(&mut self, ty: &ast::Type, span: Span) -> Type {
        self.types
            .get(&ty.0 .0.resolve())
            .copied()
            .unwrap_or_else(|| {
                self.errors.push(Error::UndefinedType {
                    name: ty.0 .0.resolve().to_string(),
                    span,
                });

                Type::Error
            })
    }

    fn add_primitive_types(&mut self) {
        for (name, ty) in [
            ("int8", PrimitiveType::Int8),
            ("int16", PrimitiveType::Int16),
            ("int32", PrimitiveType::Int32),
            ("int64", PrimitiveType::Int64),
            ("uint8", PrimitiveType::Uint8),
            ("uint16", PrimitiveType::Uint16),
            ("uint32", PrimitiveType::Uint32),
            ("uint64", PrimitiveType::Uint64),
            ("float32", PrimitiveType::Float32),
            ("float64", PrimitiveType::Float64),
            ("bool", PrimitiveType::Bool),
            ("unit", PrimitiveType::Unit),
        ] {
            self.types.insert(name, Type::Primitive(ty));
        }
    }
}

#[derive(Clone, Debug)]
struct FunctionSignature {
    params: Vec<Spanned<Type>>,
    return_ty: Spanned<Type>,
}

macro_rules! bin_op_type {
    (
        $lhs_ty:expr, $rhs_ty:expr, $op:expr,
        $($op_pat:pat => $lhs_ty_pat:ident, $rhs_ty_pat:ident => $result_ty:ident),+ $(,)?
    ) => {
        match ($lhs_ty, $rhs_ty, $op) {
            $(
                (
                    Type::Primitive(PrimitiveType::$lhs_ty_pat),
                    Type::Primitive(PrimitiveType::$rhs_ty_pat),
                    $op_pat
                ) => Type::Primitive(PrimitiveType::$result_ty),
            )+
            _ => Type::Error,
        }
    };
}
use bin_op_type;
