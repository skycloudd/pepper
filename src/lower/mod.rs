use crate::{
    lexer::tokens::Identifier,
    parser::ast::{self, BinaryOp},
    scopes::Scopes,
    typecheck::typed_ast::{self, TypedAst},
};
use mir::{
    Expression, FuncParam, Function, Intrinsic, MatchArm, Mir, Name, Pattern, PatternType,
    Primitive, Type, TypedExpression,
};

pub mod mir;

pub fn lower(ast: TypedAst) -> Mir {
    Lower::default().lower(ast)
}

#[derive(Default)]
struct Lower {
    variables: Scopes<&'static str, Name>,
    counter: u32,
}

impl Lower {
    fn lower(&mut self, ast: TypedAst) -> Mir {
        let functions = ast
            .0
            .into_iter()
            .map(|toplevel| match toplevel.0 {
                typed_ast::TopLevel::Function(spanned) => spanned.0,
            })
            .collect::<Vec<_>>();

        for function in &functions {
            self.insert_variable(&function.name);
        }

        Mir {
            functions: functions
                .into_iter()
                .map(|function| self.lower_function(function))
                .collect(),
        }
    }

    fn lower_function(&mut self, function: typed_ast::Function) -> Function {
        self.variables.push_scope();

        let name = self.get_variable(&function.name);

        let params = function
            .params
            .0
            .into_iter()
            .map(|parameter| FuncParam {
                name: self.insert_variable(&parameter.name),
                ty: Self::lower_type(parameter.0.ty.0),
            })
            .collect();

        let return_ty = Self::lower_type(function.return_ty.0);

        let body = self.lower_expression(function.body.0);

        self.variables.pop_scope();

        Function {
            name,
            params,
            return_ty,
            body,
        }
    }

    fn lower_expression(&mut self, expr: typed_ast::TypedExpression) -> TypedExpression {
        TypedExpression {
            expr: match expr.expr {
                typed_ast::Expression::Int(n) => Expression::Int(n),
                typed_ast::Expression::Float(n) => Expression::Float(n),
                typed_ast::Expression::Bool(b) => Expression::Bool(b),
                typed_ast::Expression::Variable(identifier) => {
                    Expression::Variable(self.get_variable(&identifier))
                }
                typed_ast::Expression::BinaryOp { op, lhs, rhs } => {
                    let lhs = self.lower_expression(*lhs.0);
                    let rhs = self.lower_expression(*rhs.0);

                    Expression::Intrinsic(Box::new(bin_op_intrinsics! {
                        op.0, &lhs.ty, &rhs.ty,
                        Add, Int => Intrinsic::add_ints(lhs, rhs),
                        Sub, Int => Intrinsic::sub_ints(lhs, rhs),
                        Mul, Int => Intrinsic::mul_ints(lhs, rhs),
                        Div, Int => Intrinsic::div_ints(lhs, rhs),
                        LessEquals, Int => Intrinsic::lte_ints(lhs, rhs),
                        GreaterEquals, Int => Intrinsic::gte_ints(lhs, rhs),
                        Less, Int => Intrinsic::lt_ints(lhs, rhs),
                        Greater, Int => Intrinsic::gt_ints(lhs, rhs),
                        Equals, Int => Intrinsic::eq_ints(lhs, rhs),
                        NotEquals, Int => Intrinsic::neq_ints(lhs, rhs),
                        Add, Float => Intrinsic::add_floats(lhs, rhs),
                        Sub, Float => Intrinsic::sub_floats(lhs, rhs),
                        Mul, Float => Intrinsic::mul_floats(lhs, rhs),
                        Div, Float => Intrinsic::div_floats(lhs, rhs),
                        LessEquals, Float => Intrinsic::lte_floats(lhs, rhs),
                        GreaterEquals, Float => Intrinsic::gte_floats(lhs, rhs),
                        Less, Float => Intrinsic::lt_floats(lhs, rhs),
                        Greater, Float => Intrinsic::gt_floats(lhs, rhs),
                        Equals, Float => Intrinsic::eq_floats(lhs, rhs),
                        NotEquals, Float => Intrinsic::neq_floats(lhs, rhs),
                        Equals, Bool => Intrinsic::eq_bools(lhs, rhs),
                        NotEquals, Bool => Intrinsic::neq_bools(lhs, rhs),
                    }))
                }
                typed_ast::Expression::UnaryOp { op, expr } => {
                    let expr = self.lower_expression(*expr.0);

                    Expression::Intrinsic(Box::new(match (&expr.ty, op.0) {
                        (Type::Primitive(Primitive::Int), ast::UnaryOp::Neg) => {
                            Intrinsic::neg_int(expr)
                        }
                        (Type::Primitive(Primitive::Float), ast::UnaryOp::Neg) => {
                            Intrinsic::neg_float(expr)
                        }
                        (Type::Primitive(Primitive::Bool), ast::UnaryOp::Not) => {
                            Intrinsic::not_bool(expr)
                        }

                        _ => unreachable!(),
                    }))
                }
                typed_ast::Expression::Call { callee, args } => Expression::Call {
                    callee: Box::new(self.lower_expression(*callee.0)),
                    args: args
                        .0
                        .into_iter()
                        .map(|arg| self.lower_expression(arg.0))
                        .collect(),
                },
                typed_ast::Expression::Match { expr, arms } => Expression::Match {
                    expr: Box::new(self.lower_expression(*expr.0)),
                    arms: arms
                        .0
                        .into_iter()
                        .map(|arm| self.lower_match_arm(arm.0))
                        .collect(),
                },
            },
            ty: Self::lower_type(expr.ty),
        }
    }

    fn lower_match_arm(&mut self, arm: typed_ast::MatchArm) -> MatchArm {
        self.variables.push_scope();

        let pattern_type = match arm.pattern.pattern_type.0 {
            typed_ast::PatternType::Wildcard => PatternType::Wildcard,
            typed_ast::PatternType::Variable(identifier) => {
                PatternType::Variable(self.insert_variable(&identifier))
            }
            typed_ast::PatternType::Int(n) => PatternType::Int(n),
            typed_ast::PatternType::Float(n) => PatternType::Float(n),
            typed_ast::PatternType::Bool(b) => PatternType::Bool(b),
        };

        let condition = arm
            .pattern
            .0
            .condition
            .map(|condition| self.lower_expression(condition.0));

        let pattern = Pattern {
            pattern_type,
            condition,
        };

        let body = self.lower_expression(arm.body.0);

        self.variables.pop_scope();

        MatchArm { pattern, body }
    }

    fn lower_type(ty: ast::Type<typed_ast::Primitive>) -> Type<Primitive> {
        match ty {
            ast::Type::Error => unreachable!(),
            ast::Type::Primitive(p) => Type::Primitive(Self::lower_primitive(p)),
            ast::Type::Tuple(inner) => {
                Type::Tuple(inner.into_iter().map(|ty| Self::lower_type(ty.0)).collect())
            }
            ast::Type::Never => Type::Never,
            ast::Type::Function { params, return_ty } => Type::Function {
                params: params
                    .0
                    .into_iter()
                    .map(|ty| Self::lower_type(ty.0))
                    .collect(),
                return_ty: Box::new(Self::lower_type(*return_ty.0)),
            },
        }
    }

    const fn lower_primitive(primitive: typed_ast::Primitive) -> Primitive {
        match primitive {
            typed_ast::Primitive::Int => Primitive::Int,
            typed_ast::Primitive::Float => Primitive::Float,
            typed_ast::Primitive::Bool => Primitive::Bool,
        }
    }

    fn get_variable(&self, ident: &Identifier) -> Name {
        self.variables
            .get(&ident.resolve())
            .copied()
            .unwrap_or_else(|| panic!("name not found: {ident:?}"))
    }

    fn insert_variable(&mut self, ident: &Identifier) -> Name {
        let name = self.new_name();
        self.variables.insert(ident.resolve(), name);
        name
    }

    fn new_name(&mut self) -> Name {
        let counter = self.counter;
        self.counter = self.counter.checked_add(1).unwrap();
        Name::new(counter)
    }
}

macro_rules! bin_op_intrinsics {
    (
        $op:expr, $lhs_ty:expr, $rhs_ty:expr,
        $($op_name:ident, $ty_name:ident => $intrinsic:expr,)* $(,)?
    ) => {
        match ($op, $lhs_ty, $rhs_ty) {
            $(
                (BinaryOp::$op_name, Type::Primitive(Primitive::$ty_name), Type::Primitive(Primitive::$ty_name)) => $intrinsic,
            )*
            _ => unreachable!(),
        }
    };
}
use bin_op_intrinsics;
