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
    names: Scopes<&'static str, Name>,
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
            self.insert_name(&function.name);
        }

        Mir {
            functions: functions
                .into_iter()
                .map(|function| self.lower_function(function))
                .collect(),
        }
    }

    fn lower_function(&mut self, function: typed_ast::Function) -> Function {
        self.names.push_scope();

        let name = self.get_name(&function.name);

        let params = function
            .params
            .0
            .into_iter()
            .map(|parameter| FuncParam {
                name: self.insert_name(&parameter.name),
                ty: Self::lower_type(parameter.0.ty.0),
            })
            .collect();

        let return_ty = Self::lower_type(function.return_ty.0);

        let body = self.lower_expression(function.body.0);

        self.names.pop_scope();

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
                typed_ast::Expression::Number(n) => Expression::Number(n),
                typed_ast::Expression::Bool(b) => Expression::Bool(b),
                typed_ast::Expression::Variable(identifier) => {
                    Expression::Variable(self.get_name(&identifier))
                }
                typed_ast::Expression::BinaryOp { op, lhs, rhs } => {
                    let lhs = self.lower_expression(*lhs.0);
                    let rhs = self.lower_expression(*rhs.0);

                    Expression::Intrinsic(Box::new(bin_op_intrinsics! {
                        op.0, &lhs.ty, &rhs.ty,
                        Add, Number => Intrinsic::add_numbers(lhs, rhs),
                        Sub, Number => Intrinsic::sub_numbers(lhs, rhs),
                        Mul, Number => Intrinsic::mul_numbers(lhs, rhs),
                        Div, Number => Intrinsic::div_numbers(lhs, rhs),
                        LessEquals, Number => Intrinsic::lte_numbers(lhs, rhs),
                        GreaterEquals, Number => Intrinsic::gte_numbers(lhs, rhs),
                        Less, Number => Intrinsic::lt_numbers(lhs, rhs),
                        Greater, Number => Intrinsic::gt_numbers(lhs, rhs),
                        Equals, Number => Intrinsic::eq_numbers(lhs, rhs),
                        NotEquals, Number => Intrinsic::neq_numbers(lhs, rhs),
                        Equals, Bool => Intrinsic::eq_bools(lhs, rhs),
                        NotEquals, Bool => Intrinsic::neq_bools(lhs, rhs),
                    }))
                }
                typed_ast::Expression::UnaryOp { op, expr } => {
                    let expr = self.lower_expression(*expr.0);

                    Expression::Intrinsic(Box::new(match (&expr.ty, op.0) {
                        (Type::Primitive(Primitive::Number), ast::UnaryOp::Neg) => {
                            Intrinsic::neg_number(expr)
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
        self.names.push_scope();

        let pattern_type = match arm.pattern.pattern_type.0 {
            typed_ast::PatternType::Wildcard => PatternType::Wildcard,
            typed_ast::PatternType::Variable(identifier) => {
                PatternType::Variable(self.insert_name(&identifier))
            }
            typed_ast::PatternType::Number(n) => PatternType::Number(n),
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

        self.names.pop_scope();

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
            typed_ast::Primitive::Number => Primitive::Number,
            typed_ast::Primitive::Bool => Primitive::Bool,
        }
    }

    fn get_name(&self, ident: &Identifier) -> Name {
        self.names
            .get(&ident.resolve())
            .copied()
            .unwrap_or_else(|| panic!("name not found: {ident:?}"))
    }

    fn insert_name(&mut self, ident: &Identifier) -> Name {
        let counter = self.counter;
        self.counter = self.counter.checked_add(1).unwrap();
        self.names.insert(ident.resolve(), Name::new(counter));
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
