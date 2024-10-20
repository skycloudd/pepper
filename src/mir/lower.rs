use super::{
    BinaryOp, Expression, Function, FunctionParam, FunctionType, Identifier, Mir, Primitive, Type,
    TypedExpression, UnaryOp,
};
use crate::{
    parser::ast,
    typecheck::typed_ast::{self, TypedAst},
};
use nonempty::NonEmpty;

pub fn lower(typed_ast: TypedAst) -> Mir {
    let mut functions = vec![];
    let mut main = vec![];

    for toplevel in typed_ast.toplevels {
        match toplevel.0 {
            typed_ast::TopLevel::Function(function) => functions.push(lower_function(function.0)),
            typed_ast::TopLevel::Do(expr) => main.push(lower_expression(expr.0)),
        }
    }

    Mir { functions, main }
}

fn lower_function(function: typed_ast::Function) -> Function {
    let name = lower_ident(function.name.0);

    let params = NonEmpty::collect(function.params.0.into_iter().map(|param| {
        let name = lower_ident(param.0.name.0);
        let ty = lower_type(param.0.ty.0);

        FunctionParam { name, ty }
    }))
    .unwrap();

    let return_ty = lower_type(function.return_ty.0);

    let body = lower_expression(function.body.0);

    Function {
        name,
        params,
        return_ty,
        body,
    }
}

const fn lower_ident(ident: ast::Identifier) -> Identifier {
    Identifier(ident.0 .0)
}

fn lower_type(ty: ast::Type<typed_ast::Primitive>) -> Type<Primitive> {
    match ty {
        ast::Type::Primitive(primitive) => Type::Primitive(lower_primitive(primitive)),
        ast::Type::Unit => Type::Unit,
        ast::Type::Never => Type::Never,
        ast::Type::Function(function_type) => Type::Function(lower_function_type(function_type)),
    }
}

const fn lower_primitive(primitive: typed_ast::Primitive) -> Primitive {
    match primitive {
        typed_ast::Primitive::Number => Primitive::Number,
        typed_ast::Primitive::Bool => Primitive::Bool,
    }
}

fn lower_function_type(
    function_type: ast::FunctionType<typed_ast::Primitive>,
) -> FunctionType<Primitive> {
    let params = function_type
        .params
        .0
        .into_iter()
        .map(|param| lower_type(param.0))
        .collect();

    let return_ty = lower_type(*function_type.return_ty.0);

    FunctionType {
        params,
        return_ty: Box::new(return_ty),
    }
}

fn lower_expression(expr: typed_ast::TypedExpression) -> TypedExpression {
    let ty = lower_type(expr.ty);

    TypedExpression {
        ty,
        expr: match expr.expr {
            typed_ast::Expression::Unit => Expression::Unit,
            typed_ast::Expression::Number(value) => Expression::Number(value.0),
            typed_ast::Expression::Bool(value) => Expression::Bool(value.0),
            typed_ast::Expression::Variable(name) => Expression::Variable(lower_ident(name.0)),
            typed_ast::Expression::BinaryOp { op, lhs, rhs } => Expression::BinaryOp {
                op: lower_binary_op(op.0),
                lhs: Box::new(lower_expression(*lhs.0)),
                rhs: Box::new(lower_expression(*rhs.0)),
            },
            typed_ast::Expression::UnaryOp { op, expr } => Expression::UnaryOp {
                op: lower_unary_op(op.0),
                expr: Box::new(lower_expression(*expr.0)),
            },
            typed_ast::Expression::Call { name, args } => Expression::Call {
                name: Box::new(lower_expression(*name.0)),
                args: NonEmpty::collect(
                    args.0
                        .into_iter()
                        .map(|arg| Box::new(lower_expression(*arg.0))),
                )
                .unwrap(),
            },
        },
    }
}

const fn lower_binary_op(op: ast::BinaryOp) -> BinaryOp {
    match op {
        ast::BinaryOp::Add => BinaryOp::Add,
        ast::BinaryOp::Sub => BinaryOp::Sub,
        ast::BinaryOp::Mul => BinaryOp::Mul,
        ast::BinaryOp::Div => BinaryOp::Div,
    }
}

const fn lower_unary_op(op: ast::UnaryOp) -> UnaryOp {
    match op {
        ast::UnaryOp::Neg => UnaryOp::Neg,
    }
}
