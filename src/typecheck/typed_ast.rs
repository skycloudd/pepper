use crate::{
    parser::ast::{BinaryOp, Identifier, Type, UnaryOp},
    span::Spanned,
};
use malachite::Rational;

#[derive(Clone, Debug)]
pub struct TypedAst {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Option<Spanned<Type<Primitive>>>,
    pub body: Spanned<TypedExpression>,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type<Primitive>>,
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Unit,
    Number(Rational),
    Bool(bool),
    Variable(Identifier),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<TypedExpression>>,
        rhs: Spanned<Box<TypedExpression>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<TypedExpression>>,
    },
    Call {
        name: Spanned<Identifier>,
        args: Spanned<Vec<Spanned<TypedExpression>>>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum Primitive {
    Number,
    Bool,
}
