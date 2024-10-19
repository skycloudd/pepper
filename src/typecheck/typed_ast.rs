use crate::{
    parser::ast::{BinaryOp, Identifier, Type, UnaryOp},
    span::Spanned,
};
use malachite::Rational;
use nonempty::NonEmpty;

#[derive(Clone, Debug)]
pub struct TypedAst {
    pub toplevels: Vec<Spanned<TopLevel>>,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Spanned<Function>),
    Do(Spanned<TypedExpression>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<NonEmpty<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Primitive>>,
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
    Number(Spanned<Rational>),
    Bool(Spanned<bool>),
    Variable(Spanned<Identifier>),
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
        args: Spanned<NonEmpty<Spanned<Box<TypedExpression>>>>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    Number,
    Bool,
}

impl core::fmt::Display for Primitive {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Number => write!(f, "number"),
            Self::Bool => write!(f, "bool"),
        }
    }
}
