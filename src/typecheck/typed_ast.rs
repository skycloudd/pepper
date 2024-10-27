use crate::{
    lexer::tokens::Identifier,
    parser::ast::{BinaryOp, Type, UnaryOp},
    span::Spanned,
};

#[derive(Clone, Debug)]
pub struct TypedAst(pub Vec<Spanned<TopLevel>>);

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Spanned<Function>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
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
    Number(f64),
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
        callee: Spanned<Box<TypedExpression>>,
        args: Spanned<Vec<Spanned<TypedExpression>>>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    Number,
    Bool,
}
