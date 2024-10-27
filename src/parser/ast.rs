use crate::{lexer::tokens::Identifier, span::Spanned};

#[derive(Clone, Debug)]
pub struct Ast(pub Vec<Spanned<TopLevel>>);

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Spanned<Function>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Identifier>>,
    pub body: Spanned<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type<Identifier>>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Number(f64),
    Bool(bool),
    Variable(Identifier),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<Expression>>,
        rhs: Spanned<Box<Expression>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<Expression>>,
    },
    Call {
        callee: Spanned<Box<Expression>>,
        args: Spanned<Vec<Spanned<Expression>>>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<P> {
    Primitive(P),
    Tuple(Vec<Spanned<Type<P>>>),
    Never,
    Function {
        params: Spanned<Vec<Spanned<Type<P>>>>,
        return_ty: Spanned<Box<Type<P>>>,
    },
}
