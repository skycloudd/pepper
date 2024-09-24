use crate::{span::Spanned, RODEO};
use lasso::Spur;
use malachite::Rational;

#[derive(Clone, Debug)]
pub struct Ast {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Option<Spanned<Type<Identifier>>>,
    pub body: Spanned<Expression>,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type<Identifier>>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Unit,
    Number(Rational),
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
        name: Spanned<Identifier>,
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

impl core::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
}

impl core::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type<P> {
    Primitive(P),
    Unit,
    Never,
    Function(FunctionType<P>),
}

#[derive(Clone, Debug)]
pub struct FunctionType<P> {
    pub params: Spanned<Vec<Spanned<Type<P>>>>,
    pub return_ty: Option<Spanned<Box<Type<P>>>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Identifier(pub Spur);

impl Identifier {
    pub fn resolve(self) -> &'static str {
        RODEO.resolve(&self.0)
    }
}
