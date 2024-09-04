use crate::{span::Spanned, RODEO};
use lasso::Spur;
use ordered_float::OrderedFloat;

#[derive(Clone, Debug)]
pub struct Ast {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Option<Spanned<Type>>,
    pub body: Spanned<Vec<Spanned<Statement>>>,
    pub return_expr: Option<Spanned<Expression>>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    Expression(Spanned<Expression>),
    Block(Spanned<Vec<Spanned<Statement>>>),
    Let {
        name: Spanned<Identifier>,
        ty: Option<Spanned<Type>>,
        value: Spanned<Expression>,
    },
    Assign {
        name: Spanned<Identifier>,
        value: Spanned<Expression>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    Integer(Spanned<u128>),
    Float(Spanned<OrderedFloat<f64>>),
    Bool(Spanned<bool>),
    Variable(Spanned<Identifier>),
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Type(pub Spanned<Identifier>);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Identifier(pub Spanned<Spur>);

impl Identifier {
    pub fn resolve(&self) -> &'static str {
        RODEO.resolve(&self.0 .0)
    }
}
