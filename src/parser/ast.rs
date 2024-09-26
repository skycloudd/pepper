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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<P> {
    Primitive(P),
    Unit,
    Never,
    Function(FunctionType<P>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionType<P> {
    pub params: Vec<Type<P>>,
    pub return_ty: Box<Type<P>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Identifier(pub Spanned<Spur>);

impl Identifier {
    pub fn resolve(self) -> &'static str {
        RODEO.resolve(&self.0 .0)
    }
}

impl<P: core::fmt::Display> core::fmt::Display for Type<P> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Primitive(p) => write!(f, "{p}"),
            Self::Unit => write!(f, "#"),
            Self::Never => write!(f, "!"),
            Self::Function(function) => {
                write!(f, "(")?;

                for (i, param) in function.params.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{param}")?;
                }

                write!(f, ")")?;

                write!(f, " -> {}", function.return_ty)?;

                Ok(())
            }
        }
    }
}
