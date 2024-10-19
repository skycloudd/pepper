use crate::{span::Spanned, RODEO};
use lasso::Spur;
use malachite::Rational;
use nonempty::NonEmpty;

#[derive(Clone, Debug)]
pub struct Ast {
    pub toplevels: Vec<Spanned<TopLevel>>,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Spanned<Function>),
    Do(Spanned<Expression>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<NonEmpty<Spanned<FunctionParam>>>,
    pub return_ty: Option<Spanned<Type<Identifier>>>,
    pub body: Spanned<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type<Identifier>>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Unit,
    Number(Spanned<Rational>),
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
        args: Spanned<NonEmpty<Spanned<Box<Expression>>>>,
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
    pub params: Spanned<Vec<Spanned<Type<P>>>>,
    pub return_ty: Spanned<Box<Type<P>>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

                for (i, param) in function.params.0.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.0)?;
                }

                write!(f, ")")?;

                write!(f, " -> {}", function.return_ty.0)?;

                Ok(())
            }
        }
    }
}
