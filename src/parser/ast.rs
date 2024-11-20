use crate::{lexer::tokens::Identifier, span::Spanned};

#[derive(Clone, Debug)]
pub struct Ast<'src>(pub Vec<Spanned<TopLevel<'src>>>);

#[derive(Clone, Debug)]
pub enum TopLevel<'src> {
    Function(Spanned<Function<'src>>),
    Extern(Spanned<Extern>),
}

#[derive(Clone, Debug)]
pub struct Function<'src> {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Identifier>>,
    pub body: Spanned<Expression<'src>>,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Identifier>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type<Identifier>>,
}

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Int(&'src str),
    Float(&'src str),
    Bool(&'src str),
    Variable(Identifier),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<Self>>,
        rhs: Spanned<Box<Self>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<Self>>,
    },
    Call {
        callee: Spanned<Box<Self>>,
        args: Spanned<Vec<Spanned<Self>>>,
    },
    Match {
        expr: Spanned<Box<Self>>,
        arms: Spanned<Vec<Spanned<MatchArm<'src>>>>,
    },
}

#[derive(Clone, Debug)]
pub struct MatchArm<'src> {
    pub pattern: Spanned<Pattern<'src>>,
    pub body: Spanned<Expression<'src>>,
}

#[derive(Clone, Debug)]
pub struct Pattern<'src> {
    pub pattern_type: Spanned<PatternType<'src>>,
    pub condition: Option<Spanned<Expression<'src>>>,
}

#[derive(Clone, Debug)]
pub enum PatternType<'src> {
    Wildcard,
    Variable(Identifier),
    Int(&'src str),
    Float(&'src str),
    Bool(&'src str),
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    Equals,
    NotEquals,
}

impl core::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::LessEquals => write!(f, "<="),
            Self::GreaterEquals => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::Equals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl core::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<P> {
    Error,
    Primitive(P),
    Tuple(Vec<Spanned<Type<P>>>),
    Never,
    Function {
        params: Spanned<Vec<Spanned<Type<P>>>>,
        return_ty: Spanned<Box<Type<P>>>,
    },
}

impl<P: core::fmt::Display> core::fmt::Display for Type<P> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Error => write!(f, "<error>"),
            Self::Primitive(primitive) => write!(f, "{primitive}"),
            Self::Tuple(inner) => {
                write!(f, "(")?;
                for (i, ty) in inner.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty.0)?;
                }
                write!(f, ")")
            }
            Self::Never => write!(f, "!"),
            Self::Function { params, return_ty } => {
                write!(f, "func (")?;
                for (i, param) in params.0.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.0)?;
                }
                write!(f, ") -> {}", return_ty.0)
            }
        }
    }
}
