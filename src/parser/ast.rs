use crate::{lexer::tokens::Interned, span::Spanned};

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Interned,
    pub ast: Ast,
    pub children: Vec<Self>,
}

#[derive(Clone, Debug)]
pub struct Ast(pub Vec<Spanned<TopLevel>>);

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Spanned<Function>),
    Extern(Spanned<Extern>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Interned>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Spanned<Interned>>>,
    pub body: Spanned<Expression>,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub name: Spanned<Interned>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Spanned<Interned>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: Spanned<Interned>,
    pub ty: Spanned<Type<Spanned<Interned>>>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Int(Interned),
    Float(Interned),
    Bool(Interned),
    Variable(Spanned<Interned>),
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
        arms: Spanned<Vec<Spanned<MatchArm>>>,
    },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<Expression>,
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub pattern_type: Spanned<PatternType>,
    pub condition: Option<Spanned<Expression>>,
}

#[derive(Clone, Debug)]
pub enum PatternType {
    Wildcard,
    Variable(Interned),
    Int(Interned),
    Float(Interned),
    Bool(Interned),
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
