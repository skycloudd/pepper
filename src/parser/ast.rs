use crate::{lexer::tokens::Interned, span::Spanned};

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Ast(pub Vec<Spanned<Item>>);

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Item {
    Function(Spanned<Function>),
    Import(Spanned<Path>),
    Struct(Spanned<Struct>),
    Enum(Spanned<Enum>),
    Module(Spanned<Module>),
}

#[allow(clippy::module_name_repetitions)]
pub type AstType = Type<Path>;

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Path {
    pub base: Spanned<Interned>,
    pub segments: Vec<Spanned<Interned>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Module {
    pub name: Spanned<Interned>,
    pub ast: Option<Ast>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub name: Spanned<Interned>,
    pub generics: Option<Spanned<Vec<Spanned<Interned>>>>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Option<Spanned<AstType>>,
    pub body: Spanned<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionParam {
    pub name: Spanned<Interned>,
    pub ty: Spanned<AstType>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Struct {
    pub name: Spanned<Interned>,
    pub generics: Option<Spanned<Vec<Spanned<Interned>>>>,
    pub fields: Spanned<Vec<Spanned<StructField>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructField {
    pub name: Spanned<Interned>,
    pub ty: Spanned<AstType>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Enum {
    pub name: Spanned<Interned>,
    pub generics: Option<Spanned<Vec<Spanned<Interned>>>>,
    pub variants: Spanned<Vec<Spanned<EnumVariant>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum EnumVariant {
    Unit(Spanned<Interned>),
    Tuple {
        name: Spanned<Interned>,
        fields: Spanned<Vec<Spanned<AstType>>>,
    },
    Struct {
        name: Spanned<Interned>,
        fields: Spanned<Vec<Spanned<StructField>>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Expression {
    Int(Spanned<Interned>),
    Float(Spanned<Interned>),
    Bool(Spanned<Interned>),
    String(Spanned<Interned>),
    Name(Spanned<Interned>),
    Tuple(Spanned<Vec<Spanned<Self>>>),
    List(Spanned<Vec<Spanned<Self>>>),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<Self>>,
        rhs: Spanned<Box<Self>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<Self>>,
    },
    Dot {
        expr: Spanned<Box<Self>>,
        field: Spanned<Interned>,
    },
    Call {
        callee: Spanned<Box<Self>>,
        args: Spanned<Vec<Spanned<Self>>>,
    },
    Match {
        expr: Spanned<Box<Self>>,
        arms: Spanned<Vec<Spanned<MatchArm>>>,
    },
    Binding {
        name: Spanned<Interned>,
        ty: Option<Spanned<AstType>>,
        value: Spanned<Box<Self>>,
        body: Spanned<Box<Self>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Pattern {
    pub pattern_type: Spanned<PatternType>,
    pub condition: Option<Spanned<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum PatternType {
    Name(Spanned<Path>),
    Int(Spanned<Interned>),
    Float(Spanned<Interned>),
    Bool(Spanned<Interned>),
    String(Spanned<Interned>),
    Tuple(Spanned<Vec<Spanned<Pattern>>>),
    TupleType {
        name: Spanned<Path>,
        fields: Spanned<Vec<Spanned<Pattern>>>,
    },
    StructType {
        name: Spanned<Path>,
        fields: Spanned<Vec<Spanned<StructPatternField>>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructPatternField {
    pub name: Spanned<Interned>,
    pub pattern: Option<Spanned<Pattern>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

impl core::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Pos => write!(f, "+"),
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Type<P> {
    Primitive {
        name: Spanned<P>,
        generics: Option<Spanned<Vec<Spanned<Self>>>>,
    },
    Tuple(Spanned<Vec<Spanned<Self>>>),
    Never,
    Function {
        params: Spanned<Vec<Spanned<Self>>>,
        return_ty: Spanned<Box<Self>>,
    },
}

impl<P: core::fmt::Display> core::fmt::Display for Type<P> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Primitive { name, generics } => {
                write!(f, "{}", name.0)?;

                if let Some(generics) = generics {
                    write!(f, "[")?;
                    for (i, ty) in generics.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", ty.0)?;
                    }
                    write!(f, "]")?;
                }

                Ok(())
            }
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
                for (i, param) in params.iter().enumerate() {
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
