use crate::span::Span;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token<'src> {
    Simple(SimpleToken<'src>),
    Parentheses(Vec<Spanned<Token<'src>>>),
    CurlyBraces(Vec<Spanned<Token<'src>>>),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SimpleToken<'src> {
    Identifier(&'src str),
    Integer {
        value: Spanned<&'src str>,
        ty: Option<Spanned<&'src str>>,
    },
    Float {
        value: Spanned<&'src str>,
        ty: Option<Spanned<&'src str>>,
    },
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Kw {
    Func,
    Struct,
    Let,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Punc {
    Arrow,
    Plus,
    Minus,
    Star,
    Slash,
    Colon,
    Comma,
    Equals,
    Semicolon,
}

impl core::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Simple(simple) => write!(f, "{simple}"),
            Self::Parentheses(_tokens) => write!(f, "(...)"),
            Self::CurlyBraces(_tokens) => write!(f, "{{...}}"),
        }
    }
}

impl core::fmt::Display for SimpleToken<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{ident}"),
            Self::Integer { value, ty } | Self::Float { value, ty } => {
                write!(
                    f,
                    "{}{}",
                    value.0,
                    ty.map_or(String::new(), |ty| format!("_{}", ty.0))
                )
            }
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Kw(kw) => write!(f, "{kw}"),
            Self::Punc(punc) => write!(f, "{punc}"),
        }
    }
}

impl core::fmt::Display for Kw {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Func => "func",
                Self::Struct => "struct",
                Self::Let => "let",
            }
        )
    }
}

impl core::fmt::Display for Punc {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Arrow => "->",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Star => "*",
                Self::Slash => "/",
                Self::Colon => ":",
                Self::Comma => ",",
                Self::Equals => "=",
                Self::Semicolon => ";",
            }
        )
    }
}
