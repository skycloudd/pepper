use crate::{span::Span, RODEO};
use lasso::Spur;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Simple(SimpleToken<'src>),
    Parentheses(Vec<Spanned<Self>>),
    CurlyBraces(Vec<Spanned<Self>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SimpleToken<'src> {
    Identifier(Identifier),
    Int(&'src str),
    Float(&'src str),
    Boolean(&'src str),
    Kw(Kw),
    Punc(Punc),
    Wildcard,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kw {
    Func,
    Extern,
    Match,
    Where,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Punc {
    Arrow,
    DoubleArrow,
    Plus,
    Minus,
    Star,
    Slash,
    Colon,
    Comma,
    DoubleEquals,
    NotEquals,
    Equals,
    Bang,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    Pipe,
    Semicolon,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Identifier(Spur, pub Span);

impl Identifier {
    pub const fn new(name: Spur, span: Span) -> Self {
        Self(name, span)
    }

    pub fn resolve(self) -> &'static str {
        RODEO.resolve(&self.0)
    }
}

impl core::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Identifier")
            .field(&self.resolve())
            .field(&self.1)
            .finish()
    }
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
            Self::Identifier(name) => write!(f, "{}", name.resolve()),
            Self::Int(num) | Self::Float(num) => write!(f, "{num}"),
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Kw(kw) => write!(f, "{kw}"),
            Self::Punc(punc) => write!(f, "{punc}"),
            Self::Wildcard => write!(f, "_"),
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
                Self::Extern => "extern",
                Self::Match => "match",
                Self::Where => "where",
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
                Self::DoubleArrow => "=>",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Star => "*",
                Self::Slash => "/",
                Self::Colon => ":",
                Self::Comma => ",",
                Self::DoubleEquals => "==",
                Self::NotEquals => "!=",
                Self::Equals => "=",
                Self::Bang => "!",
                Self::LessEquals => "<=",
                Self::GreaterEquals => ">=",
                Self::Less => "<",
                Self::Greater => ">",
                Self::Pipe => "|",
                Self::Semicolon => ";",
            }
        )
    }
}
