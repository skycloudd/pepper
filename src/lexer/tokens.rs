use crate::{span::Span, RODEO};
use lasso::Spur;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Simple(SimpleToken),
    Parentheses(Vec<Spanned<Self>>),
    CurlyBraces(Vec<Spanned<Self>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SimpleToken {
    Identifier(Interned),
    Int(Interned),
    Float(Interned),
    Boolean(Interned),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Interned(Spur);

impl Interned {
    pub const fn new(name: Spur) -> Self {
        Self(name)
    }

    pub fn get_or_intern(val: impl AsRef<str>) -> Self {
        Self::new(RODEO.get_or_intern(val))
    }

    pub fn resolve(self) -> &'static str {
        RODEO.resolve(&self.0)
    }
}

impl core::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Simple(simple) => write!(f, "{simple}"),
            Self::Parentheses(_tokens) => write!(f, "(...)"),
            Self::CurlyBraces(_tokens) => write!(f, "{{...}}"),
        }
    }
}

impl core::fmt::Display for SimpleToken {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Identifier(interned)
            | Self::Int(interned)
            | Self::Float(interned)
            | Self::Boolean(interned) => write!(f, "{}", interned.resolve()),
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
