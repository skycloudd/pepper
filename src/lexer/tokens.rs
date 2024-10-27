use crate::{span::Span, RODEO};
use lasso::Spur;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Simple(SimpleToken),
    Parentheses(Vec<Spanned<Self>>),
    CurlyBraces(Vec<Spanned<Self>>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SimpleToken {
    Identifier(Identifier),
    Number(f64),
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kw {
    Func,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Punc {
    Arrow,
    Plus,
    Minus,
    Star,
    Slash,
    Colon,
    Comma,
    Equals,
    Bang,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Identifier(Spur, pub Span);

impl Identifier {
    pub const fn new(name: Spur, span: Span) -> Self {
        Self(name, span)
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
            Self::Identifier(name) => write!(f, "{}", name.resolve()),
            Self::Number(num) => write!(f, "{num}"),
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
                Self::Bang => "!",
            }
        )
    }
}
