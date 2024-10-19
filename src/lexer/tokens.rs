use crate::span::Span;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Simple(SimpleToken<'src>),
    Parentheses(Vec<Spanned<Token<'src>>>),
    CurlyBraces(Vec<Spanned<Token<'src>>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SimpleToken<'src> {
    Identifier(&'src str),
    Number(&'src str, FractionalPart<'src>),
    Boolean(bool),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FractionalPart<'src> {
    None,
    Period,
    Full(&'src str),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kw {
    Let,
    Do,
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
    Hash,
    Bang,
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
            Self::Identifier(name) => write!(f, "{name}"),
            Self::Number(int, frac) => write!(f, "{int}{frac}"),
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Kw(kw) => write!(f, "{kw}"),
            Self::Punc(punc) => write!(f, "{punc}"),
        }
    }
}

impl core::fmt::Display for FractionalPart<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::None => write!(f, ""),
            Self::Period => write!(f, "."),
            Self::Full(full) => write!(f, ".{full}"),
        }
    }
}

impl core::fmt::Display for Kw {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Let => "let",
                Self::Do => "do",
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
                Self::Hash => "#",
                Self::Bang => "!",
            }
        )
    }
}
