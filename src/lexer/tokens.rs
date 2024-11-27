use crate::{span::Span, RODEO};
use lasso::Spur;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenTree {
    Token(Token),
    Tree(Delim, Vec<Spanned<Self>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(Interned),
    Int(Interned),
    Float(Interned),
    Boolean(Interned),
    String(Interned),
    Kw(Kw),
    Punc(Punc),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kw {
    Func,
    Var,
    For,
    In,
    Match,
    Where,
    Import,
    Struct,
    Enum,
    Module,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Punc {
    Arrow,
    DoubleArrow,
    DoubleEquals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    DoublePeriod,
    Plus,
    Minus,
    Star,
    Slash,
    Colon,
    Comma,
    Period,
    Equals,
    Bang,
    Less,
    Greater,
    Semicolon,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Interned(Spur);

impl Interned {
    #[must_use]
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

impl core::fmt::Debug for Interned {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Interned").field(&self.resolve()).finish()
    }
}

#[cfg(test)]
impl serde::Serialize for Interned {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.resolve().serialize(serializer)
    }
}

impl core::fmt::Display for TokenTree {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{token}"),
            Self::Tree(delim, _tokens) => write!(f, "{delim}"),
        }
    }
}

impl core::fmt::Display for Delim {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Paren => write!(f, "(...)"),
            Self::Brace => write!(f, "{{...}}"),
            Self::Bracket => write!(f, "[...]"),
        }
    }
}

impl core::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Identifier(interned)
            | Self::Int(interned)
            | Self::Float(interned)
            | Self::Boolean(interned)
            | Self::String(interned) => write!(f, "{}", interned.resolve()),
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
                Self::Var => "var",
                Self::For => "for",
                Self::In => "in",
                Self::Match => "match",
                Self::Where => "where",
                Self::Import => "import",
                Self::Struct => "struct",
                Self::Enum => "enum",
                Self::Module => "module",
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
                Self::DoubleEquals => "==",
                Self::NotEquals => "!=",
                Self::LessEquals => "<=",
                Self::GreaterEquals => ">=",
                Self::DoublePeriod => "..",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Star => "*",
                Self::Slash => "/",
                Self::Colon => ":",
                Self::Comma => ",",
                Self::Period => ".",
                Self::Equals => "=",
                Self::Bang => "!",
                Self::Less => "<",
                Self::Greater => ">",
                Self::Semicolon => ";",
            }
        )
    }
}
