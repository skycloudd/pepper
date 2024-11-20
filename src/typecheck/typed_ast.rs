use crate::{
    lexer::tokens::Interned,
    parser::ast::{BinaryOp, Type, UnaryOp},
    span::Spanned,
};

#[derive(Clone, Debug)]
pub struct TypedAst(pub Vec<Spanned<TopLevel>>);

#[derive(Clone, Debug)]
pub enum TopLevel {
    Module(Interned, TypedAst),
    Function(Spanned<Function>),
    Extern(Spanned<Extern>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Spanned<Interned>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Primitive>>,
    pub body: Spanned<TypedExpression>,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub name: Spanned<Interned>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Primitive>>,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub name: Spanned<Interned>,
    pub ty: Spanned<Type<Primitive>>,
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Int(Interned),
    Float(Interned),
    Bool(Interned),
    Variable(Interned),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<TypedExpression>>,
        rhs: Spanned<Box<TypedExpression>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<TypedExpression>>,
    },
    Call {
        callee: Spanned<Box<TypedExpression>>,
        args: Spanned<Vec<Spanned<TypedExpression>>>,
    },
    Match {
        expr: Spanned<Box<TypedExpression>>,
        arms: Spanned<Vec<Spanned<MatchArm>>>,
    },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<TypedExpression>,
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub pattern_type: Spanned<PatternType>,
    pub condition: Option<Spanned<TypedExpression>>,
}

#[derive(Clone, Debug)]
pub enum PatternType {
    Wildcard,
    Variable(Interned),
    Int(Interned),
    Float(Interned),
    Bool(Interned),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    Int,
    Float,
    Bool,
}

impl core::fmt::Display for Primitive {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
        }
    }
}
