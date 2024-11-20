use crate::{
    lexer::tokens::Identifier,
    parser::ast::{BinaryOp, Type, UnaryOp},
    span::Spanned,
};

#[derive(Clone, Debug)]
pub struct TypedAst<'src>(pub Vec<Spanned<TopLevel<'src>>>);

#[derive(Clone, Debug)]
pub enum TopLevel<'src> {
    Function(Spanned<Function<'src>>),
    Extern(Spanned<Extern>),
}

#[derive(Clone, Debug)]
pub struct Function<'src> {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Primitive>>,
    pub body: Spanned<TypedExpression<'src>>,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type<Primitive>>,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type<Primitive>>,
}

#[derive(Clone, Debug)]
pub struct TypedExpression<'src> {
    pub expr: Expression<'src>,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
pub enum Expression<'src> {
    Int(&'src str),
    Float(&'src str),
    Bool(&'src str),
    Variable(Identifier),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<TypedExpression<'src>>>,
        rhs: Spanned<Box<TypedExpression<'src>>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<TypedExpression<'src>>>,
    },
    Call {
        callee: Spanned<Box<TypedExpression<'src>>>,
        args: Spanned<Vec<Spanned<TypedExpression<'src>>>>,
    },
    Match {
        expr: Spanned<Box<TypedExpression<'src>>>,
        arms: Spanned<Vec<Spanned<MatchArm<'src>>>>,
    },
}

#[derive(Clone, Debug)]
pub struct MatchArm<'src> {
    pub pattern: Spanned<Pattern<'src>>,
    pub body: Spanned<TypedExpression<'src>>,
}

#[derive(Clone, Debug)]
pub struct Pattern<'src> {
    pub pattern_type: Spanned<PatternType<'src>>,
    pub condition: Option<Spanned<TypedExpression<'src>>>,
}

#[derive(Clone, Debug)]
pub enum PatternType<'src> {
    Wildcard,
    Variable(Identifier),
    Int(&'src str),
    Float(&'src str),
    Bool(&'src str),
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
