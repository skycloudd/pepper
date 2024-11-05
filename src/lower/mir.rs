use crate::parser::ast::{BinaryOp, UnaryOp};

#[derive(Clone, Debug)]
pub struct Mir {
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Name,
    pub params: Vec<FuncParam>,
    pub return_ty: Type<Primitive>,
    pub body: TypedExpression,
}

#[derive(Clone, Debug)]
pub struct FuncParam {
    pub name: Name,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Number(f64),
    Bool(bool),
    Variable(Name),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<TypedExpression>,
        rhs: Box<TypedExpression>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<TypedExpression>,
    },
    Call {
        callee: Box<TypedExpression>,
        args: Vec<TypedExpression>,
    },
    Match {
        expr: Box<TypedExpression>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: TypedExpression,
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub pattern_type: PatternType,
    pub condition: Option<TypedExpression>,
}

#[derive(Clone, Debug)]
pub enum PatternType {
    Wildcard,
    Variable(Name),
    Number(f64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub enum Type<P> {
    Error,
    Primitive(P),
    Tuple(Vec<Self>),
    Never,
    Function {
        params: Vec<Self>,
        return_ty: Box<Self>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum Primitive {
    Number,
    Bool,
}

#[derive(Clone, Copy, Debug)]
pub struct Name(u32);

impl Name {
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}
