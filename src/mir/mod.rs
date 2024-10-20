use crate::RODEO;
use lasso::Spur;
use malachite::Rational;
use nonempty::NonEmpty;

pub mod lower;

#[derive(Clone, Debug)]
pub struct Mir {
    pub functions: Vec<Function>,
    pub main: Vec<TypedExpression>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub params: NonEmpty<FunctionParam>,
    pub return_ty: Type<Primitive>,
    pub body: TypedExpression,
}

#[derive(Clone, Debug)]
pub struct FunctionParam {
    pub name: Identifier,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Unit,
    Number(Rational),
    Bool(bool),
    Variable(Identifier),
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
        name: Box<TypedExpression>,
        args: NonEmpty<Box<TypedExpression>>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<P> {
    Primitive(P),
    Unit,
    Never,
    Function(FunctionType<P>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    Number,
    Bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionType<P> {
    pub params: Vec<Type<P>>,
    pub return_ty: Box<Type<P>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Identifier(pub Spur);

impl Identifier {
    pub fn resolve(self) -> &'static str {
        RODEO.resolve(&self.0)
    }
}
