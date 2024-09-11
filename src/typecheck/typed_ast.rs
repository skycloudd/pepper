use crate::{
    parser::ast::{BinaryOp, Identifier, UnaryOp},
    span::Spanned,
};
use ordered_float::OrderedFloat;
use std::borrow::Cow;

#[derive(Clone, Debug)]
pub struct TypedAst {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Spanned<Type>,
    pub body: Spanned<Block>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionParam {
    pub name: Spanned<Identifier>,
    pub ty: Spanned<Type>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    Expression(Spanned<TypedExpression>),
    Let {
        name: Spanned<Identifier>,
        ty: Spanned<Type>,
        value: Spanned<TypedExpression>,
    },
    Assign {
        name: Spanned<Identifier>,
        value: Spanned<TypedExpression>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedExpression {
    pub ty: Type,
    pub expr: Expression,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    Integer(Spanned<u128>),
    Float(Spanned<OrderedFloat<f64>>),
    Bool(Spanned<bool>),
    Variable(Spanned<Identifier>),
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
        name: Spanned<Identifier>,
        args: Spanned<Vec<Spanned<TypedExpression>>>,
    },
    Block(Spanned<Box<Block>>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block {
    pub statements: Spanned<Vec<Spanned<Statement>>>,
    pub return_expr: Option<Spanned<TypedExpression>>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Error,
    Primitive(PrimitiveType),
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Error => Cow::Borrowed("<error>"),
                Self::Primitive(ty) => Cow::Owned(ty.to_string()),
            }
        )
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
    Unit,
}

impl core::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int8 => "int8",
                Self::Int16 => "int16",
                Self::Int32 => "int32",
                Self::Int64 => "int64",
                Self::Uint8 => "uint8",
                Self::Uint16 => "uint16",
                Self::Uint32 => "uint32",
                Self::Uint64 => "uint64",
                Self::Float32 => "float32",
                Self::Float64 => "float64",
                Self::Bool => "bool",
                Self::Unit => "unit",
            }
        )
    }
}
