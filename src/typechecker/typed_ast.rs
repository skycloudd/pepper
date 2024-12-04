use crate::{
    lexer::tokens::Interned,
    parser::ast::{BinaryOp, Path, Type, UnaryOp},
    span::Spanned,
};

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypedAst(pub Vec<Spanned<Item>>);

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Item {
    Function(Spanned<Function>),
    Import(Spanned<Path>),
    Struct(Spanned<Struct>),
    Enum(Spanned<Enum>),
    Module(Spanned<Module>),
}

#[allow(clippy::module_name_repetitions)]
pub type TypedAstType = Type<Primitive>;

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Module {
    pub name: Spanned<Interned>,
    pub ast: Option<TypedAst>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Function {
    pub name: Spanned<Interned>,
    pub generics: Option<Spanned<Vec<Spanned<Interned>>>>,
    pub params: Spanned<Vec<Spanned<FunctionParam>>>,
    pub return_ty: Option<Spanned<TypedAstType>>,
    pub body: Spanned<TypedExpression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FunctionParam {
    pub name: Spanned<Interned>,
    pub ty: Spanned<TypedAstType>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Struct {
    pub name: Spanned<Interned>,
    pub generics: Option<Spanned<Vec<Spanned<Interned>>>>,
    pub fields: Spanned<Vec<Spanned<StructField>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructField {
    pub name: Spanned<Interned>,
    pub ty: Spanned<TypedAstType>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Enum {
    pub name: Spanned<Interned>,
    pub generics: Option<Spanned<Vec<Spanned<Interned>>>>,
    pub variants: Spanned<Vec<Spanned<EnumVariant>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum EnumVariant {
    Unit(Spanned<Interned>),
    Tuple {
        name: Spanned<Interned>,
        fields: Spanned<Vec<Spanned<TypedAstType>>>,
    },
    Struct {
        name: Spanned<Interned>,
        fields: Spanned<Vec<Spanned<StructField>>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: TypedAstType,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Expression {
    Int(Spanned<Interned>),
    Float(Spanned<Interned>),
    Bool(Spanned<Interned>),
    String(Spanned<Interned>),
    Name(Spanned<Interned>),
    Tuple(Spanned<Vec<Spanned<TypedExpression>>>),
    List(Spanned<Vec<Spanned<TypedExpression>>>),
    BinaryOp {
        op: Spanned<BinaryOp>,
        lhs: Spanned<Box<TypedExpression>>,
        rhs: Spanned<Box<TypedExpression>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Spanned<Box<TypedExpression>>,
    },
    Dot {
        expr: Spanned<Box<TypedExpression>>,
        field: Spanned<Interned>,
    },
    Call {
        callee: Spanned<Box<TypedExpression>>,
        args: Spanned<Vec<Spanned<TypedExpression>>>,
    },
    Match {
        expr: Spanned<Box<TypedExpression>>,
        arms: Spanned<Vec<Spanned<MatchArm>>>,
    },
    Binding {
        name: Spanned<Interned>,
        ty: Option<Spanned<TypedAstType>>,
        value: Spanned<Box<TypedExpression>>,
        body: Spanned<Box<TypedExpression>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<TypedExpression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Pattern {
    pub pattern_type: Spanned<PatternType>,
    pub condition: Option<Spanned<TypedExpression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum PatternType {
    Name(Spanned<Path>),
    Int(Spanned<Interned>),
    Float(Spanned<Interned>),
    Bool(Spanned<Interned>),
    String(Spanned<Interned>),
    Tuple(Spanned<Vec<Spanned<Pattern>>>),
    TupleType {
        name: Spanned<Path>,
        fields: Spanned<Vec<Spanned<Pattern>>>,
    },
    StructType {
        name: Spanned<Path>,
        fields: Spanned<Vec<Spanned<StructPatternField>>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct StructPatternField {
    pub name: Spanned<Interned>,
    pub pattern: Option<Spanned<Pattern>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Primitive {
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
}
