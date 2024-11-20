#![allow(dead_code)]

#[derive(Clone, Debug)]
pub struct Mir {
    pub functions: Vec<Function>,
    pub externs: Vec<Extern>,
    pub modules: Vec<Self>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Name,
    pub params: Vec<FuncParam>,
    pub return_ty: Type<Primitive>,
    pub body: TypedExpression,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub name: Name,
    pub params: Vec<FuncParam>,
    pub return_ty: Type<Primitive>,
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
    Int(u64),
    Float(f64),
    Bool(bool),
    Variable(Name),
    Intrinsic(Box<Intrinsic>),
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
    Int(u64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    Int,
    Float,
    Bool,
}

#[derive(Clone, Copy, Debug)]
pub struct Name(u32);

impl Name {
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}

bin_op_intrinsics! {
    AddInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    SubInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    MulInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    DivInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    EqInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    NeqInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    LtInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    LteInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    GtInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    GteInts { lhs, rhs }, Type::Primitive(Primitive::Int),
    AddFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    SubFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    MulFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    DivFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    EqFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    NeqFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    LtFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    LteFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    GtFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    GteFloats { lhs, rhs }, Type::Primitive(Primitive::Float),
    NegInt { expr }, Type::Primitive(Primitive::Int),
    NegFloat { expr }, Type::Primitive(Primitive::Float),
    EqBools { lhs, rhs }, Type::Primitive(Primitive::Bool),
    NeqBools { lhs, rhs }, Type::Primitive(Primitive::Bool),
    AndBools { lhs, rhs }, Type::Primitive(Primitive::Bool),
    OrBools { lhs, rhs }, Type::Primitive(Primitive::Bool),
    NotBool { expr }, Type::Primitive(Primitive::Bool),
}

macro_rules! bin_op_intrinsics {
    (
        $(
            $name:ident { $($field:ident),* $(,)? },
            $sub_expr_ty:expr,
        )* $(,)?
    ) => {
        #[derive(Clone, Debug)]
        pub enum Intrinsic {
            $(
                $name($name),
            )*
        }

        $(
            #[derive(Clone, Debug)]
            pub struct $name {
                $(pub $field: TypedExpression,)*
            }

            impl $name {
                pub fn new($($field: TypedExpression,)*) -> Self {
                    $(
                        assert_eq!($field.ty, $sub_expr_ty);
                    )*

                    Self { $($field,)* }
                }
            }
        )*

        paste::paste! {
            impl Intrinsic {
                $(
                    pub fn [<$name:snake>]($($field: TypedExpression,)*) -> Self {
                        Self::$name($name::new($($field,)*))
                    }
                )*
            }
        }
    };
}
use bin_op_intrinsics;
