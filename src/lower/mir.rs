#![allow(dead_code)]

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub struct Mir {
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub struct Function {
    pub name: Name,
    pub params: Vec<FuncParam>,
    pub return_ty: Type<Primitive>,
    pub body: TypedExpression,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub struct FuncParam {
    pub name: Name,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type<Primitive>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub enum Expression {
    Number(f64),
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
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: TypedExpression,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub struct Pattern {
    pub pattern_type: PatternType,
    pub condition: Option<TypedExpression>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub enum PatternType {
    Wildcard,
    Variable(Name),
    Number(f64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
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
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub enum Primitive {
    Number,
    Bool,
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
pub struct Name(u32);

impl Name {
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}

bin_op_intrinsics! {
    AddNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    SubNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    MulNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    DivNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    EqNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    NeqNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    LtNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    LteNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    GtNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    GteNumbers { lhs, rhs }, Type::Primitive(Primitive::Number),
    NegNumber { expr }, Type::Primitive(Primitive::Number),
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
        #[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
        pub enum Intrinsic {
            $(
                $name($name),
            )*
        }

        $(
            #[derive(Clone, Debug)]
            #[cfg_attr(test, derive(serde::Deserialize, serde::Serialize))]
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
