use super::typed_ast::{PrimitiveType, Type};
use crate::{diagnostics::error::Error, span::Spanned};
use rustc_hash::FxHashMap;

#[derive(Debug, Default)]
pub struct Engine {
    id_counter: usize,
    vars: FxHashMap<TypeId, Spanned<TypeInfo>>,
}

#[derive(Clone, Copy, Debug)]
pub struct UnifyError {
    pub a: Spanned<TypeInfo>,
    pub b: Spanned<TypeInfo>,
}

impl Engine {
    pub fn insert(&mut self, info: Spanned<TypeInfo>) -> TypeId {
        let id = TypeId(self.id_counter);
        self.id_counter = self.id_counter.checked_add(1).unwrap();
        self.vars.insert(id, info);
        id
    }

    pub fn insert_type(&mut self, ty: Spanned<Type>) -> TypeId {
        let ty_info = ty.map(Self::type_to_info);

        self.insert(ty_info)
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), UnifyError> {
        let var_a = &self.vars[&a];
        let var_b = &self.vars[&b];

        match (&var_a.0, &var_b.0) {
            (TypeInfo::Ref(a), _) => self.unify(*a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, *b),

            (TypeInfo::Unknown, _) => {
                self.vars.insert(a, Spanned(TypeInfo::Ref(b), var_b.1));
                Ok(())
            }
            (_, TypeInfo::Unknown) => {
                self.vars.insert(b, Spanned(TypeInfo::Ref(a), var_a.1));
                Ok(())
            }

            (TypeInfo::Error, _) | (_, TypeInfo::Error) => Ok(()),

            (a, b) if a == b => Ok(()),

            _ => Err(UnifyError {
                a: *var_a,
                b: *var_b,
            }),
        }
    }

    pub fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type>, Error> {
        let var = &self.vars[&id];

        match &var.0 {
            TypeInfo::Unknown => Err(Error::CantInferType { span: var.1 }),
            TypeInfo::Ref(id) => Ok(self.reconstruct(*id)?.0),
            TypeInfo::Error => Ok(Type::Error),
            TypeInfo::Primitive(ty) => Ok(Type::Primitive(*ty)),
        }
        .map(|ty| Spanned(ty, var.1))
    }

    #[must_use]
    pub const fn type_to_info(ty: Type) -> TypeInfo {
        match ty {
            Type::Error => TypeInfo::Error,
            Type::Primitive(ty) => TypeInfo::Primitive(ty),
        }
    }

    #[must_use]
    pub fn info_to_type(&self, info: TypeInfo) -> Type {
        match info {
            TypeInfo::Unknown => Type::Error,
            TypeInfo::Ref(id) => self.reconstruct(id).unwrap().0,
            TypeInfo::Error => todo!(),
            TypeInfo::Primitive(_) => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(usize);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeInfo {
    Unknown,
    Ref(TypeId),
    Error,
    Primitive(PrimitiveType),
}
