use super::typed_ast::{PrimitiveType, Type};
use crate::{diagnostics::error::Error, span::Spanned};

#[derive(Debug, Default)]
pub struct Engine {
    id_counter: usize,
    vars: Vec<Spanned<TypeInfo>>,
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
        self.vars.push(info);
        assert_eq!(id.0, self.vars.len() - 1);
        id
    }

    pub fn insert_type(&mut self, ty: Spanned<Type>) -> TypeId {
        let ty_info = ty.map(Self::type_to_info);

        self.insert(ty_info)
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), UnifyError> {
        let var_a = &self.vars[a.0];
        let var_b = &self.vars[b.0];

        match (&var_a.0, &var_b.0) {
            (TypeInfo::Ref(a), _) => self.unify(*a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, *b),

            (TypeInfo::Unknown, _) => {
                self.vars[a.0] = Spanned(TypeInfo::Ref(b), var_b.1);
                Ok(())
            }
            (_, TypeInfo::Unknown) => {
                self.vars[b.0] = Spanned(TypeInfo::Ref(a), var_a.1);
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
        let var = &self.vars[id.0];

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

    pub fn info_to_type(&self, info: TypeInfo) -> Result<Type, Error> {
        Ok(match info {
            TypeInfo::Unknown | TypeInfo::Error => Type::Error,
            TypeInfo::Ref(id) => self.reconstruct(id)?.0,
            TypeInfo::Primitive(ty) => Type::Primitive(ty),
        })
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
