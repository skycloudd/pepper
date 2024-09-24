use crate::{
    diagnostics::error::Error,
    parser::ast::{self, Ast, FunctionType, Identifier, Type},
};
use rustc_hash::FxHashMap;
use typed_ast::{Expression, Function, FunctionParam, Primitive, TypedAst, TypedExpression};

pub mod typed_ast;

pub fn typecheck(ast: Ast) -> (Option<TypedAst>, Vec<Error>) {
    let mut errors = Vec::new();

    (
        Some(Typechecker::new(&mut errors).typecheck_ast(ast)),
        errors,
    )
}

struct Typechecker<'a> {
    errors: &'a mut Vec<Error>,
    types: FxHashMap<&'static str, Type<Primitive>>,
}

impl<'a> Typechecker<'a> {
    fn new(errors: &'a mut Vec<Error>) -> Self {
        Self {
            errors,
            types: FxHashMap::default(),
        }
    }

    fn typecheck_ast(&mut self, ast: Ast) -> TypedAst {
        self.primitive_types();

        TypedAst {
            functions: ast
                .functions
                .into_iter()
                .map(|function| function.map(|function| self.typecheck_function(function)))
                .collect(),
        }
    }

    fn primitive_types(&mut self) {
        const PRIMITIVE_TYPES: [(&str, Primitive); 2] =
            [("number", Primitive::Number), ("bool", Primitive::Bool)];

        for (name, ty) in PRIMITIVE_TYPES {
            self.types.insert(name, Type::Primitive(ty));
        }
    }

    fn typecheck_function(&self, function: ast::Function) -> Function {
        Function {
            name: function.name,
            params: function.params.as_ref().map(|params| {
                params
                    .iter()
                    .map(|param| param.as_ref().map(|param| self.function_param(param)))
                    .collect()
            }),
            return_ty: function
                .return_ty
                .as_ref()
                .map(|return_ty| return_ty.as_ref().map(|ty| self.lower_type(ty))),
            body: function.body.map(|body| self.typecheck_expression(body)),
        }
    }

    fn typecheck_expression(&self, expr: ast::Expression) -> TypedExpression {
        match expr {
            ast::Expression::Unit => TypedExpression {
                expr: Expression::Unit,
                ty: Type::Unit,
            },
            ast::Expression::Number(value) => TypedExpression {
                expr: Expression::Number(value),
                ty: Type::Primitive(Primitive::Number),
            },
            ast::Expression::Bool(value) => TypedExpression {
                expr: Expression::Bool(value),
                ty: Type::Primitive(Primitive::Bool),
            },
            ast::Expression::Variable(identifier) => todo!(),
            ast::Expression::BinaryOp { op, lhs, rhs } => todo!(),
            ast::Expression::UnaryOp { op, expr } => todo!(),
            ast::Expression::Call { name, args } => todo!(),
        }
    }

    fn function_param(&self, params: &ast::FunctionParam) -> FunctionParam {
        FunctionParam {
            name: params.name,
            ty: params.ty.as_ref().map(|ty| self.lower_type(ty)),
        }
    }

    fn lower_type(&self, ty: &Type<Identifier>) -> Type<Primitive> {
        match ty {
            Type::Primitive(name) => self.types.get(name.resolve()).unwrap().clone(),
            Type::Unit => Type::Unit,
            Type::Never => Type::Never,
            Type::Function(function) => Type::Function(self.lower_function_type(function)),
        }
    }

    fn lower_function_type(&self, function: &FunctionType<Identifier>) -> FunctionType<Primitive> {
        FunctionType {
            params: function.params.as_ref().map(|params| {
                params
                    .iter()
                    .map(|param| param.as_ref().map(|param| self.lower_type(param)))
                    .collect()
            }),
            return_ty: function
                .return_ty
                .as_ref()
                .map(|ty| ty.as_ref().map(|ty| self.lower_type(ty)).boxed()),
        }
    }
}
