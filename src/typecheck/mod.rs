use crate::{
    diagnostics::error::Error,
    parser::ast::{self, Ast, BinaryOp, FunctionType, Identifier, Type, UnaryOp},
    scopes::Scopes,
    span::Spanned,
};
use chumsky::span::Span as _;
use polytype::Context;
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
    vars: Scopes<&'static str, polytype::Type<TyName>>,
    ctx: Context<TyName>,
}

impl<'a> Typechecker<'a> {
    fn new(errors: &'a mut Vec<Error>) -> Self {
        Self {
            errors,
            types: FxHashMap::default(),
            vars: Scopes::default(),
            ctx: Context::default(),
        }
    }

    fn typecheck_ast(&mut self, ast: Ast) -> TypedAst {
        self.primitive_types();

        TypedAst {
            functions: ast
                .functions
                .into_iter()
                .filter_map(|function| {
                    function
                        .map(|function| self.typecheck_function(function))
                        .transpose()
                })
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

    fn typecheck_function(&mut self, function: ast::Function) -> Option<Function> {
        self.vars.push_scope();

        let params = function.params.as_ref().map(|params| {
            params
                .iter()
                .map(|param| {
                    param.as_ref().map(|param| {
                        let param_ty = self.lower_type(&param.ty.0);
                        let param_ty = polytype_type_from_ty(&param_ty);

                        self.vars.insert(param.name.0.resolve(), param_ty);

                        self.function_param(param)
                    })
                })
                .collect()
        });

        let return_ty = match function.return_ty.as_ref() {
            Some(return_ty) => return_ty.as_ref().map(|ty| self.lower_type(ty)),

            None => Spanned::new(Type::Unit, function.params.1.to_end()),
        };

        let body = function
            .body
            .map(|body| self.typecheck_expression(body))
            .transpose();

        self.vars.pop_scope();

        body.map(|body| -> Option<_> {
            let body_ty = body.as_ref().map(|body| polytype_type_from_ty(&body.ty));

            let return_ty = return_ty.as_ref().map(polytype_type_from_ty);

            if let Err(err) = self.ctx.unify(&body_ty.0, &return_ty.0) {
                self.errors
                    .push(Error::from_unification_error(err, body_ty.1, return_ty.1));
            }

            let return_ty = return_ty
                .map(|ty| ty.apply(&self.ctx))
                .map(ty_from_polytype_type)
                .transpose()?;

            Some(Function {
                name: function.name,
                params,
                return_ty,
                body,
            })
        })?
    }

    fn typecheck_expression(&mut self, expr: ast::Expression) -> Option<TypedExpression> {
        Some(match expr {
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
            ast::Expression::Variable(identifier) => {
                let ty = self.vars.get(&identifier.resolve()).unwrap().clone();

                let var_ty = ty.apply(&self.ctx);

                TypedExpression {
                    expr: Expression::Variable(identifier),
                    ty: ty_from_polytype_type(var_ty)?,
                }
            }
            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = lhs.map(|lhs| self.typecheck_expression(*lhs)).transpose()?;
                let rhs = rhs.map(|rhs| self.typecheck_expression(*rhs)).transpose()?;

                let lhs_ty = polytype_type_from_ty(&lhs.0.ty);
                let rhs_ty = polytype_type_from_ty(&rhs.0.ty);

                let number = polytype_type_from_ty(&Type::Primitive(Primitive::Number));

                let bin_op_rule = match op.0 {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        polytype::Type::from(vec![number; 3])
                    }
                };

                let result_ty = self.ctx.new_variable();

                let bin_op_result = polytype::Type::from(vec![lhs_ty, rhs_ty, result_ty.clone()]);

                if let Err(err) = self.ctx.unify(&bin_op_rule, &bin_op_result) {
                    self.errors
                        .push(Error::from_unification_error(err, lhs.1, rhs.1));
                }

                let result_ty = result_ty.apply(&self.ctx);

                TypedExpression {
                    expr: Expression::BinaryOp {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                    ty: ty_from_polytype_type(result_ty)?,
                }
            }
            ast::Expression::UnaryOp { op, expr } => {
                let expr = expr
                    .map(|expr| self.typecheck_expression(*expr))
                    .transpose()?;

                let expr_ty = polytype_type_from_ty(&expr.0.ty);

                let number = polytype_type_from_ty(&Type::Primitive(Primitive::Number));

                let unary_op_rule = match op.0 {
                    UnaryOp::Neg => polytype::Type::from(vec![number; 2]),
                };

                let result_ty = self.ctx.new_variable();

                let unary_op_result = polytype::Type::from(vec![expr_ty, result_ty.clone()]);

                if let Err(err) = self.ctx.unify(&unary_op_rule, &unary_op_result) {
                    self.errors
                        .push(Error::from_unification_error(err, expr.1, expr.1));
                }

                let result_ty = result_ty.apply(&self.ctx);

                TypedExpression {
                    expr: Expression::UnaryOp {
                        op,
                        expr: expr.boxed(),
                    },
                    ty: ty_from_polytype_type(result_ty)?,
                }
            }
            ast::Expression::Call { name, args } => todo!(),
        })
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
            params: function
                .params
                .iter()
                .map(|param| self.lower_type(param))
                .collect(),
            return_ty: Box::new(self.lower_type(&function.return_ty)),
        }
    }
}

fn polytype_type_from_ty(ty: &Type<Primitive>) -> polytype::Type<TyName> {
    match ty {
        Type::Primitive(primitive) => {
            polytype::Type::Constructed(TyName::Primitive(*primitive), vec![])
        }
        Type::Unit => polytype::Type::Constructed(TyName::Unit, vec![]),
        Type::Never => polytype::Type::Constructed(TyName::Never, vec![]),
        Type::Function(function_type) => polytype::Type::Constructed(
            TyName::Arrow,
            function_type
                .params
                .iter()
                .map(polytype_type_from_ty)
                .chain(core::iter::once(polytype_type_from_ty(
                    &function_type.return_ty,
                )))
                .collect(),
        ),
    }
}

fn ty_from_polytype_type(ty: polytype::Type<TyName>) -> Option<Type<Primitive>> {
    match ty {
        arrow @ polytype::Type::Constructed(TyName::Arrow, _) => {
            let return_ty = Box::new(ty_from_polytype_type(arrow.returns().unwrap().clone())?);

            let params = arrow
                .args_destruct()
                .unwrap()
                .iter()
                .map(|arg| ty_from_polytype_type(arg.clone()))
                .collect::<Option<Vec<_>>>()?;

            Some(Type::Function(FunctionType { params, return_ty }))
        }
        polytype::Type::Constructed(TyName::Unit, _) => Some(Type::Unit),
        polytype::Type::Constructed(TyName::Never, _) => Some(Type::Never),
        polytype::Type::Constructed(TyName::Primitive(primitive), _) => {
            Some(Type::Primitive(primitive))
        }
        polytype::Type::Variable(_) => None,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyName {
    Arrow,
    Unit,
    Never,
    Primitive(Primitive),
}

impl polytype::Name for TyName {
    fn arrow() -> Self {
        Self::Arrow
    }

    fn show(&self) -> String {
        match self {
            Self::Arrow => "->".to_string(),
            Self::Unit => "#".to_string(),
            Self::Never => "!".to_string(),
            Self::Primitive(primitive) => primitive.to_string(),
        }
    }
}
