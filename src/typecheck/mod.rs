use crate::{
    diagnostics::error::Error,
    parser::ast::{self, Ast, BinaryOp, FunctionType, Identifier, Type, UnaryOp},
    scopes::Scopes,
    span::{Span, Spanned},
};
use chumsky::span::Span as _;
use polytype::Context;
use rustc_hash::FxHashMap;
use typed_ast::{
    Expression, Function, FunctionParam, Primitive, TopLevel, TypedAst, TypedExpression,
};

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
    functions: FxHashMap<&'static str, FunctionType<Primitive>>,
    ctx: Context<TyName>,
}

impl<'a> Typechecker<'a> {
    fn new(errors: &'a mut Vec<Error>) -> Self {
        Self {
            errors,
            types: FxHashMap::default(),
            vars: Scopes::default(),
            functions: FxHashMap::default(),
            ctx: Context::default(),
        }
    }

    fn typecheck_ast(&mut self, ast: Ast) -> TypedAst {
        self.primitive_types();

        for toplevel in &ast.toplevels {
            match &toplevel.0 {
                ast::TopLevel::Function(function) => {
                    let fn_type = self.function_type(&function.0);

                    if let Some(fn_type) = fn_type {
                        self.functions
                            .insert(function.0.name.0.resolve(), fn_type.clone());

                        self.vars.insert(
                            function.0.name.0.resolve(),
                            polytype_type_from_ty(&Type::Function(fn_type)),
                        );
                    }
                }
                ast::TopLevel::Do(_expr) => {}
            }
        }

        TypedAst {
            toplevels: ast
                .toplevels
                .into_iter()
                .filter_map(|toplevel| {
                    toplevel
                        .map(|toplevel| self.typecheck_toplevel(toplevel))
                        .transpose()
                })
                .collect(),
        }
    }

    fn typecheck_toplevel(&mut self, toplevel: ast::TopLevel) -> Option<TopLevel> {
        match toplevel {
            ast::TopLevel::Function(function) => function
                .map(|function| self.typecheck_function(function))
                .transpose()
                .map(TopLevel::Function),
            ast::TopLevel::Do(expr) => expr
                .map(|expr| self.typecheck_expression(expr))
                .transpose()
                .map(TopLevel::Do),
        }
    }

    fn function_type(&mut self, function: &ast::Function) -> Option<FunctionType<Primitive>> {
        Some(FunctionType {
            params: function
                .params
                .as_ref()
                .map(|params| {
                    params
                        .iter()
                        .map(|param| {
                            param
                                .as_ref()
                                .map(|param| self.lower_type(&param.ty.0))
                                .transpose()
                        })
                        .collect::<Option<_>>()
                })
                .transpose()?,
            return_ty: function
                .return_ty
                .as_ref()
                .map_or(
                    Spanned(Some(Type::Unit), function.params.1.to_end()),
                    |return_ty| {
                        return_ty
                            .as_ref()
                            .map(|return_ty| self.lower_type(return_ty))
                    },
                )
                .transpose()?
                .boxed(),
        })
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

        let function_ty = self.functions.get(&function.name.0.resolve())?;

        let params = function_ty.params.as_ref().map(|params| {
            params
                .iter()
                .zip(function.params.0.into_iter().map(|param| param.0.name))
                .map(|(param, name)| {
                    self.vars
                        .insert(name.0.resolve(), polytype_type_from_ty(&param.0));

                    param
                        .as_ref()
                        .cloned()
                        .map_self(|ty| FunctionParam { name, ty })
                })
                .collect()
        });

        let return_ty = function_ty.return_ty.clone();

        let body = function
            .body
            .map(|body| self.typecheck_expression(body))
            .transpose();

        self.vars.pop_scope();

        body.map(|body| -> Option<_> {
            let body_ty = body.as_ref().map(|body| polytype_type_from_ty(&body.ty));

            let return_ty = return_ty.unbox().as_ref().map(polytype_type_from_ty);

            if let Err(err) = self.ctx.unify(&body_ty.0, &return_ty.0) {
                self.errors
                    .push(Error::from_unification_error(err, body_ty.1, return_ty.1));
            }

            let return_ty = return_ty
                .map(|ty| ty.apply(&self.ctx))
                .as_ref()
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

    #[allow(clippy::too_many_lines)]
    fn typecheck_expression(&mut self, expr: ast::Expression) -> Option<TypedExpression> {
        match expr {
            ast::Expression::Unit => Some(TypedExpression {
                expr: Expression::Unit,
                ty: Type::Unit,
            }),
            ast::Expression::Number(value) => Some(TypedExpression {
                expr: Expression::Number(value),
                ty: Type::Primitive(Primitive::Number),
            }),
            ast::Expression::Bool(value) => Some(TypedExpression {
                expr: Expression::Bool(value),
                ty: Type::Primitive(Primitive::Bool),
            }),
            ast::Expression::Variable(name) => {
                let ty = self
                    .vars
                    .get(&name.0.resolve())
                    .or_else(|| {
                        self.errors.push(Error::CantFindFunction {
                            name: name.0.resolve(),
                            span: name.1,
                        });
                        None
                    })?
                    .clone();

                let var_ty = ty.apply(&self.ctx);

                Some(TypedExpression {
                    expr: Expression::Variable(name),
                    ty: ty_from_polytype_type(&var_ty)?,
                })
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

                Some(TypedExpression {
                    expr: Expression::BinaryOp {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                    ty: ty_from_polytype_type(&result_ty)?,
                })
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

                Some(TypedExpression {
                    expr: Expression::UnaryOp {
                        op,
                        expr: expr.boxed(),
                    },
                    ty: ty_from_polytype_type(&result_ty)?,
                })
            }
            ast::Expression::Call { name, args } => self.typecheck_call(name.unbox(), args),
        }
    }

    fn typecheck_call(
        &mut self,
        name: Spanned<ast::Expression>,
        args: Spanned<Vec<Spanned<ast::Expression>>>,
    ) -> Option<TypedExpression> {
        let name = name
            .map(|name| self.typecheck_expression(name))
            .transpose()?;

        let args = args
            .map(|args| {
                args.into_iter()
                    .map(|arg| arg.map(|arg| self.typecheck_expression(arg)).transpose())
                    .collect::<Option<Vec<_>>>()
            })
            .transpose()?;

        let callee_ty = polytype_type_from_ty(&name.0.ty);

        if callee_ty.as_arrow().is_none() {
            self.errors.push(Error::NotFunction { span: name.1 });
            return None;
        }

        let arg_types = args
            .as_ref()
            .0
            .iter()
            .map(|arg| polytype_type_from_ty(&arg.0.ty));

        let return_ty = self.ctx.new_variable();

        let mut call_ty_args = arg_types
            .chain(core::iter::once(return_ty.clone()))
            .collect::<Vec<_>>();

        assert!(!call_ty_args.is_empty());

        if call_ty_args.len() == 1 {
            call_ty_args.insert(0, polytype_type_from_ty(&Type::Unit));
        }

        let call_ty = polytype::Type::Constructed(TyName::Arrow, call_ty_args);

        if let Err(err) = self.ctx.unify(&callee_ty, &call_ty) {
            self.errors
                .push(Error::from_unification_error(err, name.1, args.1));
        }

        let return_ty = return_ty.apply(&self.ctx);

        Some(TypedExpression {
            expr: Expression::Call {
                name: name.boxed(),
                args,
            },
            ty: ty_from_polytype_type(&return_ty)?,
        })
    }

    fn lower_type(&mut self, ty: &Type<Identifier>) -> Option<Type<Primitive>> {
        match ty {
            Type::Primitive(name) => self.types.get(name.resolve()).cloned().or_else(|| {
                self.errors.push(Error::CantFindType {
                    name: name.resolve(),
                    span: name.0 .1,
                });
                None
            }),
            Type::Unit => Some(Type::Unit),
            Type::Never => Some(Type::Never),
            Type::Function(function) => Some(Type::Function(self.lower_function_type(function)?)),
        }
    }

    fn lower_function_type(
        &mut self,
        function: &FunctionType<Identifier>,
    ) -> Option<FunctionType<Primitive>> {
        Some(FunctionType {
            params: function
                .params
                .as_ref()
                .map(|params| {
                    params
                        .iter()
                        .map(|param| {
                            param
                                .as_ref()
                                .map(|param| self.lower_type(param))
                                .transpose()
                        })
                        .collect::<Option<_>>()
                })
                .transpose()?,
            return_ty: function
                .return_ty
                .as_ref()
                .map(|return_ty| self.lower_type(return_ty))
                .transpose()?
                .boxed(),
        })
    }
}

fn polytype_type_from_ty(ty: &Type<Primitive>) -> polytype::Type<TyName> {
    match ty {
        Type::Primitive(primitive) => {
            polytype::Type::Constructed(TyName::Primitive(*primitive), vec![])
        }
        Type::Unit => polytype::Type::Constructed(TyName::Unit, vec![]),
        Type::Never => polytype::Type::Constructed(TyName::Never, vec![]),
        Type::Function(function_type) => {
            let args = function_type
                .params
                .0
                .iter()
                .map(|param| polytype_type_from_ty(&param.0))
                .chain(core::iter::once(polytype_type_from_ty(
                    &function_type.return_ty.0,
                )))
                .collect::<Vec<_>>();

            polytype::Type::Constructed(TyName::Arrow, args)
        }
    }
}

fn ty_from_polytype_type(ty: &polytype::Type<TyName>) -> Option<Type<Primitive>> {
    match ty {
        arrow @ polytype::Type::Constructed(TyName::Arrow, _) => {
            let params = arrow
                .args()
                .unwrap()
                .into_iter()
                .map(ty_from_polytype_type)
                .map(|ty| ty.map(Box::new))
                .collect::<Option<Vec<_>>>()?;

            let return_ty = Box::new(ty_from_polytype_type(arrow.returns().unwrap())?);

            Some(Type::Function(FunctionType {
                params: Spanned(
                    params
                        .into_iter()
                        .map(|param| Spanned(*param, Span::default()))
                        .collect(),
                    Span::default(),
                ),
                return_ty: Spanned(return_ty, Span::default()),
            }))
        }
        polytype::Type::Constructed(TyName::Unit, _) => Some(Type::Unit),
        polytype::Type::Constructed(TyName::Never, _) => Some(Type::Never),
        polytype::Type::Constructed(TyName::Primitive(primitive), _) => {
            Some(Type::Primitive(*primitive))
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
