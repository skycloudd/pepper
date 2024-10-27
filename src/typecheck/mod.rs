use crate::{
    diagnostics::error::Error,
    lexer::tokens::Identifier,
    parser::ast::{self, Ast, Type},
    scopes::Scopes,
    span::Spanned,
};
use typed_ast::{
    Expression, Function, FunctionParam, Primitive, TopLevel, TypedAst, TypedExpression,
};

pub mod typed_ast;

pub fn typecheck(ast: Ast) -> (TypedAst, Vec<Error>) {
    Typechecker::default().typecheck_ast(ast)
}

#[derive(Default)]
struct Typechecker {
    errors: ErrorVec,
    names: Scopes<&'static str, Spanned<Type<Primitive>>>,
    functions: Scopes<&'static str, Spanned<Option<Type<Primitive>>>>,
    types: Scopes<&'static str, Type<Primitive>>,
}

impl Typechecker {
    fn typecheck_ast(mut self, ast: Ast) -> (TypedAst, Vec<Error>) {
        self.insert_primitive_types();

        for toplevel in &ast.0 {
            match &toplevel.0 {
                ast::TopLevel::Function(function) => self.insert_function_signature(function),
            }
        }

        (
            TypedAst(
                ast.0
                    .into_iter()
                    .filter_map(|toplevel| {
                        toplevel
                            .map(|toplevel| self.typecheck_toplevel(toplevel))
                            .transpose()
                    })
                    .collect(),
            ),
            self.errors.0,
        )
    }

    fn insert_primitive_types(&mut self) {
        for (name, ty) in &[("number", Primitive::Number), ("bool", Primitive::Bool)] {
            self.types.insert(name, Type::Primitive(*ty));
        }
    }

    fn insert_function_signature(&mut self, function: &Spanned<ast::Function>) {
        let function_type = function.as_ref().map(|function| {
            Some(Type::Function {
                params: function
                    .params
                    .as_ref()
                    .map(|params| {
                        params
                            .iter()
                            .map(|param| {
                                param
                                    .as_ref()
                                    .map(|param| self.lower_type(&param.ty))
                                    .transpose()
                            })
                            .collect::<Option<_>>()
                    })
                    .transpose()?,
                return_ty: function
                    .return_ty
                    .as_ref()
                    .map(|ty| self.lower_type(ty))
                    .transpose()?
                    .boxed(),
            })
        });

        let name = function.name.map(Identifier::resolve);

        if let Some(previous) = self.functions.insert(name.0, function_type) {
            self.errors.push(Error::FunctionRedefinition {
                name: name.0,
                new_span: function.name.1,
                previous_span: previous.1,
            });
        }
    }

    fn typecheck_toplevel(&mut self, toplevel: ast::TopLevel) -> Option<TopLevel> {
        Some(match toplevel {
            ast::TopLevel::Function(function) => TopLevel::Function(
                function
                    .map(|function| self.typecheck_function(function))
                    .transpose()?,
            ),
        })
    }

    fn typecheck_function(&mut self, function: ast::Function) -> Option<Function> {
        let params = function
            .params
            .map(|params| {
                params
                    .into_iter()
                    .map(|param| {
                        param
                            .map(|param| self.lower_function_param(&param))
                            .transpose()
                            .inspect(|param| {
                                self.names.insert(param.name.resolve(), param.ty.clone());
                            })
                    })
                    .collect::<Option<_>>()
            })
            .transpose()?;

        let return_ty = function
            .return_ty
            .as_ref()
            .map(|ty| self.lower_type(ty))
            .transpose()?;

        let body = function
            .body
            .map(|body| self.lower_expression(body))
            .transpose()?;

        Some(Function {
            name: function.name,
            params,
            return_ty,
            body,
        })
    }

    fn lower_function_param(&mut self, param: &ast::FunctionParam) -> Option<FunctionParam> {
        Some(FunctionParam {
            name: param.name,
            ty: param
                .ty
                .as_ref()
                .map(|ty| self.lower_type(ty))
                .transpose()?,
        })
    }

    fn lower_expression(&mut self, expr: ast::Expression) -> Option<TypedExpression> {
        Some(match expr {
            ast::Expression::Number(value) => TypedExpression {
                expr: Expression::Number(value),
                ty: Type::Primitive(Primitive::Number),
            },
            ast::Expression::Bool(value) => TypedExpression {
                expr: Expression::Bool(value),
                ty: Type::Primitive(Primitive::Bool),
            },
            ast::Expression::Variable(name) => {
                let ty = self
                    .names
                    .get(&name.resolve())
                    .cloned()
                    .or_else(|| {
                        self.functions
                            .get(&name.resolve())
                            .cloned()
                            .and_then(Spanned::transpose)
                    })
                    .or_else(|| {
                        self.errors.push(Error::UndefinedVariable {
                            name: name.resolve(),
                            span: name.1,
                        });

                        None
                    })?;

                TypedExpression {
                    expr: Expression::Variable(name),
                    ty: ty.0,
                }
            }
            ast::Expression::BinaryOp { op, lhs, rhs } => return None,
            ast::Expression::UnaryOp { op, expr } => return None,
            ast::Expression::Call { callee, args } => {
                let callee = callee
                    .map(|callee| self.lower_expression(*callee))
                    .transpose()?;

                let args = args
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| arg.map(|arg| self.lower_expression(arg)).transpose())
                            .collect::<Option<Vec<_>>>()
                    })
                    .transpose()?;

                if let Type::Function { params, return_ty } = callee.ty.clone() {
                    for (arg, param) in args.as_ref().0.iter().zip(params.0) {
                        assert_eq!(arg.0.ty, param.0);
                    }

                    TypedExpression {
                        expr: Expression::Call {
                            callee: callee.boxed(),
                            args,
                        },
                        ty: *return_ty.0,
                    }
                } else {
                    todo!()
                }
            }
        })
    }

    fn lower_type(&mut self, ty: &Type<Identifier>) -> Option<Type<Primitive>> {
        Some(match ty {
            Type::Primitive(name) => self
                .types
                .get(&name.resolve())
                .or_else(|| {
                    self.errors.push(todo!());

                    None
                })?
                .clone(),
            Type::Tuple(inner) => Type::Tuple(
                inner
                    .iter()
                    .map(|ty| ty.as_ref().map(|ty| self.lower_type(ty)).transpose())
                    .collect::<Option<_>>()?,
            ),
            Type::Never => Type::Never,
            Type::Function { params, return_ty } => Type::Function {
                params: params
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
                return_ty: return_ty
                    .as_ref()
                    .map(|ty| self.lower_type(ty))
                    .transpose()?
                    .boxed(),
            },
        })
    }
}

#[derive(Default)]
struct ErrorVec(Vec<Error>);

impl ErrorVec {
    fn push(&mut self, error: Error) {
        self.0.push(error);
    }
}
