use crate::{
    diagnostics::error::Error,
    lexer::tokens::Identifier,
    parser::ast::{self, Ast, BinaryOp, Type},
    scopes::Scopes,
    span::{Span, Spanned},
};
use chumsky::span::Span as _;
use typed_ast::{
    Expression, Function, FunctionParam, MatchArm, Pattern, PatternType, Primitive, TopLevel,
    TypedAst, TypedExpression,
};

pub mod typed_ast;

pub fn typecheck(ast: Ast) -> (TypedAst, Vec<Error>) {
    Typechecker::default().typecheck_ast(ast)
}

#[derive(Default)]
struct Typechecker {
    errors: ErrorVec,
    names: Scopes<&'static str, Spanned<Type<Primitive>>>,
    functions: Scopes<&'static str, Spanned<Option<FunctionSignature>>>,
    types: Scopes<&'static str, Type<Primitive>>,
    engine: Engine,
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
        let function_type = function
            .as_ref()
            .map(|function| self.function_signature(function));

        if let Some(signature) = &function_type.0 {
            let ty = Type::Function {
                params: signature
                    .params
                    .clone()
                    .map(|params| params.into_iter().map(|param| param.ty.clone()).collect()),
                return_ty: signature.return_ty.clone().boxed(),
            };

            self.names
                .insert(function.name.resolve(), Spanned::new(ty, function_type.1));
        }

        let name = function.name.map(Identifier::resolve);

        if let Some(previous) = self.functions.insert(name.0, function_type) {
            self.errors.push(Error::FunctionRedefinition {
                name: name.0,
                new_span: function.name.1,
                previous_span: previous.1,
            });
        }
    }

    fn function_signature(&mut self, function: &ast::Function) -> Option<FunctionSignature> {
        Some(FunctionSignature {
            params: function
                .params
                .as_ref()
                .map(|params| {
                    params
                        .iter()
                        .map(|param| {
                            param
                                .as_ref()
                                .map(|param| self.lower_function_param(param))
                                .transpose()
                        })
                        .collect::<Option<_>>()
                })
                .transpose()?,
            return_ty: function
                .return_ty
                .as_ref()
                .map(|ty| self.lower_type(ty))
                .transpose()?,
        })
    }

    fn typecheck_toplevel(&mut self, toplevel: ast::TopLevel) -> Option<TopLevel> {
        Some(match toplevel {
            ast::TopLevel::Function(function) => TopLevel::Function(
                function
                    .map(|function| {
                        self.names.push_scope();
                        let function = self.typecheck_function(function);
                        self.names.pop_scope();

                        function
                    })
                    .transpose()?,
            ),
        })
    }

    fn typecheck_function(&mut self, function: ast::Function) -> Option<Function> {
        let sig = &self.functions[&function.name.resolve()];

        sig.0.clone().and_then(|sig| {
            let params = sig.params;

            for param in &params.0 {
                self.names.insert(param.name.resolve(), param.ty.clone());
            }

            let return_ty = sig.return_ty;

            let body = function
                .body
                .map(|body| self.lower_expression(body))
                .transpose()?;

            {
                let return_ty_var = self.engine.insert_type(&return_ty.0, return_ty.1);
                let body_ty = self.engine.insert_type(&body.ty, body.1);

                if self.engine.unify(return_ty_var, body_ty).is_err() {
                    self.errors.push(Error::BodyTypeMismatch {
                        return_ty: return_ty.0.clone(),
                        body_ty: body.ty.clone(),
                        return_span: return_ty.1,
                        body_span: body.1,
                    });
                }
            }

            Some(Function {
                name: function.name,
                params,
                return_ty,
                body,
            })
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

    #[allow(clippy::too_many_lines)]
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
                let ty = self.names.get(&name.resolve()).cloned().or_else(|| {
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
            ast::Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = lhs.map(|lhs| self.lower_expression(*lhs)).transpose()?;
                let rhs = rhs.map(|rhs| self.lower_expression(*rhs)).transpose()?;

                let lhs_ty = self.engine.insert_type(&lhs.ty, lhs.1);
                let rhs_ty = self.engine.insert_type(&rhs.ty, rhs.1);

                if self.engine.unify(lhs_ty, rhs_ty).is_err() {
                    self.errors.push(Error::BinaryOperatorTypeMismatch {
                        op: op.0,
                        op_span: op.1,
                        lhs_ty: lhs.ty.clone(),
                        rhs_ty: rhs.ty.clone(),
                        lhs_span: lhs.1,
                        rhs_span: rhs.1,
                    });

                    return None;
                }

                let lhs_ty = self.engine.solve(lhs_ty).ok()?;
                let rhs_ty = self.engine.solve(rhs_ty).ok()?;

                let ty = match (op.0, (&lhs_ty.0, &rhs_ty.0)) {
                    (
                        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div,
                        (Type::Primitive(Primitive::Number), Type::Primitive(Primitive::Number)),
                    ) => Type::Primitive(Primitive::Number),
                    (
                        BinaryOp::LessEquals
                        | BinaryOp::GreaterEquals
                        | BinaryOp::Less
                        | BinaryOp::Greater
                        | BinaryOp::Equals
                        | BinaryOp::NotEquals,
                        (Type::Primitive(Primitive::Number), Type::Primitive(Primitive::Number))
                        | (Type::Primitive(Primitive::Bool), Type::Primitive(Primitive::Bool)),
                    ) => Type::Primitive(Primitive::Bool),
                    _ => {
                        self.errors.push(Error::CantPerformOperation {
                            op: op.0,
                            op_span: op.1,
                            lhs_ty: lhs.ty.clone(),
                            rhs_ty: rhs.ty.clone(),
                            lhs_span: lhs.1,
                            rhs_span: rhs.1,
                        });

                        return None;
                    }
                };

                TypedExpression {
                    expr: Expression::BinaryOp {
                        op,
                        lhs: lhs.boxed(),
                        rhs: rhs.boxed(),
                    },
                    ty,
                }
            }
            ast::Expression::UnaryOp { op, expr } => {
                let expr = expr.map(|expr| self.lower_expression(*expr)).transpose()?;

                let ty = match (op.0, &expr.ty) {
                    (ast::UnaryOp::Neg, Type::Primitive(Primitive::Number)) => {
                        Type::Primitive(Primitive::Number)
                    }
                    (ast::UnaryOp::Not, Type::Primitive(Primitive::Bool)) => {
                        Type::Primitive(Primitive::Bool)
                    }
                    _ => {
                        self.errors.push(Error::CantPerformUnaryOperation {
                            op: op.0,
                            op_span: op.1,
                            ty: expr.ty.clone(),
                            span: expr.1,
                        });

                        return None;
                    }
                };

                TypedExpression {
                    expr: Expression::UnaryOp {
                        op,
                        expr: expr.boxed(),
                    },
                    ty,
                }
            }
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

                match callee.ty.clone() {
                    Type::Function { params, return_ty } => {
                        if args.len() != params.len() {
                            self.errors.push(Error::ArgumentCountMismatch {
                                expected: params.len(),
                                found: args.len(),
                                expected_span: params.1,
                                found_span: args.1,
                            });
                        }

                        for (arg, param) in args.as_ref().0.iter().zip(params.0) {
                            let arg_ty = self.engine.insert_type(&arg.ty, arg.1);
                            let param_ty = self.engine.insert_type(&param.0, param.1);

                            if self.engine.unify(arg_ty, param_ty).is_err() {
                                self.errors.push(Error::ArgumentTypeMismatch {
                                    param_ty: param.0,
                                    arg_ty: arg.ty.clone(),
                                    param_span: param.1,
                                    arg_span: arg.1,
                                });
                            }
                        }

                        TypedExpression {
                            expr: Expression::Call {
                                callee: callee.boxed(),
                                args,
                            },
                            ty: *return_ty.0,
                        }
                    }
                    ty => {
                        self.errors.push(Error::CantCallType { ty, span: callee.1 });

                        return None;
                    }
                }
            }
            ast::Expression::Match { expr, arms } => {
                let expr = expr.map(|expr| self.lower_expression(*expr)).transpose()?;

                let arms = arms
                    .map(|arms| {
                        arms.into_iter()
                            .map(|arm| {
                                arm.map(|arm| {
                                    self.lower_match_arm(
                                        arm,
                                        expr.as_ref().map(|expr| expr.ty.clone()),
                                    )
                                })
                                .transpose()
                            })
                            .collect::<Option<Vec<_>>>()
                    })
                    .transpose()?;

                let ty = self
                    .engine
                    .insert_info(TyInfo::Unknown, expr.1.union(arms.1));

                for arm in &arms.0 {
                    let arm_ty = self.engine.insert_type(&arm.body.ty, arm.body.1);

                    if self.engine.unify(ty, arm_ty).is_err() {
                        let ty = self.engine.solve(ty).ok();

                        self.errors.push(Error::ArmTypeMismatch {
                            arm_ty: arm.body.ty.clone(),
                            ty: ty.as_ref().map(|ty| ty.0.clone()),
                            arm_span: arm.body.1,
                            ty_span: ty.map(|ty| ty.1),
                            whole_span: expr.1.union(arms.1),
                        });
                    }
                }

                TypedExpression {
                    expr: Expression::Match {
                        expr: expr.boxed(),
                        arms,
                    },
                    ty: self.engine.solve(ty).unwrap().0,
                }
            }
        })
    }

    fn lower_match_arm(
        &mut self,
        arm: ast::MatchArm,
        expected_pattern_type: Spanned<Type<Primitive>>,
    ) -> Option<MatchArm> {
        let pattern = arm
            .pattern
            .map(|pattern| {
                let pattern_type = pattern.pattern_type.map(|pattern| match pattern {
                    ast::PatternType::Variable(identifier) => PatternType::Variable(identifier),
                    ast::PatternType::Number(value) => PatternType::Number(value),
                    ast::PatternType::Bool(value) => PatternType::Bool(value),
                });

                match &pattern_type.0 {
                    PatternType::Variable(identifier) => {
                        self.names
                            .insert(identifier.resolve(), expected_pattern_type);
                    }
                    PatternType::Number(_) => todo!(),
                    PatternType::Bool(_) => todo!(),
                }

                let condition = match pattern.condition {
                    Some(condition) => Some(
                        condition
                            .map(|condition| self.lower_expression(condition))
                            .transpose()?,
                    ),
                    None => None,
                };

                Some(Pattern {
                    pattern_type,
                    condition,
                })
            })
            .transpose()?;

        let body = arm
            .body
            .map(|body| self.lower_expression(body))
            .transpose()?;

        Some(MatchArm { pattern, body })
    }

    fn lower_type(&mut self, ty: &Type<Identifier>) -> Option<Type<Primitive>> {
        Some(match ty {
            Type::Primitive(name) => self
                .types
                .get(&name.resolve())
                .or_else(|| {
                    self.errors.push(Error::UndefinedType {
                        name: name.resolve(),
                        span: name.1,
                    });

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

#[derive(Default)]
struct Engine {
    vars: Vec<Spanned<TyInfo>>,
}

impl Engine {
    fn insert_info(&mut self, ty: TyInfo, span: Span) -> TyVar {
        self.vars.push(Spanned::new(ty, span));
        TyVar(self.vars.len() - 1)
    }

    fn insert_type(&mut self, ty: &Type<Primitive>, span: Span) -> TyVar {
        let info = match ty {
            Type::Primitive(primitive) => TyInfo::Primitive(*primitive),
            Type::Tuple(inner) => TyInfo::Tuple(
                inner
                    .iter()
                    .map(|ty| self.insert_type(&ty.0, ty.1))
                    .collect(),
            ),
            Type::Never => TyInfo::Never,
            Type::Function { params, return_ty } => TyInfo::Function(
                params.as_ref().map(|params| {
                    params
                        .iter()
                        .map(|param| self.insert_type(&param.0, param.1))
                        .collect()
                }),
                self.insert_type(&return_ty.0, return_ty.1),
            ),
        };

        self.insert_info(info, span)
    }

    fn unify(&mut self, a: TyVar, b: TyVar) -> Result<(), ()> {
        let a_ty = self.vars[a.0].clone();
        let b_ty = self.vars[b.0].clone();

        match (a_ty.0.clone(), b_ty.0.clone()) {
            (TyInfo::Unknown, _) => {
                self.vars[a.0] = Spanned::new(TyInfo::Ref(b), b_ty.1);
                Ok(())
            }
            (_, TyInfo::Unknown) => {
                self.vars[b.0] = Spanned::new(TyInfo::Ref(a), a_ty.1);
                Ok(())
            }

            (TyInfo::Ref(a), _) => self.unify(a, b),
            (_, TyInfo::Ref(b)) => self.unify(a, b),

            (TyInfo::Primitive(a), TyInfo::Primitive(b)) if a == b => Ok(()),

            (TyInfo::Tuple(inner_a), TyInfo::Tuple(inner_b)) => {
                if inner_a.len() != inner_b.len() {
                    return Err(());
                }

                for (a, b) in inner_a.iter().zip(inner_b.iter()) {
                    self.unify(*a, *b)?;
                }

                Ok(())
            }

            (TyInfo::Never, TyInfo::Never) => Ok(()),

            (TyInfo::Function(params_a, return_a), TyInfo::Function(params_b, return_b)) => {
                if params_a.len() != params_b.len() {
                    return Err(());
                }

                for (a, b) in params_a.iter().zip(params_b.iter()) {
                    self.unify(*a, *b)?;
                }

                self.unify(return_a, return_b)
            }

            _ => Err(()),
        }
    }

    fn solve(&mut self, ty: TyVar) -> Result<Spanned<Type<Primitive>>, Spanned<TyInfo>> {
        let ty = self.vars[ty.0].clone();

        match ty.0 {
            TyInfo::Unknown => Err(ty),
            TyInfo::Ref(ty_var) => self.solve(ty_var),
            TyInfo::Primitive(primitive) => Ok(Spanned::new(Type::Primitive(primitive), ty.1)),
            TyInfo::Tuple(inner) => {
                let inner = inner
                    .into_iter()
                    .map(|ty| self.solve(ty))
                    .collect::<Result<_, _>>()?;

                Ok(Spanned::new(Type::Tuple(inner), ty.1))
            }
            TyInfo::Never => Ok(Spanned::new(Type::Never, ty.1)),
            TyInfo::Function(params, return_ty) => {
                let params = params
                    .map(|params| {
                        params
                            .into_iter()
                            .map(|param| self.solve(param))
                            .collect::<Result<_, _>>()
                    })
                    .transpose_result()?;

                let return_ty = self.solve(return_ty)?;

                Ok(Spanned::new(
                    Type::Function {
                        params,
                        return_ty: return_ty.boxed(),
                    },
                    ty.1,
                ))
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyVar(usize);

#[derive(Clone, Debug)]
enum TyInfo {
    Unknown,
    Ref(TyVar),
    Primitive(Primitive),
    Tuple(Vec<TyVar>),
    Never,
    Function(Spanned<Vec<TyVar>>, TyVar),
}

#[derive(Clone, Debug)]
struct FunctionSignature {
    params: Spanned<Vec<Spanned<FunctionParam>>>,
    return_ty: Spanned<Type<Primitive>>,
}
