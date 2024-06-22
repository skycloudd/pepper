use crate::{
    diagnostics::{error::convert, Diagnostics},
    lexer::{
        self,
        tokens::{Kw, Punc, Simple, Span, TokenKind},
    },
    SourceProgram,
};
use ast::{
    BinaryOp, BinaryOpKind, Expression, ExpressionData, Function, FunctionId, FunctionParameter,
    Program, UnaryOp, UnaryOpKind, VariableId,
};
use chumsky::{input::SpannedInput, prelude::*};

pub mod ast;

#[salsa::tracked]
pub fn parse(db: &dyn crate::Db, source_program: SourceProgram) -> Option<Program<'_>> {
    let tokens = lexer::lex(db, source_program)?;

    let eoi = tokens
        .tokens(db)
        .last()
        .map_or_else(|| Span::zero(tokens.file_id(db)), |t| t.1.to_end());

    let tokens = tokens.tokens(db);

    let (program, errors) = parser(db).parse(tokens.spanned(eoi)).into_output_errors();

    for err in errors
        .into_iter()
        .flat_map(|err| convert(&err.map_token(|token| token.to_string())))
    {
        Diagnostics::push(db, err);
    }

    program
}

type ParserInput<'tok> = SpannedInput<TokenKind, Span, &'tok [(TokenKind, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, TokenKind, Span, &'src str>>;

#[must_use]
fn parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Program<'db>, ParserExtra<'src, 'tok>> {
    function_parser(db)
        .labelled("function")
        .repeated()
        .collect()
        .map(|functions| Program::new(db, functions))
        .boxed()
}

fn function_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Function<'db>, ParserExtra<'src, 'tok>> {
    let name = function_id_parser(db).map_with(|name, e| (name, e.span()));

    let params = parenthesised(comma_separated_list(
        variable_id_parser(db)
            .map_with(|name, e| (name, e.span()))
            .labelled("parameter name")
            .then_ignore(just(TokenKind::Simple(Simple::Punc(Punc::Colon))))
            .then(
                type_parser()
                    .map_with(|type_, e| (type_, e.span()))
                    .labelled("parameter type"),
            )
            .map(
                |((name, name_span), (type_, type_span))| FunctionParameter {
                    name,
                    name_span,
                    type_,
                    type_span,
                },
            )
            .labelled("function parameter"),
    ))
    .map_with(|params, e| (params, e.span()));

    let return_type = type_parser().map_with(|return_type, e| (return_type, e.span()));

    let body = expr_parser(db);

    just(TokenKind::Simple(Simple::Kw(Kw::Fn)))
        .ignore_then(name.labelled("function name"))
        .then(params.labelled("function parameters"))
        .then_ignore(just(TokenKind::Simple(Simple::Punc(Punc::Arrow))))
        .then(return_type.labelled("return type"))
        .then_ignore(just(TokenKind::Simple(Simple::Punc(Punc::Equals))))
        .then(body.labelled("function body"))
        .map(
            |(
                (((name, name_span), (params, params_span)), (return_type, return_type_span)),
                body,
            )| {
                Function::new(
                    db,
                    name,
                    name_span,
                    return_type,
                    return_type_span,
                    params,
                    params_span,
                    body,
                )
            },
        )
        .boxed()
}

fn expr_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Expression<'db>, ParserExtra<'src, 'tok>> {
    recursive(|expr| {
        let variable = variable_id_parser(db)
            .map_with(|id, e| Expression {
                span: e.span(),
                data: ExpressionData::Variable(id),
            })
            .labelled("variable")
            .boxed();

        let integer = select! {
            TokenKind::Simple(Simple::Integer(i)) => i,
        }
        .map_with(|value, e| Expression {
            span: e.span(),
            data: ExpressionData::Integer(value),
        })
        .labelled("integer")
        .boxed();

        let float = select! {
            TokenKind::Simple(Simple::Float(f)) => f,
        }
        .map_with(|value, e| Expression {
            span: e.span(),
            data: ExpressionData::Float(value),
        })
        .labelled("float")
        .boxed();

        let parenthesised_expr = parenthesised(expr.clone()).labelled("parenthesised expression");

        let function_call = function_id_parser(db)
            .then(parenthesised(comma_separated_list(expr)))
            .map_with(|(name, args), e| Expression {
                span: e.span(),
                data: ExpressionData::Call(name, args),
            })
            .labelled("function call")
            .boxed();

        let atom = choice((function_call, variable, integer, float, parenthesised_expr)).boxed();

        let unary = unary_op!(atom, (Punc::Minus => UnaryOpKind::Negate));

        let factor = binary_op!(unary, (Punc::Star => BinaryOpKind::Multiply), (Punc::Slash => BinaryOpKind::Divide));

        let sum = binary_op!(factor, (Punc::Plus => BinaryOpKind::Add), (Punc::Minus => BinaryOpKind::Subtract));

        sum.labelled("expression")
    })
}

macro_rules! unary_op {
    ($base:expr, $(($punc:expr => $to:expr)),*) => {{
        let ops = choice((
            $(
                just(TokenKind::Simple(Simple::Punc($punc))).to($to),
            )*
        ))
        .map_with(|op, e| (op, e.span()))
        .boxed();

        ops
            .repeated()
            .foldr($base, |op, expr| {
                let span = op.1.union(expr.span.clone());

                let op = UnaryOp {
                    span: op.1,
                    data: op.0,
                };

                Expression {
                    span,
                    data: ExpressionData::UnaryOp(op, Box::new(expr)),
                }
            })
            .boxed()
    }};
}
use unary_op;

macro_rules! binary_op {
    ($base:expr, $(($punc:expr => $to:expr)),*) => {{
        let ops = choice((
            $(
                just(TokenKind::Simple(Simple::Punc($punc))).to($to),
            )*
        ))
        .map_with(|op, e| (op, e.span()))
        .boxed();

        $base
            .clone()
            .foldl(ops.then($base).repeated(), |lhs, (op, rhs)| {
                let span = lhs.span.union(rhs.span.clone());

                let op = BinaryOp {
                    span: op.1,
                    data: op.0,
                };

                Expression {
                    span,
                    data: ExpressionData::BinaryOp(op, Box::new(lhs), Box::new(rhs)),
                }
            })
            .boxed()
    }};
}
use binary_op;

fn parenthesised<'src: 'tok, 'tok, T>(
    inner: impl Parser<'tok, ParserInput<'tok>, T, ParserExtra<'src, 'tok>>,
) -> impl Parser<'tok, ParserInput<'tok>, T, ParserExtra<'src, 'tok>> {
    inner.nested_in(select_ref! {
        TokenKind::Parentheses(tokens) = e => tokens.as_slice().spanned(e.span()),
    })
}

fn comma_separated_list<'src: 'tok, 'tok, T>(
    inner: impl Parser<'tok, ParserInput<'tok>, T, ParserExtra<'src, 'tok>>,
) -> impl Parser<'tok, ParserInput<'tok>, Vec<T>, ParserExtra<'src, 'tok>> {
    inner
        .separated_by(just(TokenKind::Simple(Simple::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
}

fn function_id_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, FunctionId<'db>, ParserExtra<'src, 'tok>> {
    select! {
        TokenKind::Simple(Simple::Ident(ident)) => FunctionId::new(db, ident),
    }
    .boxed()
}

fn variable_id_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, VariableId<'db>, ParserExtra<'src, 'tok>> {
    select! {
        TokenKind::Simple(Simple::Ident(ident)) => VariableId::new(db, ident),
    }
    .boxed()
}

fn type_parser<'db: 'tok, 'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, ast::Type, ParserExtra<'src, 'tok>> {
    select! {
        TokenKind::Simple(Simple::Ident(ident)) if ident == "int" => ast::Type::Integer,
        TokenKind::Simple(Simple::Ident(ident)) if ident == "float" => ast::Type::Float,
    }
    .boxed()
}
