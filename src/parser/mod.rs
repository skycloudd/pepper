use crate::{
    diagnostics::{error::convert, Diagnostics},
    lexer::{
        self,
        tokens::{Kw, Punc, Simple, Span, TokenKind},
    },
    SourceProgram,
};
use ast::{
    BinaryOp, Expression, ExpressionData, Function, FunctionId, Program, UnaryOp, VariableId,
};
use chumsky::{input::SpannedInput, prelude::*};

pub mod ast;

#[salsa::tracked]
pub fn parse(db: &dyn crate::Db, source_program: SourceProgram) -> Option<Program<'_>> {
    let tokens = lexer::lex(db, source_program);

    tokens.and_then(|tokens| {
        let eoi = tokens
            .tokens(db)
            .last()
            .map_or_else(|| Span::new(tokens.file_id(db), 0..0), |t| t.1.to_end());

        let tokens = tokens.tokens(db);

        let (program, errors) = parser(db).parse(tokens.spanned(eoi)).into_output_errors();

        for err in errors
            .into_iter()
            .flat_map(|err| convert(&err.map_token(|token| token.to_string())))
        {
            Diagnostics::push(db, err);
        }

        program
    })
}

type ParserInput<'tok> = SpannedInput<TokenKind, Span, &'tok [(TokenKind, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, TokenKind, Span, &'src str>>;

#[must_use]
fn parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Program<'db>, ParserExtra<'src, 'tok>> {
    function_parser(db)
        .repeated()
        .collect()
        .map(|functions| Program::new(db, functions))
        .boxed()
}

fn function_parser<'db: 'tok, 'src: 'tok, 'tok>(
    db: &'db dyn crate::Db,
) -> impl Parser<'tok, ParserInput<'tok>, Function<'db>, ParserExtra<'src, 'tok>> {
    let name = function_id_parser(db).map_with(|name, e| (name, e.span()));

    let args = parenthesised(comma_separated_list(variable_id_parser(db)));

    let body = expr_parser(db);

    just(TokenKind::Simple(Simple::Kw(Kw::Fn)))
        .ignore_then(name)
        .then(args)
        .then_ignore(just(TokenKind::Simple(Simple::Punc(Punc::Equals))))
        .then(body)
        .map(|(((name, name_span), args), body)| Function::new(db, name, name_span, args, body))
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
            .boxed();

        let integer = select! {
            TokenKind::Simple(Simple::Integer(i)) => i,
        }
        .map_with(|value, e| Expression {
            span: e.span(),
            data: ExpressionData::Integer(value),
        })
        .boxed();

        let float = select! {
            TokenKind::Simple(Simple::Float(f)) => f,
        }
        .map_with(|value, e| Expression {
            span: e.span(),
            data: ExpressionData::Float(value),
        })
        .boxed();

        let parenthesised_expr = parenthesised(expr.clone());

        let function_call = function_id_parser(db)
            .then(parenthesised(comma_separated_list(expr)))
            .map_with(|(name, args), e| Expression {
                span: e.span(),
                data: ExpressionData::Call(name, args),
            })
            .boxed();

        let atom = choice((variable, integer, float, parenthesised_expr, function_call)).boxed();

        let unary = unary_op!(atom, (Punc::Minus => UnaryOp::Negate));

        let factor = binary_op!(unary, (Punc::Star => BinaryOp::Multiply), (Punc::Slash => BinaryOp::Divide));

        binary_op!(factor, (Punc::Plus => BinaryOp::Add), (Punc::Minus => BinaryOp::Subtract))
    })
}

macro_rules! unary_op {
    ($base:expr, $(($punc:expr => $to:expr)),*) => {{
        let ops = choice((
            $(
                just(TokenKind::Simple(Simple::Punc($punc))).to($to),
            )*
        ))
        .map_with(|op, e| (op, e.span()));

        ops
            .repeated()
            .foldr($base, |op, expr| {
                let span = op.1.union(expr.span.clone());

                Expression {
                    span,
                    data: ExpressionData::UnaryOp(op.0, Box::new(expr)),
                }
            })
    }};
}
use unary_op;

macro_rules! binary_op {
    ($base:expr, $(($punc:expr => $to:expr)),*) => {{
        let ops = choice((
            $(
                just(TokenKind::Simple(Simple::Punc($punc))).to($to),
            )*
        ));

        $base
            .clone()
            .foldl(ops.then($base).repeated(), |lhs, (op, rhs)| {
                let span = lhs.span.union(rhs.span.clone());

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
