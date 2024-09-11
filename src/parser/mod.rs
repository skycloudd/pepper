use crate::{
    lexer::tokens::{Kw, Punc, SimpleToken, Token},
    span::{Span, Spanned},
    RODEO,
};
use ast::{
    Ast, BinaryOp, Block, Expression, Function, FunctionParam, Identifier, Statement, Type, UnaryOp,
};
use chumsky::{extra, input::SpannedInput, prelude::*};
use ordered_float::OrderedFloat;

pub mod ast;

type ParserInput<'src, 'tok> = SpannedInput<Token<'src>, Span, &'tok [(Token<'src>, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, Token<'src>, Span, &'src str>>;

#[must_use]
pub fn parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Ast, ParserExtra<'src, 'tok>> {
    function_parser()
        .with_span()
        .repeated()
        .collect()
        .map(|functions| Ast { functions })
        .boxed()
}

fn function_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Function, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span();

    let params = function_param_parser()
        .with_span()
        .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .parenthesized()
        .with_span();

    let body = block_parser(statement_parser(), expression_parser(statement_parser())).with_span();

    just(Token::Simple(SimpleToken::Kw(Kw::Func)))
        .ignore_then(name)
        .then(params)
        .then(
            just(Token::Simple(SimpleToken::Punc(Punc::Arrow)))
                .ignore_then(type_parser().with_span())
                .or_not(),
        )
        .then(body)
        .map(|(((name, params), return_ty), body)| Function {
            name,
            params,
            return_ty,
            body,
        })
        .boxed()
}

fn function_param_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, FunctionParam, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Colon))))
        .then(type_parser().with_span())
        .map(|(name, ty)| FunctionParam { name, ty })
        .boxed()
}

fn statement_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Statement, ParserExtra<'src, 'tok>> {
    recursive(|statement| {
        let expr = expression_parser(statement.clone())
            .with_span()
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Semicolon))))
            .map(Statement::Expression)
            .boxed();

        let let_ = just(Token::Simple(SimpleToken::Kw(Kw::Let)))
            .ignore_then(ident_parser().with_span())
            .then(
                just(Token::Simple(SimpleToken::Punc(Punc::Colon)))
                    .ignore_then(type_parser().with_span())
                    .or_not(),
            )
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Equals))))
            .then(expression_parser(statement.clone()).with_span())
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Semicolon))))
            .map(|((name, ty), value)| Statement::Let { name, ty, value })
            .boxed();

        let assign = ident_parser()
            .with_span()
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Equals))))
            .then(expression_parser(statement).with_span())
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Semicolon))))
            .map(|(name, value)| Statement::Assign { name, value })
            .boxed();

        choice((expr, let_, assign)).boxed()
    })
    .boxed()
}

#[allow(clippy::too_many_lines)]
fn expression_parser<'src: 'tok, 'tok>(
    statement: impl Parser<'tok, ParserInput<'src, 'tok>, Statement, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Expression, ParserExtra<'src, 'tok>> {
    macro_rules! unary_op {
        ($base:expr, $(($punc:expr => $to:expr)),*) => {{
            let ops = choice((
                $(
                    just(Token::Simple(SimpleToken::Punc($punc))).to($to),
                )*
            ))
            .with_span()
            .boxed();

            ops
                .repeated()
                .foldr($base.with_span(), |op, expr| {
                    let span = op.1.union(expr.1);

                    Spanned::new(
                        Expression::UnaryOp {
                            op,
                            expr: expr.boxed(),
                        },
                        span
                    )
                })
                .map(|expr| expr.0)
                .boxed()
        }};
    }

    macro_rules! binary_op {
        ($base:expr, $(($punc:expr => $to:expr)),*) => {{
            let ops = choice((
                $(
                    just(Token::Simple(SimpleToken::Punc($punc))).to($to),
                )*
            ))
            .with_span()
            .boxed();

            $base
                .clone()
                .with_span()
                .foldl(ops.then($base.with_span()).repeated(), |lhs, (op, rhs)| {
                    let span = lhs.1.union(rhs.1);

                    Spanned::new(
                        Expression::BinaryOp {
                            op,
                            lhs: lhs.boxed(),
                            rhs: rhs.boxed(),
                        },
                        span
                    )
                })
                .map(|expr| expr.0)
                .boxed()
        }};
    }

    recursive(|expression| {
        let integer = select! {
            Token::Simple(SimpleToken::Integer { value, ty }) => (value, ty),
        }
        .map(|(value, ty)| {
            let value = Spanned(value.0.parse().unwrap(), value.1);

            let ty = ty.map(|ty| {
                let span = ty.1;

                Spanned(
                    Type(Spanned(
                        Identifier(Spanned(RODEO.get_or_intern(ty.0), span)),
                        span,
                    )),
                    span,
                )
            });

            Expression::Integer(value, ty)
        })
        .boxed();

        let float = select! {
            Token::Simple(SimpleToken::Float { value, ty }) => (value, ty),
        }
        .map(|(value, ty)| {
            let value = Spanned(OrderedFloat(value.0.parse().unwrap()), value.1);

            let ty = ty.map(|ty| {
                let span = ty.1;

                Spanned(
                    Type(Spanned(
                        Identifier(Spanned(RODEO.get_or_intern(ty.0), span)),
                        span,
                    )),
                    span,
                )
            });

            Expression::Float(value, ty)
        })
        .boxed();

        let bool = select! {
            Token::Simple(SimpleToken::Boolean(bool)) => bool,
        }
        .with_span()
        .map(Expression::Bool)
        .boxed();

        let variable = ident_parser().with_span().map(Expression::Variable).boxed();

        let call_args = expression
            .clone()
            .with_span()
            .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .parenthesized()
            .with_span()
            .boxed();

        let call = ident_parser()
            .with_span()
            .then(call_args)
            .map(|(name, args)| Expression::Call { name, args })
            .boxed();

        let parenthesized = expression
            .clone()
            .with_span()
            .parenthesized()
            .map(|expr| expr.0)
            .boxed();

        let block = block_parser(statement, expression)
            .with_span()
            .map(|block| Expression::Block(block.boxed()))
            .boxed();

        let atom = choice((parenthesized, block, call, integer, float, bool, variable)).boxed();

        let unary = unary_op!(atom, (Punc::Minus => UnaryOp::Neg)).boxed();

        let factor =
            binary_op!(unary, (Punc::Star => BinaryOp::Mul), (Punc::Slash => BinaryOp::Div))
                .boxed();

        binary_op!(factor, (Punc::Plus => BinaryOp::Add), (Punc::Minus => BinaryOp::Sub)).boxed()
    })
    .boxed()
}

fn block_parser<'src: 'tok, 'tok>(
    statement: impl Parser<'tok, ParserInput<'src, 'tok>, Statement, ParserExtra<'src, 'tok>> + 'tok,
    expression: impl Parser<'tok, ParserInput<'src, 'tok>, Expression, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Block, ParserExtra<'src, 'tok>> {
    statement
        .with_span()
        .repeated()
        .collect()
        .with_span()
        .then(expression.with_span().or_not())
        .curly_braced()
        .map(|(statements, return_expr)| Block {
            statements,
            return_expr,
        })
        .boxed()
}

fn ident_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Identifier, ParserExtra<'src, 'tok>> {
    select! {
        Token::Simple(SimpleToken::Identifier(ident)) = e => Identifier(Spanned::new(RODEO.get_or_intern(ident), e.span())),
    }
    .boxed()
}

fn type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Type, ParserExtra<'src, 'tok>> {
    ident_parser().with_span().map(Type).boxed()
}

trait SpannedExt<'src: 'tok, 'tok, O> {
    fn with_span(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, Spanned<O>, ParserExtra<'src, 'tok>>;

    fn parenthesized(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>>;

    fn curly_braced(self)
        -> impl Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>>;
}

impl<'src: 'tok, 'tok, P, O> SpannedExt<'src, 'tok, O> for P
where
    P: Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>>,
{
    fn with_span(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, Spanned<O>, ParserExtra<'src, 'tok>> {
        self.map_with(|t, e| Spanned::new(t, e.span()))
    }

    fn parenthesized(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>> {
        self.nested_in(select_ref! {
            Token::Parentheses(tokens) = e => tokens.as_slice().spanned(e.span()),
        })
    }

    fn curly_braced(
        self,
    ) -> impl Parser<'tok, ParserInput<'src, 'tok>, O, ParserExtra<'src, 'tok>> {
        self.nested_in(select_ref! {
            Token::CurlyBraces(tokens) = e => tokens.as_slice().spanned(e.span()),
        })
    }
}
