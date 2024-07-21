use crate::{
    lexer::tokens::{Kw, Punc, SimpleToken, Token},
    span::{Span, Spanned},
    RODEO,
};
use ast::{
    Ast, BinaryOp, Expression, Function, FunctionParam, Identifier, Item, Path, PrimitiveType,
    Statement, Struct, StructField, Type, UnaryOp,
};
use chumsky::{extra, input::SpannedInput, prelude::*};

pub mod ast;

type ParserInput<'src, 'tok> = SpannedInput<Token<'src>, Span, &'tok [(Token<'src>, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, Token<'src>, Span, &'src str>>;

pub fn parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Ast, ParserExtra<'src, 'tok>> {
    item_parser()
        .with_span()
        .repeated()
        .collect()
        .with_span()
        .map(Ast)
        .boxed()
}

fn item_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Item, ParserExtra<'src, 'tok>> {
    choice((
        function_parser().with_span().map(Item::Function),
        struct_parser().with_span().map(Item::Struct),
    ))
    .boxed()
}

fn function_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Function, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span();

    let params = function_param_parser()
        .with_span()
        .repeated()
        .collect()
        .parenthesized()
        .with_span();

    let body_return_expr = statement_parser()
        .with_span()
        .repeated()
        .collect()
        .with_span()
        .then(expression_parser().with_span().or_not())
        .curly_braced()
        .map(|(body, return_expr)| (body, return_expr));

    just(Token::Simple(SimpleToken::Kw(Kw::Func)))
        .ignore_then(name)
        .then(params)
        .then(
            just(Token::Simple(SimpleToken::Punc(Punc::Arrow)))
                .ignore_then(type_parser().with_span())
                .or_not(),
        )
        .then(body_return_expr)
        .map(
            |(((name, params), return_ty), (body, return_expr))| Function {
                name,
                params,
                return_ty,
                body,
                return_expr,
            },
        )
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
        let expr = expression_parser()
            .with_span()
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Semicolon))))
            .map(Statement::Expression)
            .boxed();

        let block = statement
            .clone()
            .with_span()
            .repeated()
            .collect()
            .curly_braced()
            .with_span()
            .map(Statement::Block)
            .boxed();

        let let_ = just(Token::Simple(SimpleToken::Kw(Kw::Let)))
            .ignore_then(ident_parser().with_span())
            .then(
                just(Token::Simple(SimpleToken::Punc(Punc::Colon)))
                    .ignore_then(type_parser().with_span())
                    .or_not(),
            )
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Equals))))
            .then(expression_parser().with_span())
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Semicolon))))
            .map(|((name, ty), value)| Statement::Let { name, ty, value })
            .boxed();

        let assign = ident_parser()
            .with_span()
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Equals))))
            .then(expression_parser().with_span())
            .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Semicolon))))
            .map(|(name, value)| Statement::Assign { name, value })
            .boxed();

        choice((expr, block, let_, assign)).boxed()
    })
    .boxed()
}

fn expression_parser<'src: 'tok, 'tok>(
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
            Token::Simple(SimpleToken::Integer(int)) => int.parse().unwrap(),
        }
        .with_span()
        .map(Expression::Integer)
        .boxed();

        let float = select! {
            Token::Simple(SimpleToken::Float(float)) => float.parse().unwrap(),
        }
        .with_span()
        .map(Expression::Float)
        .boxed();

        let bool = select! {
            Token::Simple(SimpleToken::Boolean(bool)) => bool,
        }
        .with_span()
        .map(Expression::Bool)
        .boxed();

        let variable = ident_parser().with_span().map(Expression::Variable).boxed();

        let call_args = expression
            .with_span()
            .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .parenthesized()
            .with_span()
            .boxed();

        let call = path_parser()
            .with_span()
            .then(call_args)
            .map(|(name, args)| Expression::Call { name, args })
            .boxed();

        let atom = choice((call, integer, float, bool, variable)).boxed();

        let unary = unary_op!(atom, (Punc::Minus => UnaryOp::Neg)).boxed();

        let factor =
            binary_op!(unary, (Punc::Star => BinaryOp::Mul), (Punc::Slash => BinaryOp::Div))
                .boxed();

        binary_op!(factor, (Punc::Plus => BinaryOp::Add), (Punc::Minus => BinaryOp::Sub)).boxed()
    })
    .boxed()
}

fn struct_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Struct, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span();

    let fields = struct_field_parser()
        .with_span()
        .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .curly_braced()
        .with_span();

    just(Token::Simple(SimpleToken::Kw(Kw::Struct)))
        .ignore_then(name)
        .then(fields)
        .map(|(name, fields)| Struct { name, fields })
        .boxed()
}

fn struct_field_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, StructField, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .then_ignore(just(Token::Simple(SimpleToken::Punc(Punc::Colon))))
        .then(type_parser().with_span())
        .map(|(name, ty)| StructField { name, ty })
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
    macro_rules! prim {
        ($ident:literal, $variant:ident) => {
            just(Token::Simple(SimpleToken::Identifier($ident))).to(PrimitiveType::$variant)
        };
    }

    let primitive = choice((
        prim!("int8", Int8),
        prim!("int16", Int16),
        prim!("int32", Int32),
        prim!("int64", Int64),
        prim!("uint8", Uint8),
        prim!("uint16", Uint16),
        prim!("uint32", Uint32),
        prim!("uint64", Uint64),
        prim!("float32", Float32),
        prim!("float64", Float64),
        prim!("bool", Bool),
    ))
    .with_span()
    .map(Type::Primitive)
    .boxed();

    let user = path_parser().with_span().map(Type::User).boxed();

    choice((primitive, user)).boxed()
}

fn path_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'src, 'tok>, Path, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .separated_by(just(Token::Simple(SimpleToken::Punc(Punc::ColonColon))))
        .collect()
        .with_span()
        .map(Path)
        .boxed()
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
