use crate::{
    lexer::tokens::{Delim, Interned, Kw, Punc, Token, TokenTree},
    span::{Span, Spanned},
};
use ast::{
    Ast, AstType, BinaryOp, Block, Expression, Function, FunctionParam, Item, ListPattern,
    MatchArm, Path, Pattern, PatternType, Statement, Type, UnaryOp,
};
use chumsky::{extra, input::SpannedInput, prelude::*};

pub mod ast;

type ParserInput<'tok> = SpannedInput<TokenTree, Span, &'tok [(TokenTree, Span)]>;

type ParserExtra<'src, 'tok> = extra::Err<Rich<'tok, TokenTree, Span, &'src str>>;

#[must_use]
pub fn parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Ast, ParserExtra<'src, 'tok>> {
    item_parser()
        .with_span()
        .repeated()
        .collect()
        .map(Ast)
        .boxed()
}

fn item_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Item, ParserExtra<'src, 'tok>> {
    let function = function_parser().with_span().map(Item::Function).boxed();

    let import = just(TokenTree::Token(Token::Kw(Kw::Import)))
        .ignore_then(path_parser().with_span())
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Semicolon))))
        .map(Item::Import)
        .boxed();

    choice((function, import)).boxed()
}

fn function_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Function, ParserExtra<'src, 'tok>> {
    let name = ident_parser().with_span();

    let params = function_param_parser()
        .with_span()
        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .delim(Delim::Paren)
        .with_span()
        .boxed()
        .labelled("function parameters");

    let return_ty = just(TokenTree::Token(Token::Punc(Punc::Arrow)))
        .ignore_then(type_parser().with_span())
        .or_not()
        .boxed()
        .labelled("return type");

    let body = block_parser(expression_parser())
        .with_span()
        .labelled("function body");

    just(TokenTree::Token(Token::Kw(Kw::Func)))
        .ignore_then(name)
        .then(params)
        .then(return_ty)
        .then(body)
        .map(|(((name, params), return_ty), body)| Function {
            name,
            params,
            return_ty,
            body,
        })
        .boxed()
        .labelled("function")
}

fn function_param_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, FunctionParam, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Colon))))
        .then(type_parser().with_span())
        .map(|(name, ty)| FunctionParam { name, ty })
        .boxed()
        .labelled("function parameter")
}

fn statement_parser<'src: 'tok, 'tok>(
    expression: impl Parser<'tok, ParserInput<'tok>, Expression, ParserExtra<'src, 'tok>> + Clone + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Statement, ParserExtra<'src, 'tok>> {
    let expr = expression
        .clone()
        .with_span()
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Semicolon))))
        .map(Statement::Expression)
        .boxed();

    let var_decl = just(TokenTree::Token(Token::Kw(Kw::Var)))
        .ignore_then(ident_parser().with_span())
        .then(
            just(TokenTree::Token(Token::Punc(Punc::Colon)))
                .ignore_then(type_parser().with_span())
                .or_not(),
        )
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Equals))))
        .then(expression.with_span())
        .then_ignore(just(TokenTree::Token(Token::Punc(Punc::Semicolon))))
        .map(|((name, ty), value)| Statement::VarDecl { name, ty, value })
        .boxed();

    choice((expr, var_decl)).boxed()
}

fn block_parser<'src: 'tok, 'tok>(
    expression: impl Parser<'tok, ParserInput<'tok>, Expression, ParserExtra<'src, 'tok>> + Clone + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Block, ParserExtra<'src, 'tok>> {
    statement_parser(expression.clone())
        .with_span()
        .repeated()
        .collect()
        .then(expression.with_span().or_not())
        .delim(Delim::Brace)
        .map(|(statements, return_expr)| Block {
            statements,
            return_expr,
        })
        .boxed()
}

macro_rules! unary_op {
    ($base:expr, $($punc:expr => $to:expr),* $(,)?) => {{
        let ops = choice((
            $(
                just(TokenTree::Token(Token::Punc($punc))).to($to),
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
    ($base:expr, $($punc:expr => $to:expr),* $(,)?) => {{
        let ops = choice((
            $(
                just(TokenTree::Token(Token::Punc($punc))).to($to),
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

fn expression_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Expression, ParserExtra<'src, 'tok>> + Clone {
    recursive(|expression| {
        let int = int_parser()
            .with_span()
            .map(Expression::Int)
            .boxed()
            .labelled("integer");

        let float = float_parser()
            .with_span()
            .map(Expression::Float)
            .boxed()
            .labelled("float");

        let bool = bool_parser()
            .with_span()
            .map(Expression::Bool)
            .boxed()
            .labelled("boolean");

        let string = string_parser()
            .with_span()
            .map(Expression::String)
            .boxed()
            .labelled("string");

        let name = path_parser()
            .with_span()
            .map(Expression::Name)
            .boxed()
            .labelled("name");

        let block = block_parser(expression.clone())
            .map(Box::new)
            .with_span()
            .map(Expression::Block)
            .boxed()
            .labelled("block");

        let tuple = tuple_parser(expression.clone())
            .with_span()
            .map(Expression::Tuple)
            .boxed()
            .labelled("tuple");

        let list = expression
            .clone()
            .with_span()
            .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .delim(Delim::Bracket)
            .with_span()
            .map(Expression::List)
            .boxed();

        let pattern = pattern_parser(expression.clone())
            .with_span()
            .boxed()
            .labelled("pattern");

        let match_ = {
            let match_arm = pattern
                .clone()
                .then_ignore(just(TokenTree::Token(Token::Punc(Punc::DoubleArrow))))
                .then(expression.clone().with_span())
                .map(|(pattern, body)| MatchArm { pattern, body })
                .with_span()
                .boxed()
                .labelled("match arm");

            just(TokenTree::Token(Token::Kw(Kw::Match)))
                .ignore_then(expression.clone().map(Box::new).with_span())
                .then(
                    match_arm
                        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
                        .allow_trailing()
                        .collect()
                        .delim(Delim::Brace)
                        .with_span(),
                )
                .map(|(expr, arms)| Expression::Match { expr, arms })
                .boxed()
                .labelled("match expression")
        };

        let for_ = just(TokenTree::Token(Token::Kw(Kw::For)))
            .ignore_then(pattern)
            .then_ignore(just(TokenTree::Token(Token::Kw(Kw::In))))
            .then(expression.clone().map(Box::new).with_span())
            .then(block_parser(expression.clone()).map(Box::new).with_span())
            .map(|((pattern, iter), body)| Expression::For {
                pattern: pattern.boxed(),
                iter,
                body,
            })
            .boxed()
            .labelled("for loop");

        let parenthesized = expression
            .clone()
            .with_span()
            .delim(Delim::Paren)
            .map(|expr| expr.0)
            .boxed()
            .labelled("parenthesized expression");

        let atom = choice((
            parenthesized,
            block,
            tuple,
            list,
            match_,
            for_,
            int,
            float,
            bool,
            string,
            name,
        ))
        .boxed();

        let call = {
            let call_args = expression
                .clone()
                .with_span()
                .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
                .allow_trailing()
                .collect()
                .delim(Delim::Paren)
                .with_span()
                .boxed()
                .labelled("call arguments");

            atom.with_span()
                .foldl(call_args.repeated(), |callee, args| {
                    let span = callee.1.union(args.1);

                    Spanned::new(
                        Expression::Call {
                            callee: callee.boxed(),
                            args,
                        },
                        span,
                    )
                })
                .map(|expr| expr.0)
                .boxed()
                .labelled("function call")
        };

        let unary = unary_op!(
            call,
            Punc::Minus => UnaryOp::Neg,
            Punc::Bang => UnaryOp::Not,
        )
        .boxed()
        .labelled("unary expression");

        let equality = binary_op!(
            unary,
            Punc::DoubleEquals => BinaryOp::Equals,
            Punc::NotEquals => BinaryOp::NotEquals,
        )
        .boxed();

        let relational = binary_op!(
            equality,
            Punc::LessEquals => BinaryOp::LessEquals,
            Punc::GreaterEquals => BinaryOp::GreaterEquals,
            Punc::Less => BinaryOp::Less,
            Punc::Greater => BinaryOp::Greater,
        )
        .boxed();

        let factor = binary_op!(
            relational,
            Punc::Star => BinaryOp::Mul,
            Punc::Slash => BinaryOp::Div,
        )
        .boxed();

        binary_op!(
            factor,
            Punc::Plus => BinaryOp::Add,
            Punc::Minus => BinaryOp::Sub,
        )
        .boxed()
    })
    .boxed()
    .labelled("expression")
}

fn pattern_parser<'src: 'tok, 'tok>(
    expression: impl Parser<'tok, ParserInput<'tok>, Expression, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Pattern, ParserExtra<'src, 'tok>> {
    recursive(|pattern| {
        pattern_type_parser(pattern)
            .with_span()
            .then(
                just(TokenTree::Token(Token::Kw(Kw::Where)))
                    .ignore_then(expression.with_span())
                    .or_not(),
            )
            .map(|(pattern_type, condition)| Pattern {
                pattern_type,
                condition,
            })
            .boxed()
    })
}

fn pattern_type_parser<'src: 'tok, 'tok>(
    pattern: impl Parser<'tok, ParserInput<'tok>, Pattern, ParserExtra<'src, 'tok>> + Clone + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, PatternType, ParserExtra<'src, 'tok>> {
    let list_pattern = {
        let list_pattern = choice((
            pattern.clone().with_span().map(ListPattern::Pattern),
            just(TokenTree::Token(Token::Punc(Punc::DoublePeriod))).to(ListPattern::Rest),
        ))
        .boxed();

        list_pattern
            .with_span()
            .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
            .allow_trailing()
            .collect()
            .delim(Delim::Bracket)
            .with_span()
            .map(PatternType::List)
            .boxed()
    };

    choice((
        just(TokenTree::Token(Token::Wildcard)).to(PatternType::Wildcard),
        ident_parser().with_span().map(PatternType::Variable),
        int_parser().with_span().map(PatternType::Int),
        float_parser().with_span().map(PatternType::Float),
        bool_parser().with_span().map(PatternType::Bool),
        string_parser().with_span().map(PatternType::String),
        tuple_parser(pattern).with_span().map(PatternType::Tuple),
        list_pattern,
    ))
    .boxed()
}

macro_rules! interned_parser {
    ($name:ident, $token:ident) => {
        fn $name<'src: 'tok, 'tok>(
        ) -> impl Parser<'tok, ParserInput<'tok>, Interned, ParserExtra<'src, 'tok>> {
            select! {
                TokenTree::Token(Token::$token(ident)) => ident
            }
        }
    };
}

interned_parser!(int_parser, Int);
interned_parser!(float_parser, Float);
interned_parser!(bool_parser, Boolean);
interned_parser!(string_parser, String);
interned_parser!(ident_parser, Identifier);

fn path_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Path, ParserExtra<'src, 'tok>> {
    ident_parser()
        .with_span()
        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Period))))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|segments| {
            let (base, segments) = segments.split_first().unwrap();

            Path {
                base: *base,
                segments: segments.to_vec(),
            }
        })
        .boxed()
}

fn type_parser<'src: 'tok, 'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, AstType, ParserExtra<'src, 'tok>> {
    recursive(|type_| {
        let prim = path_parser()
            .with_span()
            .map(Type::Primitive)
            .boxed()
            .labelled("primitive type");

        let tuple = tuple_parser(type_.clone())
            .with_span()
            .map(Type::Tuple)
            .labelled("tuple type")
            .boxed();

        let never = just(TokenTree::Token(Token::Punc(Punc::Bang)))
            .to(Type::Never)
            .boxed()
            .labelled("never type");

        let function = {
            let params = type_
                .clone()
                .with_span()
                .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
                .allow_trailing()
                .collect()
                .delim(Delim::Paren)
                .with_span()
                .boxed();

            let return_ty = just(TokenTree::Token(Token::Punc(Punc::Arrow)))
                .ignore_then(type_.with_span())
                .boxed();

            just(TokenTree::Token(Token::Kw(Kw::Func)))
                .ignore_then(params)
                .then(return_ty)
                .boxed()
                .map(|(params, return_ty)| Type::Function {
                    params,
                    return_ty: return_ty.boxed(),
                })
                .boxed()
                .labelled("function type")
        };

        choice((prim, tuple, never, function))
            .boxed()
            .labelled("type")
    })
}

fn tuple_parser<'src: 'tok, 'tok, Inner: 'tok>(
    inner: impl Parser<'tok, ParserInput<'tok>, Inner, ParserExtra<'src, 'tok>> + 'tok,
) -> impl Parser<'tok, ParserInput<'tok>, Vec<Spanned<Inner>>, ParserExtra<'src, 'tok>> {
    inner
        .with_span()
        .separated_by(just(TokenTree::Token(Token::Punc(Punc::Comma))))
        .allow_trailing()
        .collect()
        .delim(Delim::Paren)
        .boxed()
}

trait SpannedExt<'src: 'tok, 'tok, O> {
    fn with_span(self)
        -> impl Parser<'tok, ParserInput<'tok>, Spanned<O>, ParserExtra<'src, 'tok>>;

    fn delim(
        self,
        delim: Delim,
    ) -> impl Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>>;
}

impl<'src: 'tok, 'tok, P, O> SpannedExt<'src, 'tok, O> for P
where
    P: Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>>,
{
    fn with_span(
        self,
    ) -> impl Parser<'tok, ParserInput<'tok>, Spanned<O>, ParserExtra<'src, 'tok>> {
        self.map_with(|t, e| Spanned::new(t, e.span()))
    }

    fn delim(
        self,
        delim: Delim,
    ) -> impl Parser<'tok, ParserInput<'tok>, O, ParserExtra<'src, 'tok>> {
        self.nested_in(select_ref! {
            TokenTree::Tree(d, tokens) = e if *d == delim => tokens.as_slice().spanned(e.span()),
        })
    }
}
