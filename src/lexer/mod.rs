use crate::span::Span;
use chumsky::{input::WithContext, prelude::*};
use tokens::{Kw, Punc, SimpleToken, Token};

pub mod tokens;

type ParserInput<'src> = WithContext<Span, &'src str>;

type ParserExtra<'src> = extra::Err<Rich<'src, char, Span, &'src str>>;

#[must_use]
pub fn lexer<'src>(
) -> impl Parser<'src, ParserInput<'src>, Vec<tokens::Spanned<Token<'src>>>, ParserExtra<'src>> {
    recursive(|tokens| {
        let ident = text::ascii::ident().map(SimpleToken::Identifier).boxed();

        let bool = choice((
            text::keyword("true").to(true),
            text::keyword("false").to(false),
        ))
        .map(SimpleToken::Boolean)
        .boxed();

        let integer = text::int(10)
            .to_slice()
            .with_span()
            .then(ident.clone().to_slice().with_span().or_not())
            .map(|(value, ty)| SimpleToken::Integer {
                value,
                ty: ty.map(remove_prefix_underscores),
            })
            .boxed();

        let float = text::int(10)
            .then_ignore(just('.'))
            .then(text::digits(10).or_not())
            .to_slice()
            .with_span()
            .then(ident.clone().to_slice().with_span().or_not())
            .map(|(value, ty)| SimpleToken::Float {
                value,
                ty: ty.map(remove_prefix_underscores),
            })
            .boxed();

        let keyword = choice((
            text::keyword("func").to(Kw::Func),
            text::keyword("struct").to(Kw::Struct),
            text::keyword("let").to(Kw::Let),
        ))
        .map(SimpleToken::Kw)
        .boxed();

        let punctuation = choice((
            just("->").to(Punc::Arrow),
            just('+').to(Punc::Plus),
            just('-').to(Punc::Minus),
            just('*').to(Punc::Star),
            just('/').to(Punc::Slash),
            just(":").to(Punc::Colon),
            just(",").to(Punc::Comma),
            just("=").to(Punc::Equals),
            just(";").to(Punc::Semicolon),
        ))
        .map(SimpleToken::Punc)
        .boxed();

        let comment = just("//")
            .then(any().and_is(just('\n').not()).repeated())
            .padded()
            .boxed();

        let simple = choice((keyword, bool, ident, float, integer, punctuation))
            .map(Token::Simple)
            .boxed();

        let parenthesised = tokens
            .clone()
            .delimited_by(just('('), just(')'))
            .map(Token::Parentheses)
            .boxed();

        let curly_braces = tokens
            .delimited_by(just('{'), just('}'))
            .map(Token::CurlyBraces)
            .boxed();

        let token = choice((simple, parenthesised, curly_braces))
            .map_with(|token, e| (token, e.span()))
            .padded_by(comment.repeated())
            .padded()
            .boxed();

        token.repeated().collect().padded().boxed()
    })
    .then_ignore(end())
    .boxed()
}

fn remove_prefix_underscores(ident: (&str, Span)) -> (&str, Span) {
    let (ident, span) = ident;

    ident
        .strip_prefix('_')
        .map_or((ident, span), |ident| (ident, span.without_start(1)))
}

trait SpannedExt<'src, O> {
    fn with_span(self) -> impl Parser<'src, ParserInput<'src>, (O, Span), ParserExtra<'src>>;
}

impl<'src, P, O> SpannedExt<'src, O> for P
where
    P: Parser<'src, ParserInput<'src>, O, ParserExtra<'src>>,
{
    fn with_span(self) -> impl Parser<'src, ParserInput<'src>, (O, Span), ParserExtra<'src>> {
        self.map_with(|t, e| (t, e.span()))
    }
}
