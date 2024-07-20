use crate::span::Span;
use chumsky::{input::WithContext, prelude::*};
use tokens::{Kw, Punc, SimpleToken, Token};

pub mod tokens;

pub fn lexer<'src>() -> impl Parser<
    'src,
    WithContext<Span, &'src str>,
    Vec<tokens::Spanned<Token<'src>>>,
    extra::Err<Rich<'src, char, Span, &'src str>>,
> {
    recursive(|tokens| {
        let ident = text::ascii::ident().map(SimpleToken::Identifier).boxed();

        let bool = choice((
            text::keyword("true").to(true),
            text::keyword("false").to(false),
        ))
        .map(SimpleToken::Boolean)
        .boxed();

        let integer = text::int(10).to_slice().map(SimpleToken::Integer).boxed();

        let float = text::int(10)
            .then_ignore(just('.'))
            .then(text::digits(10))
            .to_slice()
            .map(SimpleToken::Float)
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
            just("::").to(Punc::ColonColon),
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
