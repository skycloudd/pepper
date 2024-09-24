use crate::span::Span;
use chumsky::{input::WithContext, prelude::*};
use tokens::{FractionalPart, Kw, Punc, SimpleToken, Token};

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

        let number = text::int(10)
            .then(
                just('.')
                    .ignore_then(text::digits(10).to_slice().or_not())
                    .or_not()
                    .map(|frac| match frac {
                        Some(Some(frac)) => FractionalPart::Full(frac),
                        Some(None) => FractionalPart::Period,
                        None => FractionalPart::None,
                    }),
            )
            .map(|(int, frac)| SimpleToken::Number(int, frac))
            .boxed();

        let keyword = choice((text::keyword("let").to(Kw::Let),))
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
            just("#").to(Punc::Hash),
            just("!").to(Punc::Bang),
        ))
        .map(SimpleToken::Punc)
        .boxed();

        let comment = just("//")
            .then(any().and_is(just('\n').not()).repeated())
            .padded()
            .boxed();

        let simple = choice((keyword, bool, ident, number, punctuation))
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
