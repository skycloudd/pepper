use crate::{span::Span, RODEO};
use chumsky::{input::WithContext, prelude::*};
use tokens::{Delim, Interned, Kw, Punc, Token, TokenTree};

pub mod tokens;

type ParserInput<'src> = WithContext<Span, &'src str>;

type ParserExtra<'src> = extra::Err<Rich<'src, char, Span, &'src str>>;

#[must_use]
pub fn lexer<'src>(
) -> impl Parser<'src, ParserInput<'src>, Vec<tokens::Spanned<TokenTree>>, ParserExtra<'src>> {
    recursive(|tokens| {
        let ident = text::unicode::ident()
            .map(|name| Interned::new(RODEO.get_or_intern(name)))
            .map(Token::Identifier)
            .boxed();

        let bool = choice((text::keyword("true"), text::keyword("false")))
            .to_slice()
            .map(Interned::get_or_intern)
            .map(Token::Boolean)
            .boxed();

        let int = text::int(10)
            .to_slice()
            .map(Interned::get_or_intern)
            .map(Token::Int)
            .boxed();

        let float = text::int(10)
            .then(just('.').then(text::digits(10).or_not()))
            .to_slice()
            .map(Interned::get_or_intern)
            .map(Token::Float)
            .boxed();

        let string = just('"')
            .ignore_then(none_of('"').repeated().to_slice())
            .then_ignore(just('"'))
            .map(Interned::get_or_intern)
            .map(Token::String)
            .boxed();

        let keyword = choice((
            text::keyword("func").to(Kw::Func),
            text::keyword("var").to(Kw::Var),
            text::keyword("for").to(Kw::For),
            text::keyword("in").to(Kw::In),
            text::keyword("match").to(Kw::Match),
            text::keyword("where").to(Kw::Where),
            text::keyword("import").to(Kw::Import),
            text::keyword("struct").to(Kw::Struct),
            text::keyword("enum").to(Kw::Enum),
        ))
        .map(Token::Kw)
        .boxed();

        let punctuation = choice((
            just("->").to(Punc::Arrow),
            just("=>").to(Punc::DoubleArrow),
            just("==").to(Punc::DoubleEquals),
            just("!=").to(Punc::NotEquals),
            just("<=").to(Punc::LessEquals),
            just(">=").to(Punc::GreaterEquals),
            just("..").to(Punc::DoublePeriod),
            just("+").to(Punc::Plus),
            just("-").to(Punc::Minus),
            just("*").to(Punc::Star),
            just("/").to(Punc::Slash),
            just(":").to(Punc::Colon),
            just(",").to(Punc::Comma),
            just(".").to(Punc::Period),
            just("=").to(Punc::Equals),
            just("!").to(Punc::Bang),
            just("<").to(Punc::Less),
            just(">").to(Punc::Greater),
            just(";").to(Punc::Semicolon),
        ))
        .map(Token::Punc)
        .boxed();

        let wildcard = text::ascii::keyword("_").to(Token::Wildcard).boxed();

        let comment = just("//")
            .then(any().and_is(just('\n').not()).repeated())
            .padded()
            .boxed();

        let simple = choice((
            keyword,
            bool,
            ident,
            float,
            int,
            string,
            punctuation,
            wildcard,
        ))
        .map(TokenTree::Token)
        .boxed();

        let parenthesised = tokens
            .clone()
            .delimited_by(just('('), just(')'))
            .map(|tokens| TokenTree::Tree(Delim::Paren, tokens))
            .boxed();

        let curly_braces = tokens
            .clone()
            .delimited_by(just('{'), just('}'))
            .map(|tokens| TokenTree::Tree(Delim::Brace, tokens))
            .boxed();

        let square_brackets = tokens
            .delimited_by(just('['), just(']'))
            .map(|tokens| TokenTree::Tree(Delim::Bracket, tokens))
            .boxed();

        let token = choice((simple, parenthesised, curly_braces, square_brackets))
            .map_with(|token, e| (token, e.span()))
            .boxed();

        token
            .padded_by(comment.clone().repeated())
            .padded()
            .repeated()
            .collect()
            .padded_by(comment.repeated())
            .padded()
            .boxed()
    })
    .then_ignore(end())
    .boxed()
}
