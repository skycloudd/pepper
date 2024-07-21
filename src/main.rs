use camino::Utf8PathBuf;
use chumsky::{input::Input as _, span::Span, Parser as _};
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use diagnostics::{error::convert, report::report};
use lasso::ThreadedRodeo;
use once_cell::sync::Lazy;
use parser::ast::Ast;
use span::FileId;
use std::fs::read_to_string;

pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod span;

static RODEO: Lazy<ThreadedRodeo> = Lazy::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let mut files = SimpleFiles::new();

    let mut errors = Vec::new();

    let source = read_to_string(&args.filename).unwrap();

    let file_id = FileId::new(files.add(&args.filename, source.to_string()));

    let (ast, parse_errors) = parse_file(&source, file_id);

    errors.extend(parse_errors);

    if let Some(ast) = ast {
        eprintln!("{ast:?}");
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in errors {
        let diagnostic = report(&error);

        term::emit(&mut writer.lock(), &term_config, &files, &diagnostic)?;
    }

    Ok(())
}

fn parse_file(source: &str, file_id: FileId) -> (Option<Ast>, Vec<diagnostics::error::Error>) {
    let mut errors = Vec::new();

    let (tokens, lexer_errors) = lexer::lexer()
        .parse(source.with_context(file_id))
        .into_output_errors();

    errors.extend(lexer_errors.iter().flat_map(|error| convert(error)));

    let (ast, parser_errors) = tokens.as_ref().map_or_else(
        || (None, vec![]),
        |tokens| {
            let eoi = tokens.last().unwrap().1.to_end();

            parser::parser()
                .parse(tokens.spanned(eoi))
                .into_output_errors()
        },
    );

    errors.extend(parser_errors.iter().flat_map(|error| convert(error)));

    (ast, errors)
}
