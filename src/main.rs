use camino::Utf8PathBuf;
use chumsky::{input::Input as _, span::Span as _, Parser as _};
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
use span::{FileId, Span};
use std::{fs::read_to_string, process::ExitCode};

pub mod diagnostics;
pub mod lexer;
pub mod parser;
pub mod span;

static RODEO: Lazy<ThreadedRodeo> = Lazy::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    project: Utf8PathBuf,
}

fn main() -> Result<ExitCode, Box<dyn std::error::Error>> {
    let args = Args::parse();

    if !args.project.is_dir() {
        eprintln!("'{}' is not a directory", args.project);
        return Ok(ExitCode::FAILURE);
    }

    let mut files = SimpleFiles::new();
    let mut errors = Vec::new();

    let filename = args.project.join("main.pr");

    let source = read_to_string(&filename).unwrap();

    let file_id = FileId::new(files.add(filename.as_path(), source.to_string()));

    let (tokens, lexer_errors) = lexer::lexer()
        .parse(source.with_context(file_id))
        .into_output_errors();

    errors.extend(lexer_errors.iter().flat_map(|error| convert(error)));

    let (ast, parser_errors) = tokens.as_ref().map_or_else(
        || (None, vec![]),
        |tokens| {
            let eoi = tokens
                .last()
                .map_or_else(|| Span::zero(file_id), |(_, span)| span.to_end());

            parser::parser()
                .parse(tokens.spanned(eoi))
                .into_output_errors()
        },
    );

    errors.extend(parser_errors.iter().flat_map(|error| convert(error)));

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in &errors {
        let diagnostic = report(error);

        term::emit(&mut writer.lock(), &term_config, &files, &diagnostic)?;
    }

    if errors.is_empty() {
        eprintln!("Ast for `{filename}`: {ast:?}");

        Ok(ExitCode::SUCCESS)
    } else {
        Ok(ExitCode::FAILURE)
    }
}
