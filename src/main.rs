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
use lower::mir::Mir;
use span::{FileId, Span};
use std::{fs::read_to_string, process::ExitCode, sync::LazyLock};

pub mod diagnostics;
mod lexer;
mod lower;
mod parser;
pub mod scopes;
pub mod span;
mod typecheck;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> Result<ExitCode, Box<dyn core::error::Error>> {
    let args = Args::parse();

    run_mir(&args)?.map_or_else(
        || Ok(ExitCode::FAILURE),
        |mir| {
            println!("{mir:#?}");
            Ok(ExitCode::SUCCESS)
        },
    )
}

fn run_mir(args: &Args) -> Result<Option<Mir>, Box<dyn core::error::Error>> {
    if !args.filename.is_file() {
        return Err(format!("'{}' is not a file", args.filename).into());
    }

    let mut files = SimpleFiles::new();

    let source = read_to_string(&args.filename)?;

    let file_id = FileId::new(files.add(&args.filename, source.to_string()));

    let mut errors = vec![];

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

    let (typed_ast, typecheck_errors) = ast.map_or_else(
        || (None, vec![]),
        |ast| {
            let (typed_ast, errors) = typecheck::typecheck(ast);
            (Some(typed_ast), errors)
        },
    );

    errors.extend(typecheck_errors);

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in &errors {
        let diagnostic = report(error);

        term::emit(&mut writer.lock(), &term_config, &files, &diagnostic)?;
    }

    if errors.is_empty() {
        let mir = lower::lower(typed_ast.unwrap());

        Ok(Some(mir))
    } else {
        Ok(None)
    }
}
