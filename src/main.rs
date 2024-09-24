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
use span::{FileId, Span};
use std::{fs::read_to_string, process::ExitCode, sync::LazyLock};

mod diagnostics;
mod lexer;
mod parser;
mod scopes;
mod span;
mod typecheck;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> Result<ExitCode, Box<dyn core::error::Error>> {
    let args = Args::parse();

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

    let (typed_ast, typecheck_errors) = ast.map_or_else(|| (None, vec![]), typecheck::typecheck);

    errors.extend(typecheck_errors);

    if let Some(typed_ast) = typed_ast {
        if cfg!(debug_assertions) {
            eprintln!("{:?}", typed_ast.functions);
        }
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in &errors {
        let diagnostic = report(error);

        term::emit(&mut writer.lock(), &term_config, &files, &diagnostic)?;
    }

    if errors.is_empty() {
        Ok(ExitCode::SUCCESS)
    } else {
        Ok(ExitCode::FAILURE)
    }
}
