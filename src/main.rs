use camino::{Utf8Path, Utf8PathBuf};
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use diagnostics::{
    error::{convert, Error},
    report::report,
};
use lasso::ThreadedRodeo;
use parser::ast::Ast;
use span::{FileId, Span};
use std::{fs::read_to_string, process::ExitCode, sync::LazyLock};

pub mod diagnostics;
mod lexer;
mod parser;
pub mod scopes;
pub mod span;
mod typecheck;

static RODEO: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut files = SimpleFiles::new();

    let (ast, mut errors) = parse_file(&args.filename, &mut files);

    let (typed_ast, typecheck_errors) = ast.map_or_else(
        || (None, vec![]),
        |ast| {
            let (typed_ast, errs) = typecheck::typecheck(ast);

            (Some(typed_ast), errs)
        },
    );

    errors.extend(typecheck_errors);

    if errors.is_empty() {
        println!("{typed_ast:#?}");
        ExitCode::SUCCESS
    } else {
        emit_errors(&errors, &files);
        ExitCode::FAILURE
    }
}

fn parse_file<'path>(
    filename: &'path Utf8Path,
    files: &mut SimpleFiles<&'path Utf8Path, String>,
) -> (Option<Ast>, Vec<Error>) {
    let source = read_to_string(filename).unwrap();

    let file_id = FileId::new(files.add(filename, source.clone()));

    let (tokens, lexer_errors) = lexer::lexer()
        .parse(source.with_context(file_id))
        .into_output_errors();

    let mut errors = lexer_errors
        .iter()
        .flat_map(|error| convert(error))
        .collect::<Vec<_>>();

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

    (ast, errors)
}

fn emit_errors(errors: &[Error], files: &SimpleFiles<&Utf8Path, String>) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in errors {
        let diagnostic = report(error);

        term::emit(&mut writer.lock(), &term_config, files, &diagnostic).unwrap();
    }
}
