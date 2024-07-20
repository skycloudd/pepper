use camino::{Utf8Path, Utf8PathBuf};
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
use span::FileId;
use std::fs::read_to_string;

mod diagnostics;
mod lexer;
mod parser;
mod span;

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    println!("{args:?}");

    let mut files = SimpleFiles::new();

    let source = read_to_string(&args.filename)?;

    let errors = parse_file(&args.filename, &source, &mut files);

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in errors {
        let diagnostic = report(&error);

        term::emit(&mut writer.lock(), &term_config, &files, &diagnostic)?;
    }

    Ok(())
}

type Name<'path> = &'path Utf8Path;
type Source<'src> = &'src str;

fn parse_file<'path, 'src>(
    name: Name<'path>,
    source: Source<'src>,
    files: &mut SimpleFiles<Name<'path>, Source<'src>>,
) -> Vec<diagnostics::error::Error> {
    let file_id = FileId::new(files.add(name, source));

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

    eprintln!("{ast:?}");

    errors
}
