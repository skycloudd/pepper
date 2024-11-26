use camino::Utf8PathBuf;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use pepper::{
    diagnostics::{error::Error, report::report},
    span::FileId,
};
use std::{fs::read_to_string, process::ExitCode};

#[derive(Debug, Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut files = SimpleFiles::new();

    let source = read_to_string(&args.filename).unwrap();
    let file_id = FileId::new(files.add(args.filename, source.clone()));

    let (ast, errors) = pepper::parse_file(file_id, source);

    eprintln!("ast: {ast:#?}");

    if errors.is_empty() {
        ExitCode::SUCCESS
    } else {
        emit_errors(&errors, &files);
        ExitCode::FAILURE
    }
}

fn emit_errors(errors: &[Error], files: &SimpleFiles<Utf8PathBuf, String>) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in errors {
        let diagnostic = report(error);

        term::emit(&mut writer.lock(), &term_config, files, &diagnostic).unwrap();
    }
}
