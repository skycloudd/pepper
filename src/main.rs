use camino::Utf8PathBuf;
use clap::Parser;
use pepper::{
    compile,
    ir::{Diagnostics, FileId, SourceProgram},
};

#[derive(Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() {
    let args = Args::parse();

    let text = std::fs::read_to_string(&args.filename).unwrap();

    let db = pepper::Database::default();

    let source_program = SourceProgram::new(&db, text);
    let filename = FileId::new(&db, args.filename);

    let diagnostics = compile::accumulated::<Diagnostics>(&db, source_program, filename);

    for diagnostic in diagnostics {
        eprintln!("Error: {diagnostic:?}");
    }
}
