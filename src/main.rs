use camino::Utf8PathBuf;
use clap::Parser;
use owo_colors::OwoColorize as _;
use pepper::{
    compile,
    diagnostics::{error::Error, Diagnostics},
    lexer::tokens::FileId,
    SourceProgram,
};

#[derive(Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() {
    let args = Args::parse();

    let db = pepper::db::Database::default();

    let diagnostics = run_compiler(&db, args.filename);

    for diagnostic in diagnostics {
        eprintln!("Error: {:?}", diagnostic.red());
    }
}

fn run_compiler(db: &dyn pepper::Db, filename: Utf8PathBuf) -> Vec<Error> {
    let text = std::fs::read_to_string(&filename).unwrap();

    let filename = FileId::new(db, filename);
    let source_program = SourceProgram::new(db, text, filename);

    compile::accumulated::<Diagnostics>(db, source_program)
}
