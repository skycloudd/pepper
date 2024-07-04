use camino::Utf8PathBuf;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use core::{fmt::Write as _, str::FromStr as _};
use owo_colors::OwoColorize as _;
use pepper::{
    compile,
    diagnostics::{report::report, Diagnostics},
    lexer::tokens::FileId,
    SourceProgram,
};
use serde::Deserialize;
use std::io::Write as _;
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt as _;
use target_lexicon::Triple;

#[derive(Parser)]
struct Args {
    filename: Utf8PathBuf,

    #[clap(short, long)]
    out: Utf8PathBuf,

    #[clap(short, long)]
    target: Option<String>,

    #[clap(short, long)]
    config: Option<Utf8PathBuf>,
}

#[derive(Debug, Deserialize)]
struct Config {
    target: Target,
}

#[derive(Debug, Deserialize)]
struct Target {
    #[serde(rename = "aarch64-apple-darwin")]
    aarch64_apple_darwin: Aarch64AppleDarwin,
}

#[derive(Debug, Deserialize)]
struct Aarch64AppleDarwin {
    linker: Linker,
}

#[derive(Debug, Deserialize)]
struct Linker {
    command: String,
    flags: Vec<String>,
}

trait TargetConfig {
    fn linker(&self) -> &Linker;
}

impl TargetConfig for Aarch64AppleDarwin {
    fn linker(&self) -> &Linker {
        &self.linker
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let pepper_config: Config = toml::from_str(&std::fs::read_to_string(
        args.config
            .as_deref()
            .unwrap_or_else(|| "pepper_config.toml".into()),
    )?)?;

    let target_triple = args
        .target
        .as_deref()
        .map_or_else(|| Ok(Triple::host()), Triple::from_str)
        .map_err(|err| format!("invalid target triple: {err}"))?;

    let db = pepper::db::Database::default();

    let mut files = SimpleFiles::new();

    let text = std::fs::read_to_string(&args.filename)?;

    let id = files.add(args.filename, text.clone());
    let file_id = FileId::new(&db, id);

    let source_program = SourceProgram::new(&db, text, file_id);

    let diagnostics =
        compile::accumulated::<Diagnostics>(&db, source_program, target_triple.clone());

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let term_config = term::Config::default();

    for error in &diagnostics {
        let diag = report(&db, error);

        term::emit(&mut writer.lock(), &term_config, &files, &diag)?;
    }

    if diagnostics.is_empty() {
        let target_config = match target_triple.to_string().as_str() {
            "aarch64-apple-darwin" => {
                &pepper_config.target.aarch64_apple_darwin as &dyn TargetConfig
            }
            _ => return Err(format!("unsupported target triple: {target_triple}").into()),
        };

        let linker_config = target_config.linker();

        let object_file = args.out.with_extension("o");
        let program_file = args.out;

        let output = compile::get(&db, source_program, target_triple)
            .as_ref()
            .unwrap();

        let mut output_file = std::fs::File::create(&object_file)?;
        output_file.write_all(output)?;

        println!("Compilation successful");

        println!(
            "Linking with: {}",
            format!(
                "{} {object_file} -o {program_file}{}",
                linker_config.command,
                linker_config
                    .flags
                    .iter()
                    .fold(String::new(), |mut init, flag| {
                        write!(init, " {flag}").unwrap();
                        init
                    }),
            )
            .green()
        );

        let status = std::process::Command::new(&linker_config.command)
            .arg(object_file)
            .arg("-o")
            .arg(&program_file)
            .args(&linker_config.flags)
            .status()?;

        if !status.success() {
            return Err(format!(
                "{} exited with {}",
                linker_config.command,
                status.code().map_or_else(
                    #[cfg(unix)]
                    || format!("signal {}", status.signal().unwrap()),
                    #[cfg(not(unix))]
                    || "unknown exit code".to_string(),
                    |code| format!("code {code}")
                )
            )
            .into());
        }
    }

    Ok(())
}
