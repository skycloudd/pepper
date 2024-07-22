use crate::{
    diagnostics::{self, error::convert},
    lexer,
    parser::{
        self,
        ast::{Ast, Item},
    },
    span::{FileId, Span},
    RODEO,
};
use camino::{Utf8Path, Utf8PathBuf};
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use codespan_reporting::files::SimpleFiles;
use rustc_hash::FxHashMap;
use std::fs::read_to_string;

#[derive(Debug)]
pub struct Project<'a> {
    base_dir: Utf8PathBuf,
    files: &'a mut SimpleFiles<Utf8PathBuf, String>,
    module_to_ast: ModuleAsts,
    errors: Vec<diagnostics::error::Error>,
    queue: Vec<QueueEntry>,
}

pub type ModuleAsts = FxHashMap<ModuleName, Option<Ast>>;

impl<'a> Project<'a> {
    #[must_use]
    pub fn new(base_dir: Utf8PathBuf, files: &'a mut SimpleFiles<Utf8PathBuf, String>) -> Self {
        Self {
            base_dir,
            files,
            module_to_ast: FxHashMap::default(),
            errors: Vec::new(),
            queue: Vec::new(),
        }
    }

    pub fn construct(
        mut self,
        main_file: impl AsRef<Utf8Path>,
    ) -> Result<(ModuleAsts, Vec<diagnostics::error::Error>), Box<dyn std::error::Error>> {
        self.queue
            .push(QueueEntry::new(self.base_dir.join(main_file), None));

        while let Some(entry) = self.queue.pop() {
            let module_name = ModuleName::from_path(&entry.file);

            if let Some(included_by) = entry.included_by {
                if self.module_to_ast.contains_key(&module_name) {
                    self.errors.push(diagnostics::error::Error::Custom {
                        message: format!(
                        "cyclic module dependency detected while trying to include '{}' in '{}'",
                        module_name, included_by.0
                    ),
                        span: included_by.1,
                    });

                    continue;
                }
            }

            let source = read_to_string(&entry.file)?;

            let file_id = FileId::new(self.files.add(entry.file, source));

            self.parse_file(file_id);
        }

        Ok((self.module_to_ast, self.errors))
    }

    fn parse_file(&mut self, file_id: FileId) {
        let file_module_name = ModuleName::from_path(self.path_for_file_id(file_id));
        let source = self.files.get(file_id.0).unwrap().source();

        let (tokens, lexer_errors) = lexer::lexer()
            .parse(source.with_context(file_id))
            .into_output_errors();

        self.errors
            .extend(lexer_errors.iter().flat_map(|error| convert(error)));

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

        self.errors
            .extend(parser_errors.iter().flat_map(|error| convert(error)));

        if let Some(ast) = &ast {
            for item in &ast.0 .0 {
                if let Item::Module(module_name) = item.0 {
                    let module_name = RODEO.resolve(&module_name.0 .0 .0);

                    let module_file = self.base_dir.join(module_name).with_extension("pr");

                    self.queue.push(QueueEntry::new(
                        module_file,
                        Some((file_module_name.clone(), item.1)),
                    ));
                }
            }
        }

        self.module_to_ast
            .insert(ModuleName::from_path(self.path_for_file_id(file_id)), ast);
    }

    fn path_for_file_id(&self, file_id: FileId) -> &Utf8PathBuf {
        self.files.get(file_id.0).unwrap().name()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModuleName(String);

impl ModuleName {
    fn from_path(path: impl AsRef<Utf8Path>) -> Self {
        Self(path.as_ref().file_stem().unwrap().to_owned())
    }
}

impl core::fmt::Display for ModuleName {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
struct QueueEntry {
    file: Utf8PathBuf,
    included_by: Option<(ModuleName, Span)>,
}

impl QueueEntry {
    const fn new(file: Utf8PathBuf, included_by: Option<(ModuleName, Span)>) -> Self {
        Self { file, included_by }
    }
}
