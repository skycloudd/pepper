use crate::{
    diagnostics::error::convert,
    lexer,
    parser::{self, ast::Ast},
    span::{FileId, Span},
    RODEO,
};
use camino::{Utf8Path, Utf8PathBuf};
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use codespan_reporting::files::SimpleFiles;
use lasso::Spur;
use std::fs::read_to_string;

#[derive(Clone)]
pub struct Module<T> {
    pub id: ModuleId,
    pub name: ModuleName,
    pub ast: T,
    pub children: Vec<Module<T>>,
}

impl<T> core::fmt::Debug for Module<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Module")
            .field("id", &self.id.0)
            .field("name", &RODEO.resolve(&self.name.0))
            .field("ast", &"<ast>")
            .field("children", &self.children)
            .finish()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleId(usize);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleName(pub Spur);

#[derive(Debug)]
pub struct ModuleTree<'a> {
    files: &'a mut SimpleFiles<Utf8PathBuf, String>,
    errors: Errors,
    next_module_id: ModuleId,
}

impl<'a> ModuleTree<'a> {
    pub fn new(files: &'a mut SimpleFiles<Utf8PathBuf, String>) -> Self {
        Self {
            files,
            errors: Errors::default(),
            next_module_id: ModuleId(1),
        }
    }

    pub fn build(mut self, root_dir: impl AsRef<Utf8Path>) -> (Option<Module<Ast>>, Errors) {
        let main_file = root_dir.as_ref().join("main.pr");

        let module = self.build_module(main_file, ModuleName(RODEO.get_or_intern("main")));

        (module, self.errors)
    }

    fn build_module(
        &mut self,
        main_file: impl AsRef<Utf8Path>,
        module_name: ModuleName,
    ) -> Option<Module<Ast>> {
        let module_id = self.next_module_id();
        let main_ast = self.build_file(main_file.as_ref());

        main_ast.map(|main_ast| {
            let module_dir = main_file.as_ref().parent().unwrap();

            let mut children = Vec::new();

            for module_stmt in &main_ast.module_stmts {
                let child_module_name = ModuleName(module_stmt.0 .0 .0 .0 .0);

                let (mod_file, mod_file_exists) =
                    check_module_files(CheckModule::File, module_dir, child_module_name);

                let (mod_dir_file, mod_dir_file_exists) =
                    check_module_files(CheckModule::Dir, module_dir, child_module_name);

                let child_mod_file = match (mod_file_exists, mod_dir_file_exists) {
                    (true, false) => mod_file,
                    (false, true) => mod_dir_file,
                    (true, true) => {
                        todo!("error: module found in both file and directory");
                    }
                    (false, false) => {
                        todo!("error: module not found");
                    }
                };

                let child = self.build_module(child_mod_file, child_module_name);

                if let Some(child) = child {
                    children.push(child);
                }
            }

            Module {
                id: module_id,
                name: module_name,
                ast: main_ast,
                children,
            }
        })
    }

    fn build_file(&mut self, filename: impl AsRef<Utf8Path>) -> Option<Ast> {
        let source = read_to_string(filename.as_ref()).unwrap();

        let file_id = FileId::new(
            self.files
                .add(filename.as_ref().to_owned(), source.to_string()),
        );

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

        ast
    }

    fn next_module_id(&mut self) -> ModuleId {
        let id = self.next_module_id;
        self.next_module_id.0 += 1;
        id
    }
}

pub type Errors = Vec<crate::diagnostics::error::Error>;

#[derive(Clone, Copy, Debug)]
enum CheckModule {
    File,
    Dir,
}

fn check_module_files(
    check: CheckModule,
    module_dir: impl AsRef<Utf8Path>,
    mod_name: ModuleName,
) -> (Utf8PathBuf, bool) {
    let mod_name = RODEO.resolve(&mod_name.0);

    let file = module_dir.as_ref().join(format!("{mod_name}.pr"));
    let dir = module_dir.as_ref().join(mod_name).join("module.pr");

    let (file_exists, dir_exists) = (
        file.exists() && file.is_file(),
        dir.exists() && dir.is_file(),
    );

    match check {
        CheckModule::File => (file, file_exists),
        CheckModule::Dir => (dir, dir_exists),
    }
}
