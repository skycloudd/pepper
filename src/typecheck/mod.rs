use crate::{
    diagnostics,
    hir::Module,
    parser::ast::{Ast, Function, Path, Struct},
    scopes::Scopes,
    span::Spanned,
};
use lasso::Spur;

pub mod typed;

#[derive(Debug, Default)]
pub struct Typechecker {
    errors: Vec<diagnostics::error::Error>,
}

impl Typechecker {
    #[must_use]
    pub const fn new() -> Self {
        Self { errors: vec![] }
    }

    #[must_use]
    pub fn typecheck(
        mut self,
        module: Module<Ast>,
    ) -> (Module<typed::Ast>, Vec<diagnostics::error::Error>) {
        let typed_module = self.typecheck_module(module);

        (typed_module, self.errors)
    }

    fn typecheck_module(&mut self, module: Module<Ast>) -> Module<typed::Ast> {
        Module {
            id: module.id,
            name: module.name,
            ast: self.typecheck_ast(module.ast, &module.children),
            children: module
                .children
                .into_iter()
                .map(|module| self.typecheck_module(module))
                .collect(),
        }
    }

    fn typecheck_ast(&mut self, ast: Ast, module_children: &[Module<Ast>]) -> typed::Ast {
        let mut in_scope = InScope::new();

        self.fill_scope(&mut in_scope, ast, module_children);

        typed::Ast {}
    }

    fn fill_scope(&mut self, in_scope: &mut InScope, ast: Ast, module_children: &[Module<Ast>]) {
        for struct_ in ast.structs {
            in_scope.insert_type(struct_.0.name.0 .0 .0, struct_);
        }

        for function in ast.functions {
            in_scope.insert_function(function.0.name.0 .0 .0, function);
        }

        for module_stmt in ast.module_stmts {
            let module_name = module_stmt.0 .0 .0 .0 .0;

            let module = module_children
                .iter()
                .find(|module| module.name.0 == module_name)
                .cloned();

            if let Some(module) = module {
                in_scope.insert_module(module_name, module);
            }
        }

        for use_stmt in ast.use_stmts {
            let thing = Self::traverse_path(&use_stmt.0 .0 .0, in_scope);

            if let Some(thing) = thing {
                match thing {
                    InScopeItem::Struct(struct_) => {
                        in_scope.insert_type(struct_.0.name.0 .0 .0, struct_);
                    }
                    InScopeItem::Function(function) => {
                        in_scope.insert_function(function.0.name.0 .0 .0, function);
                    }
                    InScopeItem::Module(module) => {
                        in_scope.insert_module(module.name.0, module);
                    }
                }
            } else {
                self.errors.push(diagnostics::error::Error::Custom {
                    message: "item can't be found".to_string(),
                    span: use_stmt.0 .0 .1,
                });
            }
        }
    }

    fn traverse_path(path: &Path, in_scope: &InScope) -> Option<InScopeItem> {
        let mut current_item = None;

        #[allow(clippy::option_if_let_else, clippy::single_match_else)]
        for segment in &path.0 .0 {
            current_item = match current_item {
                Some(item) => match item {
                    InScopeItem::Struct(_struct) => {
                        todo!("error: cannot traverse path from struct");
                    }
                    InScopeItem::Function(_function) => {
                        todo!("error: cannot traverse path from function");
                    }
                    InScopeItem::Module(module_) => (|| {
                        if let Some(submodule) = module_
                            .children
                            .into_iter()
                            .find(|module| module.name.0 == segment.0 .0 .0)
                            .map(InScopeItem::Module)
                        {
                            return Some(submodule);
                        }

                        for struct_ in module_.ast.structs {
                            if struct_.0.name.0 .0 .0 == segment.0 .0 .0 {
                                return Some(InScopeItem::Struct(struct_));
                            }
                        }

                        for function in module_.ast.functions {
                            if function.0.name.0 .0 .0 == segment.0 .0 .0 {
                                return Some(InScopeItem::Function(function));
                            }
                        }

                        None
                    })(),
                },
                None => {
                    let name = segment.0 .0 .0;

                    #[allow(clippy::manual_map)]
                    if let Some(type_) = in_scope.types.get(&name) {
                        Some(InScopeItem::Struct(type_.clone()))
                    } else if let Some(function) = in_scope.functions.get(&name) {
                        Some(InScopeItem::Function(function.clone()))
                    } else if let Some(module) = in_scope.modules.get(&name) {
                        Some(InScopeItem::Module(module.clone()))
                    } else {
                        None
                    }
                }
            };

            if current_item.is_none() {
                break;
            }
        }

        current_item
    }
}

#[derive(Debug)]
struct InScope {
    types: Scopes<Spur, Spanned<Struct>>,
    functions: Scopes<Spur, Spanned<Function>>,
    modules: Scopes<Spur, Module<Ast>>,
}

impl InScope {
    fn new() -> Self {
        Self {
            types: Scopes::default(),
            functions: Scopes::default(),
            modules: Scopes::default(),
        }
    }

    fn insert_type(&mut self, name: Spur, struct_: Spanned<Struct>) {
        if self.types.contains_key(&name) || self.modules.contains_key(&name) {
            todo!("error: type already in scope");
        }

        self.types.insert(name, struct_);
    }

    fn insert_function(&mut self, name: Spur, function: Spanned<Function>) {
        if self.functions.contains_key(&name) {
            todo!("error: function already in scope");
        }

        self.functions.insert(name, function);
    }

    fn insert_module(&mut self, name: Spur, module: Module<Ast>) {
        if self.types.contains_key(&name) || self.modules.contains_key(&name) {
            todo!("error: module already in scope");
        }

        self.modules.insert(name, module);
    }
}

#[derive(Debug)]
enum InScopeItem {
    Struct(Spanned<Struct>),
    Function(Spanned<Function>),
    Module(Module<Ast>),
}
