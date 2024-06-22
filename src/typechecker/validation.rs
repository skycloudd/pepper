use super::typed_ast::TypedProgram;
use crate::{
    diagnostics::{error::Error, Diagnostics},
    parser::ast::{FunctionId, Type},
    typechecker::find_function_typed,
};
use rustc_hash::FxHashSet;

#[salsa::tracked]
pub fn validate_main_function<'db>(db: &'db dyn crate::Db, program: TypedProgram<'db>) {
    match find_function_typed(db, program, FunctionId::new(db, "main".to_string())) {
        Some(main_function) => {
            if main_function.return_type(db) != Type::Integer {
                Diagnostics::push(
                    db,
                    Error::ExpectedMainReturnType {
                        expected: Type::Integer,
                        expected_span: main_function.return_type_span(db),
                        found: main_function.return_type(db),
                        found_span: main_function.return_type_span(db),
                    },
                );
            }

            if !main_function.params(db).is_empty() {
                Diagnostics::push(
                    db,
                    Error::ExpectedMainNoParameters {
                        found: main_function.params(db).len(),
                        found_span: main_function.params_span(db),
                    },
                );
            }
        }
        None => Diagnostics::push(db, Error::MainFunctionNotFound),
    }
}

#[salsa::tracked]
pub fn validate_unique_function_names<'db>(db: &'db dyn crate::Db, program: TypedProgram<'db>) {
    let mut seen = FxHashSet::default();

    for new_function in program.functions(db) {
        match seen.get(new_function) {
            None => {
                seen.insert(*new_function);
            }
            Some(seen_function) => {
                Diagnostics::push(
                    db,
                    Error::DuplicateFunction {
                        name: new_function.name(db).text(db).to_owned(),
                        new_span: new_function.name_span(db),
                        first_span: seen_function.name_span(db),
                    },
                );
            }
        }
    }
}
