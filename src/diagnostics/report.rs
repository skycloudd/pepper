use super::Diag;
use chumsky::span::Span as _;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn report(diagnostic: &dyn Diag) -> Diagnostic<usize> {
    Diagnostic::new(diagnostic.kind())
        .with_message(diagnostic.message())
        .with_labels(
            diagnostic
                .spans()
                .into_iter()
                .map(|error_span| {
                    let mut label = Label::new(
                        error_span.label_style,
                        error_span.span.context().get(),
                        error_span.span.range(),
                    );

                    if let Some(message) = error_span.message {
                        label = label.with_message(message);
                    }

                    label
                })
                .collect(),
        )
        .with_notes(diagnostic.notes())
}
