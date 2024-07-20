use super::{Diag, ErrorSpan};
use chumsky::span::Span as _;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn report(diagnostic: &impl Diag) -> Diagnostic<usize> {
    codespan_reporting::diagnostic::Diagnostic::<usize>::new(diagnostic.kind())
        .with_message(diagnostic.message())
        .with_labels(
            diagnostic
                .spans()
                .into_iter()
                .map(|span| match span {
                    ErrorSpan::Primary(message, span) => {
                        let mut label =
                            Label::primary(span.context().id(), span.start()..span.end());

                        if let Some(message) = message {
                            label = label.with_message(message);
                        }

                        label
                    }
                    ErrorSpan::Secondary(message, span) => {
                        let mut label =
                            Label::secondary(span.context().id(), span.start()..span.end());

                        if let Some(message) = message {
                            label = label.with_message(message);
                        }

                        label
                    }
                })
                .collect(),
        )
        .with_notes(diagnostic.notes())
}
