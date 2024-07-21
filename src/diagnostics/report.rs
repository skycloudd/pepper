use super::{Diag, ErrorSpan};
use chumsky::span::Span as _;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn report(diagnostic: &dyn Diag) -> Diagnostic<usize> {
    Diagnostic::new(diagnostic.kind())
        .with_message(diagnostic.message())
        .with_labels(
            diagnostic
                .spans()
                .into_iter()
                .map(|span| {
                    let (mut label, message) = match span {
                        ErrorSpan::Primary(message, span) => (
                            Label::primary(span.context().0, span.start()..span.end()),
                            message,
                        ),
                        ErrorSpan::Secondary(message, span) => (
                            Label::secondary(span.context().0, span.start()..span.end()),
                            message,
                        ),
                    };

                    if let Some(message) = message {
                        label = label.with_message(message);
                    }

                    label
                })
                .collect(),
        )
        .with_notes(diagnostic.notes())
}
