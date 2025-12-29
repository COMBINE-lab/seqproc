use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

pub fn failure(
    msg: String,
    label: (String, SimpleSpan),
    extra_labels: impl IntoIterator<Item = (String, SimpleSpan)>,
    source: String,
) -> ! {
    Report::build(ReportKind::Error, ((), label.1.into_range()))
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(&msg)
        .with_label(
            Label::new(((), label.1.into_range()))
                .with_message(label.0)
                .with_color(Color::Red),
        )
        .with_labels(extra_labels.into_iter().map(|label2| {
            Label::new(((), label2.1.into_range()))
                .with_message(label2.0)
                .with_color(Color::Yellow)
        }))
        .finish()
        .print(Source::from(&source))
        .unwrap();
    std::process::exit(1)
}

pub fn parse_failure(err: &Rich<'_, impl fmt::Display>, src: String) -> ! {
    failure(
        err.to_string(),
        (err.reason().to_string(), *err.span()),
        err.contexts()
            .map(|(l, s)| (format!("while parsing this {l}"), *s)),
        src,
    )
}
