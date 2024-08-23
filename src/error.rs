use std::ops::Range;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{error::SimpleReason, prelude::*};

use crate::lexer::Token;

pub fn handle_errors(errs: Vec<Simple<String>>, source: String) {
    // error recovery
    errs.into_iter().for_each(|e| {
        let report = Report::build(ReportKind::Error, (), e.span().start);

        let report = match e.reason() {
            chumsky::error::SimpleReason::Custom(msg) => report
                .with_message("Parsing and Compiling EFGDL")
                .with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(e.span())
                        .with_message(format!(
                            "Must be closed before this {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Unexpected => {
                report.with_message(format!(
                    "{}, expected {}",
                    if e.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if e.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
            }
            .with_label(
                Label::new(e.span())
                    .with_message(format!(
                        "Unexpected token {}",
                        e.found()
                            .unwrap_or(&"end of file".to_string())
                            .fg(Color::Red)
                    ))
                    .with_color(Color::Red),
            ),
        };

        report.finish().print(Source::from(source.clone())).unwrap();
    });
}

pub fn missing_delimiter(token: Token, span: Range<usize>, obj: Option<&str>) -> Simple<Token> {
    let msg = |d1, d2| match obj {
        Some(obj) => format!("Missing delimitter for {obj} - delimit with '{d1} .. {d2}'."),
        None => format!("Missing delimtter - delimit with '{d1} .. {d2}'."),
    };

    match token {
        Token::RParen | Token::LParen => Simple::custom(span, msg('(', ')')),
        Token::RBrace | Token::LBrace => Simple::custom(span, msg('{', '}')),
        Token::RBracket | Token::LBracket => Simple::custom(span, msg('[', ']')),
        Token::RAngle | Token::LAngle => Simple::custom(span, msg('<', '>')),
        _ => Simple::custom(span, "Missing delimitter"),
    }
}

pub fn comma(span: Range<usize>) -> Simple<Token> {
    Simple::custom(span, "Expected a ',' to separate arguments.")
}

pub fn throw(prev_err: Simple<Token>, next_err: Simple<Token>) -> Simple<Token> {
    let expected = prev_err
        .expected()
        .map(|expected| match expected {
            Some(expected) => expected.to_string(),
            None => "end of input".to_string(),
        })
        .collect::<Vec<_>>();

    if expected.len() == 1 {
        let expected = expected.first().unwrap();
        let range = prev_err.span();
        let start = range.start;

        let msg = match expected.as_str() {
            ":" => {
                Some("Unfinished interval - add a ':' or specify interval with different length.")
            }
            _ => None,
        };

        if let Some(msg) = msg {
            return Simple::custom(start - 1..start - 1 + expected.len(), msg);
        }
    }

    match prev_err.reason() {
        SimpleReason::Custom(_) => prev_err,
        _ => next_err,
    }
}
