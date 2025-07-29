use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{error::RichReason, prelude::*};

use crate::lexer::Token;

pub fn handle_errors(errs: Vec<Rich<'_, String>>, source: String) {
    // error recovery
    errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, ((), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(e.to_string())
            .with_label(
                Label::new(((), e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new(((), span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .print(Source::from(&source))
            .unwrap();
    });
}

pub fn missing_delimiter<'a>(token: Token, span: SimpleSpan, obj: Option<&str>) -> Rich<'a, Token> {
    let msg = |d1, d2| match obj {
        Some(obj) => format!("Missing delimitter for {obj} - delimit with '{d1} .. {d2}'."),
        None => format!("Missing delimtter - delimit with '{d1} .. {d2}'."),
    };

    match token {
        Token::RParen | Token::LParen => Rich::custom(span, msg('(', ')')),
        Token::RBrace | Token::LBrace => Rich::custom(span, msg('{', '}')),
        Token::RBracket | Token::LBracket => Rich::custom(span, msg('[', ']')),
        Token::RAngle | Token::LAngle => Rich::custom(span, msg('<', '>')),
        _ => Rich::custom(span, "Missing delimitter"),
    }
}

pub fn comma<'a>(span: SimpleSpan) -> Rich<'a, Token> {
    Rich::custom(span, "Expected a ',' to separate arguments.")
}

pub fn throw<'a>(prev_err: Rich<'a, Token>, next_err: Rich<'a, Token>) -> Rich<'a, Token> {
    if prev_err.expected().len() > 0 {
        let expected = prev_err.clone();
        let range = prev_err.span();
        let start = range.start;

        let msg = match prev_err.clone().into_reason() {
            chumsky::error::RichReason::Custom(msg) => match msg.as_str() {
                ":" => Some(String::from(
                    "Unfinished interval - add a ':' or specify interval with different length.",
                )),
                _ => None,
            },
            chumsky::error::RichReason::ExpectedFound {
                expected,
                found: o_found,
            } => match o_found {
                Some(found) => match found {
                    chumsky::util::Maybe::Ref(r_t) => Some(format!(
                        "Expected {} but found: {}.",
                        expected
                            .iter()
                            .map(|exp| format!("{exp}"))
                            .collect::<String>(),
                        r_t
                    )),
                    chumsky::util::Maybe::Val(t) => Some(format!(
                        "Expected {} but found: {}.",
                        expected
                            .iter()
                            .map(|exp| format!("{exp}"))
                            .collect::<String>(),
                        t
                    )),
                },
                None => Some(format!(
                    "Expected {} but found nothing.",
                    expected
                        .iter()
                        .map(|exp| format!("{exp}"))
                        .collect::<String>()
                )),
            },
        };

        if let Some(msg) = msg {
            return Rich::custom((start..start + expected.span().end).into(), msg);
        }
    }

    match prev_err.reason() {
        RichReason::Custom(_) => prev_err,
        _ => next_err,
    }
}
