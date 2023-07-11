use antisequence::{iter_fastq2, sel, Reads};
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, Stream};
use clap::{arg, Parser as cParser};
// use std::time::Instant;

use seqproc::{compile::compile, lexer, parser::parser, syntax::Read};

/// General puprose sequence preprocessor
#[derive(Debug, cParser)]
pub struct Args {
    /// FGDL string
    #[arg(short, long)]
    geom: String,

    /// r1 fastq file
    #[arg(short = '1', long)]
    file1: String,

    /// r2 fastq file
    #[arg(short = '2', long)]
    file2: String,

    /// r1 out fastq file
    #[arg(short = 'o', long, default_value = "")]
    out1: String,

    /// r2 out fastq file
    #[arg(short = 'w', long, default_value = "")]
    out2: String,

    /// number of threads to use
    #[arg(short, long, default_value = "1")]
    threads: usize,
}

pub fn interpret(args: Args, reads: Vec<Read>) {
    let Args {
        geom: _,
        file1,
        file2,
        out1,
        out2,
        threads,
    } = args;

    let read_one = reads.first().unwrap().to_owned();
    let read_two = reads.last().unwrap().to_owned();

    let read = iter_fastq2(file1, file2, 256)
        .unwrap_or_else(|e| panic!("{e}"))
        .boxed();

    let read = read_one.interpret(read);
    let read = read_two.interpret(read);

    if out1.is_empty() && out2.is_empty() {
        return read
            .collect_fastq1(sel!(), "/dev/null")
            .run_with_threads(threads);
    }

    read.collect_fastq2(sel!(), out1, out2)
        .run_with_threads(threads)
}

fn main() {
    let args: Args = Args::parse();

    // let start = Instant::now();
    let geom = std::fs::read_to_string(args.geom).unwrap();

    let (tokens, mut errs) = lexer::lexer().parse_recovery(geom.clone());

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser().parse_recovery(Stream::from_iter(
            tokens.len()..tokens.len() + 1,
            tokens.clone().into_iter(),
        ));

        if let Some((ast, _)) = &ast {
            let err = compile(ast.clone());

            if let Err(e) = err {
                errs.push(Simple::custom(e.span, e.msg));
            }
        };

        parse_errs
    } else {
        Vec::new()
    };

    // error recovery
    errs.into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
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
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
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

            report.finish().print(Source::from(&geom)).unwrap();
        })

    // match seqproc::parse::parser().parse(geom) {
    //     Ok(reads) => {
    //         println!("{:?}", reads);
    //         // interpret(args, reads)
    //     }
    //     Err(errs) => println!("Error: {:?}", errs),
    // }
    // let duration = start.elapsed();
    // println!("tranformation completed in {:.2}s", duration.as_secs_f32());
}
