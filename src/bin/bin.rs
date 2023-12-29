use antisequence::{iter_fastq2, Reads};
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, Stream};
use clap::{arg, Parser as ClapParser};

use seqproc::{
    compile::{compile, CompiledData},
    lexer,
    parser::parser,
};

/// General puprose sequence preprocessor
#[derive(Debug, ClapParser)]
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

    #[arg(short, long, value_parser, num_args = 1.., value_delimiter = ' ')]
    additional: Vec<String>,
}

pub fn interpret(args: Args, compiled_data: CompiledData) {
    let Args {
        geom: _,
        file1,
        file2,
        out1,
        out2,
        threads,
        additional,
    } = args;

    let read = iter_fastq2(file1, file2, 256)
        .unwrap_or_else(|e| panic!("{e}"))
        .boxed();

    let read = compiled_data.interpret(read, &out1, &out2, &additional);

    read.run_with_threads(threads)
}

fn main() {
    let args: Args = Args::parse();

    let geom = std::fs::read_to_string(&args.geom).unwrap();

    let (tokens, mut errs) = lexer::lexer().parse_recovery(&*geom);

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser().parse_recovery(Stream::from_iter(
            tokens.len()..tokens.len() + 1,
            tokens.clone().into_iter(),
        ));

        if let Some((ast, _)) = &ast {
            let res = compile(ast.clone());

            if let Err(e) = res {
                errs.push(Simple::custom(e.span, e.msg));
            } else {
                interpret(args, res.ok().unwrap());
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
}
