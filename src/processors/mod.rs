use std::{
    fs::File,
    io::{BufRead, BufReader},
    ops::RangeInclusive,
    path::PathBuf,
};

use crate::{
    geometry::compile::functions::CompiledFunction,
    interpret::{LabelOrAttr, AMBIG, FILTER, MAPPED, SUB},
};

use antisequence::{
    expr::{label, TransformExpr},
    graph::*,
    *,
};
use expr::Expr;
use serde::Deserialize;

use crate::Nucleotide;

impl CompiledFunction {
    pub fn to_expr(
        self,
        interval_name: &str,
        meta_data: &Option<RangeInclusive<usize>>,
    ) -> antisequence::expr::Expr {
        use antisequence::expr::Expr;
        match self {
            CompiledFunction::Reverse => Expr::from(label(interval_name)).rev(),
            CompiledFunction::ReverseComp => Expr::from(label(interval_name)).revcomp(),
            // trunc by
            CompiledFunction::Truncate(n) => {
                Expr::from(label(interval_name)).slice(..=-(n as isize))
            }
            CompiledFunction::TruncateLeft(n) => {
                Expr::from(label(interval_name)).slice(-(n as isize)..)
            }
            // trunc to
            CompiledFunction::TruncateTo(n) => Expr::from(label(interval_name)).slice(..n),
            CompiledFunction::TruncateToLeft(n) => Expr::from(label(interval_name))
                .slice(Expr::from(label(interval_name)).len().sub(Expr::from(n))..),
            CompiledFunction::Pad(n, nuc) => Expr::from(label(interval_name))
                .concat(Expr::from(Nucleotide::as_string(nuc)).repeat(n)),
            CompiledFunction::PadLeft(n, nuc) => Expr::from(Nucleotide::as_string(nuc))
                .repeat(n)
                .concat(Expr::from(label(interval_name))),
            CompiledFunction::PadTo(n, nuc) => Expr::from(label(interval_name)).pad(
                Expr::from(Nucleotide::as_string(nuc)),
                Expr::from(n),
                End::Right,
            ),
            CompiledFunction::PadToLeft(n, nuc) => Expr::from(label(interval_name)).pad(
                Expr::from(Nucleotide::as_string(nuc)),
                Expr::from(n),
                End::Left,
            ),
            CompiledFunction::Normalize => {
                let range = if let Some(r) = meta_data {
                    r
                } else {
                    panic!("Expected a range")
                };
                Expr::from(label(interval_name)).normalize(range.clone())
            }
            // these cannot be exprs
            CompiledFunction::Remove => unimplemented!(),
            CompiledFunction::Map(_, _) => unimplemented!(),
            CompiledFunction::MapWithMismatch(_, _, _) => unimplemented!(),
            CompiledFunction::FilterWithinDist(_, _) => unimplemented!(),
            CompiledFunction::Hamming(_) => unimplemented!(),
            // Search is handled in the interpreter, not as an expr
            CompiledFunction::Search => unimplemented!(),
            // SearchWhitelist is handled in the interpreter; in expr context just return the label
            CompiledFunction::SearchWhitelist(_, _, _) => Expr::from(label(interval_name)),
            // AnchorRelative is handled in the interpreter, not as an expr
            CompiledFunction::AnchorRelative => unimplemented!(),
        }
    }
}

pub fn into_transform_expr<'a>(
    this_label: &str,
    next_labels: impl IntoIterator<Item = &'a str>,
) -> TransformExpr {
    TransformExpr::new(
        [label(this_label)],
        next_labels.into_iter().map(|l| {
            if l.eq("_") {
                None
            } else {
                Some(expr::LabelOrAttr::Label(label(l)))
            }
        }),
    )
}

pub fn cut_node(tr_expr: TransformExpr, index: antisequence::expr::Expr) -> CutOp {
    CutOp::new(tr_expr, index)
}

pub fn set_node(
    label_or_attr: crate::interpret::LabelOrAttr<'_>,
    expr: antisequence::expr::Expr,
) -> SetOp {
    use crate::interpret::LabelOrAttr;

    match label_or_attr {
        LabelOrAttr::Attr(attr) => SetOp::new(expr::attr(attr), expr),
        LabelOrAttr::Label(label) => SetOp::new(expr::label(label), expr),
    }
}

pub fn retain_node(expr: antisequence::expr::Expr) -> RetainOp {
    RetainOp::new(expr)
}

pub fn valid_label_length(this_label: &str, from: usize, to: Option<usize>) -> RetainOp {
    if let Some(to) = to {
        return retain_node(
            antisequence::expr::Expr::from(label(this_label))
                .len()
                .in_bounds(from..=to),
        );
    }
    retain_node(
        antisequence::expr::Expr::from(label(this_label))
            .len()
            .eq(from),
    )
}

pub fn trim_node(labels: impl IntoIterator<Item = antisequence::expr::Label>) -> TrimOp {
    TrimOp::new(labels)
}

pub fn map(this_label: &str, patterns: Patterns, match_type: MatchType, graph: &mut Graph) {
    let next_label: &str = &format!("{this_label}{MAPPED}");

    graph.add(match_node(
        patterns,
        this_label,
        vec![next_label],
        match_type,
    ));

    graph.add(set_node(
        LabelOrAttr::Attr(&format!("{this_label}.{MAPPED}")),
        Expr::from(antisequence::expr::attr(format!("{this_label}.{AMBIG}"))).not(),
    ));

    let mut mapping_graph = Graph::new();
    mapping_graph.add(set_node(
        LabelOrAttr::Label(next_label),
        Expr::from(expr::attr(format!("{this_label}.{SUB}"))),
    ));
    graph.add(SelectOp::new(
        Expr::from(expr::attr(&format!("{this_label}.{MAPPED}"))),
        mapping_graph,
    ));
}

pub fn match_node(
    patterns: Patterns,
    starting_label: &str,
    next_labels: Vec<&str>,
    match_type: MatchType,
) -> MatchAnyOp {
    let tr_expr = into_transform_expr(starting_label, next_labels);

    MatchAnyOp::new(tr_expr, patterns, match_type)
}

pub fn parse_file_filter(path: PathBuf) -> Patterns {
    let file = File::open(path.clone()).unwrap_or_else(|_| {
        panic!(
            "Expected file -- could not open {:?}",
            path.file_name().unwrap()
        )
    });
    let reader = BufReader::new(file);
    let mut contents = vec![];
    for (i, line) in reader.lines().enumerate() {
        let line = line.unwrap_or_else(|_| {
            panic!(
                "Could not read line {i} in file {:?}.",
                path.file_name().unwrap()
            )
        });
        contents.push(line);
    }
    Patterns::from_strs(contents).with_pattern_name(FILTER)
}

#[derive(Debug, Deserialize)]
struct SeqprocMap {
    sub_patt: String,
    match_patt: String,
}

pub fn parse_file_match(path: PathBuf) -> Patterns {
    let mut rdr = csv::ReaderBuilder::new()
        .delimiter(b'\t')
        .comment(Some(b'#'))
        .has_headers(false)
        .from_path(path)
        .expect("cannot open mapping file");

    let mut mappings = vec![];
    for result in rdr.deserialize() {
        let mapping: SeqprocMap = result.expect("Could not parse line in map file");

        mappings.push(Pattern::Expr {
            expr: Expr::bytes(mapping.match_patt.into()),
            attrs: vec![Data::Bytes(mapping.sub_patt.into())],
        });
    }

    Patterns::new(mappings, vec![SUB])
        .with_multimatch_name(AMBIG)
        .with_pattern_name(MAPPED)
}
