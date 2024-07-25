use std::ops::RangeInclusive;

use crate::geometry::compile::functions::CompiledFunction;

use antisequence::{
    expr::{label, TransformExpr},
    graph::*,
    *,
};

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
            CompiledFunction::TruncateTo(n) => Expr::from(label(interval_name)).slice(..=n),
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
        }
    }
}

pub fn into_transform_expr(
    this_label: &str,
    next_labels: impl IntoIterator<Item = String>,
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

pub fn cut_node(tr_expr: TransformExpr, index: EndIdx) -> CutNode {
    CutNode::new(tr_expr, index)
}

pub fn set_node(label_name: &str, expr: antisequence::expr::Expr) -> SetNode {
    SetNode::new(label(label_name), expr)
}

pub fn retain_node(expr: antisequence::expr::Expr) -> RetainNode {
    RetainNode::new(expr)
}

pub fn valid_label_length(this_label: &str, from: usize, to: Option<usize>) -> RetainNode {
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

pub fn trim_node(labels: impl IntoIterator<Item = antisequence::expr::Label>) -> TrimNode {
    TrimNode::new(labels)
}

pub fn filter(
    label: &str,
    attr: &str,
    filename: String,
    mismatch: usize,
) -> Box<dyn antisequence::graph::GraphNode> {
    todo!();
    // let sel_expr = get_selector(label, attr);
    // let sel_retain_expr = get_selector(label, "_f");

    // let tr_expr = TransformExpr::new(format!("{label} -> {label}._f").as_bytes()).unwrap();

    // read.filter(sel_expr, tr_expr, filename, mismatch)
    //     .retain(sel_retain_expr)
    //     .boxed()
}

pub fn map(
    label: &str,
    attr: &str,
    file: String,
    mismatch: usize,
) -> Box<dyn antisequence::graph::GraphNode> {
    todo!();
    // let sel_expr = get_selector(label, attr);
    // let tr_expr = TransformExpr::new(format!("{label} -> {label}.not_mapped").as_bytes()).unwrap();

    // read.map(sel_expr, tr_expr, file, mismatch).boxed()
}

pub fn match_node(
    sequence: &[Nucleotide],
    starting_label: &str,
    this_label: &str,
    prev_label: &str,
    next_label: &str,
    match_type: MatchType,
) -> MatchAnyNode {
    let tr_expr = match match_type {
        PrefixAln { .. } => into_transform_expr(
            starting_label,
            [this_label.to_owned(), next_label.to_owned()],
        ),
        ExactSearch | HammingSearch(_) => into_transform_expr(
            starting_label,
            [
                prev_label.to_owned(),
                this_label.to_owned(),
                next_label.to_owned(),
            ],
        ),
        _ => unreachable!(),
    };

    MatchAnyNode::new(
        tr_expr,
        Patterns::from_strs([Nucleotide::as_str(sequence)]),
        match_type,
    )
}
