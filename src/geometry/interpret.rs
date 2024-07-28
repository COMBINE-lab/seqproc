use std::{path::PathBuf, str::FromStr};

use antisequence::{
    graph::{
        MatchType::{ExactSearch, HammingSearch, PrefixAln},
        Threshold::Frac,
    },
    *,
};
use chumsky::chain::Chain;
use expr::{label_exists, Expr};
use graph::{Graph, MatchType::Hamming, Threshold};

use crate::{
    compile::{
        functions::CompiledFunction,
        utils::{GeometryMeta, GeometryPiece},
        CompiledData,
    },
    parser::{IntervalKind, IntervalShape},
    processors::*,
    S,
};

use super::Nucleotide;

// use these consts for left and right
static VOID_LABEL: &str = "_";
static NEXT_RIGHT: &str = "_r";
static NEXT_LEFT: &str = "_l";
pub static FILTER: &str = "_f";

fn labels(read_label: &[&str]) -> (String, String) {
    let len = read_label.len();
    let next_label = read_label
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join("");

    if len == 1 {
        (format!("{}*", read_label.first().unwrap()), next_label)
    } else {
        (next_label.clone(), next_label)
    }
}

impl<'a> CompiledData {
    pub fn interpret<'b: 'a>(&'a self, graph: &'b mut Graph, additional_args: &[String]) {
        let Self {
            geometry,
            transformation,
        } = self;

        for (i, read_geometry) in geometry.iter().enumerate() {
            interpret_geometry(
                graph,
                read_geometry,
                &format!("seq{}.", i + 1),
                additional_args,
            );
        }

        if let Some(transformation) = transformation {
            for (i, tr) in transformation.iter().enumerate() {
                let seq_name = format!("seq{}.*", i + 1);
                let tr = format!("{{{}}}", tr.join("}{"));
                graph.add(set_node(&seq_name, antisequence::expr::fmt_expr(tr)));
            }
        };
    }
}

fn interpret_geometry(
    graph: &mut Graph,
    geometry: &[GeometryMeta],
    init_label: &str,
    additional_args: &[String],
) {
    let mut geometry_iter = geometry.iter();

    let mut label: Vec<&str> = vec![init_label];

    while let Some(gp) = geometry_iter.next() {
        let (_, size, _, _) = gp.unpack();

        match size {
            IntervalShape::FixedSeq(_) | IntervalShape::FixedLen(_) => {
                gp.interpret(&label, additional_args, graph);
            }
            IntervalShape::RangedLen(_) | IntervalShape::UnboundedLen => {
                // by rules of geometry this should either be None or a sequence
                if let Some(next) = geometry_iter.next() {
                    next.interpret_dual(gp, &mut label, additional_args, graph);
                } else {
                    gp.interpret(&label, additional_args, graph);
                }
            }
        };

        label.push(NEXT_RIGHT);
    }
}

fn parse_additional_args(arg: String, args: &[String]) -> PathBuf {
    match arg.parse::<usize>() {
        Ok(n) => PathBuf::from_str(
            &args
                .get(n)
                .unwrap_or_else(|| {
                    panic!(
                        "Expected {n} additional arguments with `--additional` tag. Found only {}.",
                        args.len()
                    )
                })
                .clone(),
        )
        .expect(&format!(
            "Expected path as argument -- could not parse argument {n} as path."
        )),
        _ => PathBuf::from_str(&arg).expect(&format!(
            "Expected path as argument -- could not parse {arg} as path."
        )),
    }
}

fn execute_stack(
    stack: Vec<S<CompiledFunction>>,
    label: &str,
    attr: &str,
    size: &IntervalShape,
    additional_args: &[String],
    graph: &mut Graph,
) {
    let range = if let IntervalShape::RangedLen(S((a, b), _)) = size {
        Some(*a..=*b)
    } else {
        None
    };

    let interval_length = match size {
        IntervalShape::FixedSeq(v) => v.len(),
        IntervalShape::FixedLen(S(n, _)) => *n,
        IntervalShape::RangedLen(S((_, b), _)) => *b,
        IntervalShape::UnboundedLen => 0,
    };

    let interval_name = if attr.is_empty() {
        label
    } else {
        &[label, ".", attr].concat()
    };

    for S(fn_, _) in stack.into_iter().rev() {
        match fn_ {
            CompiledFunction::Remove => {
                graph.add(trim_node([antisequence::expr::label(interval_name)]));
            }
            CompiledFunction::Hamming(_) => {
                panic!("Hamming requires to be bound to a sequence cannot operate in isolation")
            }
            CompiledFunction::Map(file, fns) => {
                let file = parse_additional_args(file, additional_args);

                // map node
                // let mapped = map(read, label, attr, file, 0);
                execute_stack(fns, label, "not_mapped", size, additional_args, graph);
            }
            CompiledFunction::MapWithMismatch(file, fns, mismatch) => {
                let file = parse_additional_args(file, additional_args);

                // map node
                // let mapped = map(read, label, attr, file, mismatch);
                execute_stack(fns, label, "not_mapped", size, additional_args, graph);
            }
            CompiledFunction::FilterWithinDist(file, mismatch) => {
                let file_path = parse_additional_args(file, additional_args);
                let patterns = parse_file_filter(file_path);

                graph.add(match_node(
                    patterns,
                    label,
                    vec![label],
                    Hamming(Threshold::Count(interval_length - mismatch)),
                ));
                graph.add(retain_node(
                    expr::attr_exists(&[label, ".", FILTER].concat()).not(),
                ));
            }
            // for the rest of the compliled functions which translate exactly to a single node
            _ => {
                graph.add(set_node(interval_name, fn_.to_expr(interval_name, &range)));
            }
        };
    }
}

impl<'a> GeometryMeta {
    fn unpack<'b: 'a>(
        &'b self,
    ) -> (
        IntervalKind,
        IntervalShape,
        Option<&'a str>,
        Vec<S<CompiledFunction>>,
    ) {
        let GeometryMeta {
            expr: S(GeometryPiece { type_, size, label }, _),
            stack,
        } = self;

        if let Some(l) = label {
            (*type_, size.clone(), Some(l), stack.to_vec())
        } else {
            (*type_, size.clone(), None, stack.to_vec())
        }
    }

    fn interpret_no_cut(&self, label: &[&str], additional_args: &[String], graph: &mut Graph) {
        let (type_, size, self_label, mut stack) = self.unpack();

        let (init_label, cur_label) = labels(label);
        let seq_name = label.first().unwrap();

        let this_label = if let Some(l) = self_label {
            format!("{seq_name}{l}")
        } else {
            cur_label
        };

        if type_ == IntervalKind::Discard {
            stack.push(S(CompiledFunction::Remove, 0..1));
        }

        // this is only called from `interpret_dual` which is for variable to fixedSeq
        // thus this is only for variable sized segments
        match size {
            IntervalShape::RangedLen(S((a, b), _)) => {
                graph.add(valid_label_length(&this_label, a, Some(b)));
            }
            IntervalShape::UnboundedLen => {
                graph.add(set_node(
                    &init_label,
                    antisequence::expr::Expr::from(antisequence::expr::label(this_label.clone())),
                ));
            }
            _ => unreachable!(),
        };

        execute_stack(stack, &this_label, "", &size, additional_args, graph);
    }

    fn interpret<'c: 'a>(&self, label: &[&str], additional_args: &[String], graph: &mut Graph) {
        let (type_, size, self_label, mut stack) = self.unpack();

        let (init_label, cur_label) = labels(label);
        let seq_name = label.first().unwrap();

        let this_label = if let Some(l) = self_label {
            format!("{seq_name}{l}")
        } else {
            format!("{cur_label}{NEXT_LEFT}")
        };
        let next_label = format!("{cur_label}{NEXT_RIGHT}");

        if type_ == IntervalKind::Discard {
            stack.push(S(CompiledFunction::Remove, 0..1));
        }

        // execute the requisite process here
        match size.clone() {
            IntervalShape::FixedSeq(S(seq, _)) => {
                let labels;
                let match_type = if !stack.is_empty() {
                    match stack.last().unwrap() {
                        S(CompiledFunction::Hamming(n), _) => {
                            labels = vec!["_", &this_label, &next_label];
                            let dist = Frac(1.0 - (*n as f64 / seq.len() as f64));
                            HammingSearch(dist)
                        }
                        _ => {
                            labels = vec![&this_label, &next_label];
                            PrefixAln {
                                identity: 1.0,
                                overlap: 1.0,
                            }
                        }
                    }
                } else {
                    labels = vec!["_", &this_label, &next_label];
                    ExactSearch
                };

                graph.add(match_node(
                    Patterns::from_strs([Nucleotide::as_str(&seq)]),
                    &init_label,
                    labels,
                    match_type,
                ));
                graph.add(retain_node(label_exists(this_label.clone())));
            }
            IntervalShape::FixedLen(S(len, _)) => {
                graph.add(cut_node(
                    into_transform_expr(&init_label, [this_label.as_str(), &next_label]),
                    Expr::from(len),
                    // LeftEnd(len),
                ));
                graph.add(valid_label_length(&this_label, len, None));
            }
            IntervalShape::RangedLen(S((a, b), _)) => {
                graph.add(cut_node(
                    into_transform_expr(&init_label, [this_label.as_str(), &next_label]),
                    Expr::from(b),
                    // LeftEnd(b),
                ));
                graph.add(valid_label_length(&this_label, a, Some(b)));
            }
            IntervalShape::UnboundedLen => {
                graph.add(cut_node(
                    into_transform_expr(&init_label, [VOID_LABEL, &this_label]),
                    Expr::from(0),
                    // LeftEnd(0),
                ));
                graph.add(set_node(
                    &init_label,
                    antisequence::expr::Expr::from(antisequence::expr::label(this_label.clone())),
                ));
            }
        };

        execute_stack(
            stack,
            this_label.as_str(),
            "",
            &size,
            additional_args,
            graph,
        );
    }

    fn interpret_dual(
        &self,
        prev: &Self,
        label: &mut Vec<&str>,
        additional_args: &[String],
        graph: &mut Graph,
    ) {
        // unpack label for self
        let (_, size, this_label, mut stack) = self.unpack();
        let (_, _, prev_label, _) = prev.unpack();
        // execute the processing for next

        let (init_label, cur_label) = labels(label);
        let seq_name = label.first().unwrap();

        let mut left_label = label.to_owned();

        let prev_label = if let Some(l) = prev_label {
            left_label.push(l);
            format!("{seq_name}{l}")
        } else {
            left_label.push(NEXT_LEFT);
            format!("{cur_label}{NEXT_LEFT}")
        };
        let this_label = if let Some(l) = this_label {
            format!("{seq_name}{l}")
        } else {
            format!("{cur_label}_anchor")
        };
        let next_label = format!("{cur_label}{NEXT_RIGHT}");

        match size.clone() {
            IntervalShape::FixedSeq(S(seq, _)) => {
                // check if the first function on the stack is a hamming search
                // else do an exact match
                let match_type = if !stack.is_empty() {
                    match stack.pop().unwrap() {
                        S(CompiledFunction::Hamming(n), _) => {
                            let dist = Frac(1.0 - (n as f64 / seq.len() as f64));
                            HammingSearch(dist)
                        }
                        _ => ExactSearch,
                    }
                } else {
                    ExactSearch
                };

                graph.add(match_node(
                    Patterns::from_strs([Nucleotide::as_str(&seq)]),
                    &init_label,
                    vec![&prev_label, &this_label, &next_label],
                    match_type,
                ));
                graph.add(retain_node(label_exists(this_label.clone())));

                execute_stack(stack, &this_label, "", &size, additional_args, graph);
            }
            _ => unreachable!(),
        };

        // call interpret for self
        // this is just an unbounded or ranged segment. No cut just set or validate
        prev.interpret_no_cut(&left_label, additional_args, graph);
    }
}
