use std::{path::PathBuf, str::FromStr, usize};

use antisequence::{
    graph::MatchType::{
        self, ExactBoundedMatch, ExactSearch, HammingBoundedMatch, HammingSearch, PrefixAln,
    },
    *,
};
use chumsky::chain::Chain;
use expr::Expr;
use graph::{
    Graph,
    MatchType::{Exact, ExactPrefix, Hamming, HammingPrefix},
    SelectOp, Threshold,
};

use crate::{
    compile::{
        functions::CompiledFunction,
        utils::{GeometryMeta, GeometryPiece},
        CompiledData,
    },
    parser::{IntervalKind, IntervalShape},
    processors::*,
    Nucleotide, S,
};

static VOID_LABEL: &str = "_";
static NEXT_RIGHT: &str = "_r";
static NEXT_LEFT: &str = "_l";
pub static FILTER: &str = "_f";
pub static MAPPED: &str = "_m";
pub static AMBIG: &str = "ambig";
pub static SUB: &str = "sub";

pub enum LabelOrAttr<'a> {
    Label(&'a str),
    Attr(&'a str),
}

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
    pub fn interpret<'b: 'a>(&'a self, graph: &'a mut Graph, additional_args: &[&str]) {
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
                graph.add(set_node(
                    LabelOrAttr::Label(&seq_name),
                    antisequence::expr::fmt_expr(tr),
                ));
            }
        };
    }
}

fn interpret_geometry(
    graph: &mut Graph,
    geometry: &[GeometryMeta],
    init_label: &str,
    additional_args: &[&str],
) {
    let mut geometry_iter = geometry.iter();

    let mut label: Vec<&str> = vec![init_label];

    // keep a running length that we have parsed to far
    // TODO: this may need to be tested more
    let mut min_start_idx = 0;

    while let Some(gp) = geometry_iter.next() {
        let (interval_type, size, _, _) = gp.unpack();

        match size {
            IntervalShape::FixedSeq(S(v, _)) => {
                match interval_type {
                    IntervalKind::Discard => (),
                    _ => min_start_idx += v.len(),
                }
                gp.interpret(&label, additional_args, graph);
            }
            IntervalShape::FixedLen(S(len, _)) => {
                match interval_type {
                    IntervalKind::Discard => (),
                    _ => min_start_idx += len,
                }
                gp.interpret(&label, additional_args, graph);
            }
            // in the case of a ranged length we add the minimum of the range to the min_idx
            // then match within the bounds of the ranged len
            IntervalShape::RangedLen(S((from, _), _)) => {
                min_start_idx += from;
                // by rules of geometry this should either be None or a sequence
                if let Some(next) = geometry_iter.next() {
                    next.interpret_dual(gp, &mut label, additional_args, graph, &mut min_start_idx);
                } else {
                    gp.interpret(&label, additional_args, graph);
                }
            }
            IntervalShape::UnboundedLen => {
                // by rules of geometry this should either be None or a sequence
                if let Some(next) = geometry_iter.next() {
                    next.interpret_dual(gp, &mut label, additional_args, graph, &mut min_start_idx);
                } else {
                    gp.interpret(&label, additional_args, graph);
                }
            }
        };

        label.push(NEXT_RIGHT);
    }
}

fn parse_additional_args(arg: String, args: &[&str]) -> PathBuf {
    let len = args.len();
    match arg.parse::<usize>() {
        Ok(n) => PathBuf::from_str(args.get(n).unwrap_or_else(|| {
            panic!(
                "Expected {n} additional arguments with `--additional` tag. Found only {}.",
                len
            )
        }))
        .unwrap_or_else(|_| {
            panic!("Expected path as argument -- could not parse argument {n} as path.")
        }),
        _ => PathBuf::from_str(&arg).unwrap_or_else(|_| {
            panic!("Expected path as argument -- could not parse {arg} as path.")
        }),
    }
}

fn execute_stack(
    stack: Vec<S<CompiledFunction>>,
    label: &str,
    size: &IntervalShape,
    additional_args: &[&str],
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

    for S(fn_, _) in stack.into_iter().rev() {
        match fn_ {
            CompiledFunction::Remove => {
                graph.add(trim_node([antisequence::expr::label(label)]));
            }
            CompiledFunction::Hamming(_) => {
                panic!("Hamming requires to be bound to a sequence cannot operate in isolation")
            }
            CompiledFunction::Map(file, fns) => {
                let file_path = parse_additional_args(file, additional_args);
                let patterns = parse_file_match(file_path);

                map(label, patterns, Exact, graph);

                let mut fallback_graph = Graph::new();
                execute_stack(fns, label, size, additional_args, &mut fallback_graph);

                graph.add(SelectOp::new(
                    Expr::from(expr::attr(&format!("{label}.{MAPPED}"))).not(),
                    fallback_graph,
                ));
            }
            CompiledFunction::MapWithMismatch(file, fns, mismatch) => {
                let file_path = parse_additional_args(file, additional_args);
                let patterns = parse_file_match(file_path);

                map(
                    label,
                    patterns,
                    Hamming(Threshold::Count(interval_length - mismatch)),
                    graph,
                );

                let mut fallback_graph = Graph::new();
                execute_stack(fns, label, size, additional_args, &mut fallback_graph);

                graph.add(SelectOp::new(
                    Expr::from(expr::attr(&format!("{label}.{MAPPED}"))).not(),
                    fallback_graph,
                ));
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
                    expr::attr_exists([label, ".", FILTER].concat()).not(),
                ));
            }
            // for the rest of the compliled functions which translate exactly to a single node
            _ => {
                graph.add(set_node(
                    LabelOrAttr::Label(label),
                    fn_.to_expr(label, &range),
                ));
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

    fn interpret_no_cut(&self, label: &[&str], additional_args: &[&str], graph: &mut Graph) {
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
                    LabelOrAttr::Label(&init_label),
                    antisequence::expr::Expr::from(antisequence::expr::label(this_label.clone())),
                ));
            }
            _ => unreachable!(),
        };

        execute_stack(stack, &this_label, &size, additional_args, graph);
    }

    fn interpret<'c: 'a>(&self, label: &[&str], additional_args: &[&str], graph: &mut Graph) {
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
                let labels = vec![this_label.as_str(), &next_label];
                let match_type = if !stack.is_empty() {
                    match stack.last().unwrap() {
                        S(CompiledFunction::Hamming(n), _) => {
                            HammingPrefix(Threshold::Count(seq.len() - n))
                        }
                        _ => PrefixAln {
                            identity: 1.0,
                            overlap: 1.0,
                        },
                    }
                } else {
                    ExactPrefix
                };

                graph.add(match_node(
                    Patterns::from_strs([Nucleotide::as_str(&seq)]),
                    &init_label,
                    labels,
                    match_type,
                ));
                graph.add(retain_node(expr::label_exists(this_label.clone())));
            }
            IntervalShape::FixedLen(S(len, _)) => {
                graph.add(cut_node(
                    into_transform_expr(&init_label, [this_label.as_str(), &next_label]),
                    Expr::from(len),
                ));
                graph.add(valid_label_length(&this_label, len, None));
            }
            IntervalShape::RangedLen(S((a, b), _)) => {
                graph.add(cut_node(
                    into_transform_expr(&init_label, [this_label.as_str(), &next_label]),
                    Expr::from(b),
                ));
                graph.add(valid_label_length(&this_label, a, Some(b)));
            }
            IntervalShape::UnboundedLen => {
                graph.add(cut_node(
                    into_transform_expr(&init_label, [VOID_LABEL, &this_label]),
                    Expr::from(0),
                ));
                graph.add(set_node(
                    LabelOrAttr::Label(&init_label),
                    antisequence::expr::Expr::from(antisequence::expr::label(this_label.clone())),
                ));
            }
        };

        execute_stack(stack, this_label.as_str(), &size, additional_args, graph);
    }

    fn interpret_dual(
        &self,
        prev: &Self,
        label: &mut Vec<&str>,
        additional_args: &[&str],
        graph: &mut Graph,
        range_start: &mut usize,
    ) {
        // unpack label for self
        let (_, size, this_label, mut stack) = self.unpack();
        let (_, prev_shape, prev_label, _) = prev.unpack();
        // execute the processing for next

        // if it is an unbounded beginning then we should do search
        let prev_len_offset = match prev_shape {
            IntervalShape::RangedLen(S((start, end), _)) => Some(end - start),
            _ => None,
        };

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
                let seq_len = seq.len();
                // check if the first function on the stack is a hamming search
                // else do an exact match
                let match_type = get_match_type(prev_len_offset, &mut stack, seq_len, range_start);

                graph.add(match_node(
                    Patterns::from_strs([Nucleotide::as_str(&seq)]),
                    &init_label,
                    vec![&prev_label, &this_label, &next_label],
                    match_type,
                ));
                graph.add(retain_node(expr::label_exists(this_label.clone())));

                execute_stack(stack, &this_label, &size, additional_args, graph);

                // update range_start
                // TODO: this needs to be tested for unbounded beginning segments
                *range_start += prev_len_offset.unwrap_or(0) + seq_len;
            }
            _ => unreachable!(),
        };

        // call interpret for self
        // this is just an unbounded or ranged segment. No cut just set or validate
        prev.interpret_no_cut(&left_label, additional_args, graph);
    }
}

fn get_match_type(
    offset_len: Option<usize>,
    stack: &mut Vec<S<CompiledFunction>>,
    seq_len: usize,
    range_start: &mut usize,
) -> MatchType {
    match offset_len {
        Some(offset) => {
            if !stack.is_empty() {
                match stack.pop().unwrap() {
                    S(CompiledFunction::Hamming(n), _) => HammingBoundedMatch {
                        threshold: Threshold::Count(seq_len - n),
                        from: *range_start,
                        to: *range_start + seq_len + offset,
                    },
                    _ => ExactBoundedMatch {
                        from: *range_start,
                        to: *range_start + seq_len + offset,
                    },
                }
            } else {
                ExactBoundedMatch {
                    from: *range_start,
                    to: *range_start + seq_len + offset,
                }
            }
        }
        None => {
            if !stack.is_empty() {
                match stack.pop().unwrap() {
                    S(CompiledFunction::Hamming(n), _) => {
                        HammingSearch(Threshold::Count(seq_len - n))
                    }
                    _ => ExactSearch,
                }
            } else {
                ExactSearch
            }
        }
    }
}
