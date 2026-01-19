use std::{path::PathBuf, str::FromStr};

use antisequence::{
    graph::MatchType::{self, ExactBoundedMatch, ExactSearch, HammingBoundedMatch, HammingSearch, PrefixAln},
    *,
};
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
                // Check if an anchor FixedSeq follows - if so, collect intermediate pieces
                // and use interpret_dual to extract relative to the anchor
                let mut intermediate_fixed: Vec<&GeometryMeta> = vec![gp];
                let mut anchor: Option<&GeometryMeta> = None;
                let mut lookahead = geometry_iter.clone();

                while let Some(piece) = lookahead.next() {
                    let (_, piece_size, _, piece_stack) = piece.unpack();
                    match piece_size {
                        IntervalShape::FixedSeq(_) => {
                            // Check if this FixedSeq has Anchor modifier
                            let has_anchor = piece_stack.iter().any(|s| {
                                matches!(s.0, CompiledFunction::Anchor)
                            });
                            if has_anchor {
                                anchor = Some(piece);
                            }
                            break;
                        }
                        IntervalShape::FixedLen(_) => {
                            intermediate_fixed.push(piece);
                        }
                        _ => {
                            // Hit variable-length segment; stop scanning
                            break;
                        }
                    }
                }

                if let Some(anchor_gp) = anchor {
                    // Found an anchor/anchor_relative anchor - use interpret_dual mechanism
                    // Skip intermediate_fixed[0] since it's `gp` itself
                    for _ in 1..intermediate_fixed.len() {
                        geometry_iter.next();
                    }
                    // Consume the anchor
                    geometry_iter.next();

                    let seq_name = label.first().unwrap();
                    let (_, cur_label) = labels(&label);
                    
                    // anchor() searches from position 0 (the original read)
                    let search_label = format!("{}*", seq_name);

                    // Create a synthetic unbounded segment for the prev_label
                    // Then use interpret_dual-like logic to search for anchor and slice intermediates
                    let (_, anchor_size, anchor_label, mut anchor_stack) = anchor_gp.unpack();

                    // Remove Anchor from stack (it's a modifier)
                    anchor_stack.retain(|s| !matches!(s.0, CompiledFunction::Anchor));

                    if let IntervalShape::FixedSeq(S(seq, _)) = anchor_size.clone() {
                        let anchor_this_label = if let Some(l) = anchor_label {
                            format!("{seq_name}{l}")
                        } else {
                            format!("{cur_label}_anchor")
                        };
                        let prev_label = format!("{cur_label}{NEXT_LEFT}");
                        let next_label_str = format!("{cur_label}{NEXT_RIGHT}");

                        // Get Hamming distance if specified
                        let match_type = if let Some(S(CompiledFunction::Hamming(n), _)) = anchor_stack.last() {
                            let n = *n;
                            anchor_stack.pop();
                            HammingSearch(Threshold::Count(seq.len() - n))
                        } else {
                            ExactSearch
                        };

                        // Search for anchor with 3-way split
                        // For anchor(), search_label is the original read (position 0)
                        // For anchor_relative(), search_label is the current position
                        graph.add(match_node(
                            Patterns::from_strs([Nucleotide::as_str(&seq)]),
                            &search_label,
                            vec![&prev_label, &anchor_this_label, &next_label_str],
                            match_type,
                        ));
                        graph.add(retain_node(expr::label_exists(anchor_this_label.clone())));

                        // Execute any remaining stack functions on anchor
                        execute_stack(anchor_stack, &anchor_this_label, &anchor_size, additional_args, graph);

                        // Now slice intermediate_fixed from the prev_label region (RIGHT side)
                        let total_fixed_len: usize = intermediate_fixed
                            .iter()
                            .map(|p| {
                                if let IntervalShape::FixedLen(S(len, _)) = p.unpack().1 {
                                    len
                                } else {
                                    0
                                }
                            })
                            .sum();

                        // Slice pieces directly from prev_label (the "before anchor" region)
                        // For anchor_relative: slice from LEFT, last piece takes remainder (flexible)
                        // This handles indels where "before" region is shorter/longer than expected
                        let mut slice_label = prev_label.clone();
                        for (i, piece) in intermediate_fixed.iter().enumerate() {
                            let (piece_type, piece_size, piece_label, piece_stack) = piece.unpack();
                            if let IntervalShape::FixedLen(S(len, _)) = piece_size {
                                let this_label = if let Some(l) = piece_label {
                                    format!("{seq_name}{l}")
                                } else {
                                    format!("{cur_label}_p{i}")
                                };

                                let is_last = i == intermediate_fixed.len() - 1;
                                let next_slice_label = if is_last {
                                    VOID_LABEL.to_string()
                                } else {
                                    format!("{cur_label}_slice{}", i + 1)
                                };

                                if is_last {
                                    // Last piece: take whatever remains (flexible length)
                                    // Use negative cut to take from right side of remaining region
                                    graph.add(cut_node(
                                        into_transform_expr(
                                            &slice_label,
                                            [VOID_LABEL, this_label.as_str()],
                                        ),
                                        Expr::from(-(len as isize)),
                                    ));
                                    // No length validation - accept whatever is there
                                } else {
                                    // Non-last pieces: cut exact length from left
                                    graph.add(cut_node(
                                        into_transform_expr(
                                            &slice_label,
                                            [this_label.as_str(), next_slice_label.as_str()],
                                        ),
                                        Expr::from(len),
                                    ));
                                    graph.add(valid_label_length(&this_label, len, None));
                                }

                                let mut stack = piece_stack;
                                if piece_type == IntervalKind::Discard {
                                    stack.push(S(CompiledFunction::Remove, (0..1).into()));
                                }
                                execute_stack(stack, &this_label, &piece_size, additional_args, graph);

                                slice_label = next_slice_label;
                            }
                        }

                        // NOTE: We do NOT set computed_init_label to next_label_str here.
                        // The read.set() function modifies the underlying string and adjusts
                        // all intersecting mappings, which would corrupt the umi/bc3 mappings
                        // we just created. Instead, the label vector update (label.push("_r"))
                        // at the end of this iteration handles redirection for subsequent pieces.

                        // Update min_start_idx
                        min_start_idx += total_fixed_len + seq.len();
                    }
                } else {
                    // No anchor_relative anchor found - process normally
                    match interval_type {
                        IntervalKind::Discard => (),
                        _ => min_start_idx += len,
                    }
                    gp.interpret(&label, additional_args, graph);
                }
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
                // Mini-backtracking: scan ahead to find the first FixedSeq anchor.
                // Collect any intermediate FixedLen intervals so we can slice them
                // out of the "before anchor" region after the search.
                let mut intermediate_fixed: Vec<&GeometryMeta> = Vec::new();
                let mut anchor: Option<&GeometryMeta> = None;
                let mut lookahead = geometry_iter.clone();

                while let Some(piece) = lookahead.next() {
                    let (_, piece_size, _, _) = piece.unpack();
                    match piece_size {
                        IntervalShape::FixedSeq(_) => {
                            anchor = Some(piece);
                            break;
                        }
                        IntervalShape::FixedLen(_) => {
                            intermediate_fixed.push(piece);
                        }
                        _ => {
                            // Hit another variable-length segment; stop scanning.
                            break;
                        }
                    }
                }

                if let Some(anchor_gp) = anchor {
                    // Consume all the pieces we scanned past.
                    for _ in 0..intermediate_fixed.len() {
                        geometry_iter.next();
                    }
                    // Consume the anchor itself.
                    geometry_iter.next();

                    // Use interpret_dual to search for the anchor and create
                    // the 3-way split: prev_label (before anchor), anchor, next_label (after).
                    anchor_gp.interpret_dual(
                        gp,
                        &mut label,
                        additional_args,
                        graph,
                        &mut min_start_idx,
                    );

                    // Now slice the prev_label region into the intermediate FixedLen pieces.
                    // After interpret_dual, the "before anchor" region is labeled as either
                    // the unbounded segment's label or `{cur_label}_l`.
                    if !intermediate_fixed.is_empty() {
                        let (_, _, unbounded_label, _) = gp.unpack();
                        let seq_name = label.first().unwrap();
                        let (_, cur_label) = labels(&label);

                        let prev_region_label = if let Some(l) = unbounded_label {
                            format!("{seq_name}{l}")
                        } else {
                            format!("{cur_label}{NEXT_LEFT}")
                        };

                        // We need to slice from the RIGHT side of prev_region_label
                        // because the FixedLen pieces are adjacent to the anchor.
                        // E.g., for "r: u[2] b[2] f[ACG]" with read "XXTTGGACG":
                        //   prev_region_label = "XXTTGG" (6 chars)
                        //   We want: discard "XX", then umi="TT", bc="GG"
                        // So we cut from the right: bc is last 2, umi is 2 before that.

                        // Calculate total length of intermediate fixed segments
                        let total_fixed_len: usize = intermediate_fixed
                            .iter()
                            .map(|p| {
                                if let IntervalShape::FixedLen(S(len, _)) = p.unpack().1 {
                                    len
                                } else {
                                    0
                                }
                            })
                            .sum();

                        // First, cut off the "true unbounded" prefix from the fixed portion.
                        // The fixed portion is the last `total_fixed_len` bases of prev_region_label.
                        // Use negative index to cut from the right.
                        let fixed_region_label = format!("{cur_label}_fixed");
                        graph.add(cut_node(
                            into_transform_expr(
                                &prev_region_label,
                                [VOID_LABEL, fixed_region_label.as_str()],
                            ),
                            Expr::from(-(total_fixed_len as isize)),
                        ));

                        // Now slice the fixed_region_label into individual pieces from left to right.
                        let mut slice_label = fixed_region_label.clone();
                        for (i, piece) in intermediate_fixed.iter().enumerate() {
                            let (piece_type, piece_size, piece_label, piece_stack) = piece.unpack();
                            if let IntervalShape::FixedLen(S(len, _)) = piece_size {
                                let this_label = if let Some(l) = piece_label {
                                    format!("{seq_name}{l}")
                                } else {
                                    format!("{cur_label}_p{i}")
                                };

                                let is_last = i == intermediate_fixed.len() - 1;
                                let next_slice_label = if is_last {
                                    VOID_LABEL.to_string()
                                } else {
                                    format!("{cur_label}_slice{}", i + 1)
                                };

                                graph.add(cut_node(
                                    into_transform_expr(
                                        &slice_label,
                                        [this_label.as_str(), next_slice_label.as_str()],
                                    ),
                                    Expr::from(len),
                                ));
                                graph.add(valid_label_length(&this_label, len, None));

                                // Execute any stack functions on this piece
                                let mut stack = piece_stack;
                                if piece_type == IntervalKind::Discard {
                                    stack.push(S(CompiledFunction::Remove, (0..1).into()));
                                }
                                execute_stack(stack, &this_label, &piece_size, additional_args, graph);

                                slice_label = next_slice_label;
                            }
                        }
                    }
                } else {
                    // No FixedSeq anchor found; treat as a normal unbounded segment.
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
            panic!("Expected {n} additional arguments with `--additional` tag. Found only {len}.",)
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
        IntervalShape::FixedSeq(S(v, _)) => v.len(),
        IntervalShape::FixedLen(S(n, _)) => *n,
        IntervalShape::RangedLen(S((_, b), _)) => *b,
        IntervalShape::UnboundedLen => 0,
    };

    for S(fn_, _) in stack.into_iter().rev() {
        match fn_ {
            // Anchor is a modifier handled in interpret(), skip here
            CompiledFunction::Anchor => continue,
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
                    Expr::from(expr::attr(format!("{label}.{MAPPED}"))).not(),
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
                    Expr::from(expr::attr(format!("{label}.{MAPPED}"))).not(),
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
            stack.push(S(CompiledFunction::Remove, (0..1).into()));
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
            stack.push(S(CompiledFunction::Remove, (0..1).into()));
        }

        // execute the requisite process here
        match size.clone() {
            IntervalShape::FixedSeq(S(seq, _)) => {
                // Check if Anchor is on the stack - search for anchor
                let has_anchor = stack.iter().any(|s| matches!(s.0, CompiledFunction::Anchor));

                if has_anchor {
                    // Remove Anchor from stack (it's a modifier, not an operation)
                    stack.retain(|s| !matches!(s.0, CompiledFunction::Anchor));

                    // For anchor_relative, we need 3-way split: before, anchor, after
                    // This allows extracting preceding elements relative to the anchor position
                    let prev_label = format!("{cur_label}{NEXT_LEFT}");
                    let labels = vec![prev_label.as_str(), this_label.as_str(), &next_label];

                    // Determine match type based on what's on stack
                    let match_type = if let Some(S(CompiledFunction::Hamming(n), _)) = stack.last() {
                        let n = *n;
                        stack.pop();
                        HammingSearch(Threshold::Count(seq.len() - n))
                    } else {
                        ExactSearch
                    };

                    graph.add(match_node(
                        Patterns::from_strs([Nucleotide::as_str(&seq)]),
                        &init_label,
                        labels,
                        match_type,
                    ));
                    graph.add(retain_node(expr::label_exists(this_label.clone())));
                } else {
                    // Original prefix matching behavior
                    let labels = vec![this_label.as_str(), &next_label];

                    // Determine how we should perform the prefix match based on the top of the stack:
                    let match_type = if let Some(S(CompiledFunction::Hamming(n), _)) = stack.last() {
                        let n = *n;
                        stack.pop();
                        HammingPrefix(Threshold::Count(seq.len() - n))
                    } else if !stack.is_empty() {
                        PrefixAln {
                            identity: 1.0,
                            overlap: 1.0,
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
            }
            IntervalShape::FixedLen(S(len, _)) => {
                // Fixed-position cut behavior
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
    // Check for Hamming on the stack
    let hamming_dist = if let Some(S(CompiledFunction::Hamming(n), _)) = stack.last() {
        let n = *n;
        stack.pop();
        Some(n)
    } else {
        None
    };

    // Logic based on predecessor type
    match offset_len {
        Some(offset) => {
            match hamming_dist {
                Some(n) => HammingBoundedMatch {
                    threshold: Threshold::Count(seq_len - n),
                    from: *range_start,
                    to: *range_start + seq_len + offset,
                },
                None => ExactBoundedMatch {
                    from: *range_start,
                    to: *range_start + seq_len + offset,
                },
            }
        }
        None => {
            match hamming_dist {
                Some(n) => HammingSearch(Threshold::Count(seq_len - n)),
                None => ExactSearch,
            }
        }
    }
}
