use std::{ops::Not, path::PathBuf, str::FromStr};

use antisequence::{
    graph::MatchType::{
        self, EditBoundedMatch, EditSearch, ExactBoundedMatch, ExactSearch, HammingBoundedMatch,
        HammingSearch, PrefixAln,
    },
    *,
};
use expr::Expr;
use graph::{
    Graph,
    MatchType::{Edit, EditPrefix, Exact, ExactPrefix, Hamming, HammingPrefix},
    ProjectOp, ProjectPart, SelectOp, SwitchOp, Threshold, TryOrientationOp,
};

use crate::{
    compile::{
        functions::CompiledFunction,
        utils::{
            CompiledHeaderMode, GeometryMeta, GeometryPiece, HeaderSegment, HeaderTransformation,
            ReadTransformation, TransformSegment,
        },
        CompiledData, ElementAnnotations, ElementId,
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

fn add_output_projection(
    graph: &mut Graph,
    output_index: usize,
    parts: &[TransformSegment],
    terminal_projection: bool,
) {
    let target_str_type = StrType::Seq(output_index as u8);
    let same_lane = parts.iter().all(|part| match part {
        TransformSegment::Label(name) => antisequence::expr::Label::new(name.as_bytes())
            .map(|label| label.str_type == target_str_type)
            .unwrap_or(false),
        TransformSegment::Literal(_) => true,
    });

    if terminal_projection && same_lane {
        let project_parts = parts.iter().map(|part| match part {
            TransformSegment::Label(name) => ProjectPart::Label(
                antisequence::expr::Label::new(name.as_bytes())
                    .expect("compiled transformation labels must be valid"),
            ),
            TransformSegment::Literal(bytes) => ProjectPart::Literal(bytes.clone()),
        });
        graph.add(ProjectOp::with_parts(target_str_type, project_parts));
        return;
    }

    let mut expressions = parts.iter().map(|part| match part {
        TransformSegment::Label(name) => Expr::from(antisequence::expr::label(name)),
        TransformSegment::Literal(bytes) => Expr::from(bytes.clone()),
    });
    let Some(first) = expressions.next() else {
        return;
    };
    let concatenated = expressions.fold(first, Expr::concat);
    let seq_name = format!("seq{output_index}.*");
    graph.add(set_node(LabelOrAttr::Label(&seq_name), concatenated));
}

fn concatenate_header_parts(parts: &[HeaderSegment]) -> Option<Expr> {
    let mut expressions = parts.iter().map(|part| match part {
        HeaderSegment::Literal(bytes) => Expr::from(bytes.clone()),
        HeaderSegment::Label(name) => Expr::from(antisequence::expr::label(name)),
    });
    let first = expressions.next()?;
    Some(expressions.fold(first, Expr::concat))
}

fn add_output_header(graph: &mut Graph, output_index: usize, header: &HeaderTransformation) {
    let Some(template) = concatenate_header_parts(&header.parts) else {
        return;
    };
    let name = format!("name{output_index}.*");
    let original = || Expr::from(antisequence::expr::label(&name));
    let expression = match header.mode {
        CompiledHeaderMode::Append => original().concat(template),
        CompiledHeaderMode::Prepend => template.concat(original()),
        CompiledHeaderMode::Replace => template,
    };
    graph.add(set_node(LabelOrAttr::Label(&name), expression));
}

fn add_output_transformations(
    graph: &mut Graph,
    transformations: &[ReadTransformation],
    terminal_projection: bool,
) {
    // Any output header may reference a label from any input lane. Construct
    // every name before a terminal sequence projection can discard mappings
    // needed by a later output's template.
    for (i, transformation) in transformations.iter().enumerate() {
        if let Some(header) = &transformation.header {
            add_output_header(graph, i + 1, header);
        }
    }

    for (i, transformation) in transformations.iter().enumerate() {
        add_output_projection(graph, i + 1, &transformation.sequence, terminal_projection);
    }
}

impl<'a> CompiledData {
    pub fn interpret<'b: 'a>(&'a self, graph: &'a mut Graph, additional_args: &[&str]) {
        let Self {
            geometry,
            transformation,
            element_annotations,
            ..
        } = self;

        for (i, read_geometry) in geometry.iter().enumerate() {
            let read_idx = i + 1; // 1-based

            // Check if this read has a match_ori(either) annotation.
            let has_match_ori = element_annotations.iter().any(|ea| {
                ea.element_id == ElementId::Read(read_idx)
                    && ea.annotations.iter().any(|S(ann, _)| {
                        ann.name.0 == "match_ori"
                            && ann.args.first().map(|a| a.0.as_str()) == Some("either")
                    })
            });

            if has_match_ori {
                // Build geometry into a separate inner graph, then wrap
                // it in TryOrientationOp so the read is tried in both
                // forward and reverse-complement orientations.
                let mut inner = Graph::new();
                interpret_geometry(
                    &mut inner,
                    read_geometry,
                    &format!("seq{}.", read_idx),
                    additional_args,
                    element_annotations,
                    read_idx,
                );
                let read_idx_u8 = u8::try_from(read_idx)
                    .expect("read index must fit in u8 (validated at compile time)");
                graph.add(TryOrientationOp::new(inner, read_idx_u8, b"ori"));
            } else {
                interpret_geometry(
                    graph,
                    read_geometry,
                    &format!("seq{}.", read_idx),
                    additional_args,
                    element_annotations,
                    read_idx,
                );
            }
        }

        // Apply output transformation(s).
        // If a match_block is present, use SelectOp to conditionally apply
        // different transformations based on a runtime attribute value.
        // Otherwise, apply the single transformation unconditionally.
        if let Some(match_block) = &self.match_block {
            let attr_name = format!("seq{}.*.{}", match_block.read_ref, match_block.attr);

            // Helper: build a subgraph for one arm's transformation.
            // For each label in the arm's transformation, check if the arm's
            // compiled map has extra functions compared to the base geometry.
            // If so, apply those functions (e.g., revcomp) before the label
            // rearrangement.
            let build_arm_graph = |arm_transformation: &[ReadTransformation],
                                   arm_map: &std::collections::HashMap<
                String,
                crate::compile::utils::GeometryMeta,
            >|
             -> Graph {
                let mut arm_graph = Graph::new();

                // Apply arm-specific per-label functions (diff vs base map).
                for read_transform in arm_transformation.iter() {
                    for segment in read_transform.sequence.iter() {
                        let TransformSegment::Label(full_label) = segment else {
                            continue;
                        };
                        // full_label is like "seq1.bc" -- extract just "bc"
                        let short_label = full_label.split('.').nth(1).unwrap_or(full_label);

                        let arm_stack_len = arm_map
                            .get(short_label)
                            .map(|gm| gm.stack.len())
                            .unwrap_or(0);
                        let base_stack_len = match_block
                            .base_map
                            .get(short_label)
                            .map(|gm| gm.stack.len())
                            .unwrap_or(0);

                        // If the arm has extra functions, apply them.
                        if arm_stack_len > base_stack_len {
                            if let Some(arm_gm) = arm_map.get(short_label) {
                                // The extra functions are at the front of the
                                // arm's stack (compile_transformation prepends).
                                let extra_count = arm_stack_len - base_stack_len;
                                for S(fn_, _) in arm_gm.stack.iter().take(extra_count).rev() {
                                    arm_graph.add(set_node(
                                        LabelOrAttr::Label(full_label),
                                        fn_.clone().to_expr(full_label, &None),
                                    ));
                                }
                            }
                        }
                    }
                }

                // SwitchOp evaluates every selector before running an arm, so
                // the arm may safely end in a destructive terminal projection.
                add_output_transformations(&mut arm_graph, arm_transformation, true);

                arm_graph
            };

            let fw_graph = build_arm_graph(&match_block.fw_transformation, &match_block.fw_map);
            let rc_graph = build_arm_graph(&match_block.rc_transformation, &match_block.rc_map);

            // Route once before either terminal arm can discard `ori`.
            let fw_selector = Expr::from(antisequence::expr::attr(&attr_name)).eq(b"fw".to_vec());
            let rc_selector = Expr::from(antisequence::expr::attr(&attr_name)).eq(b"rc".to_vec());
            graph.add(SwitchOp::new([
                (fw_selector, fw_graph),
                (rc_selector, rc_graph),
            ]));
        } else if let Some(transformation) = transformation {
            add_output_transformations(graph, transformation, true);
        };
    }
}

fn interpret_geometry(
    graph: &mut Graph,
    geometry: &[GeometryMeta],
    init_label: &str,
    additional_args: &[&str],
    _element_annotations: &[ElementAnnotations],
    _read_idx: usize,
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
                let lookahead = geometry_iter.clone();

                for piece in lookahead {
                    let (_, piece_size, _, piece_stack) = piece.unpack();
                    match piece_size {
                        IntervalShape::FixedSeq(_) => {
                            // Check if this FixedSeq has Anchor modifier
                            let has_anchor = piece_stack
                                .iter()
                                .any(|s| matches!(s.0, CompiledFunction::Anchor));
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

                        // Get Hamming or Edit distance if specified
                        let match_type = if let Some(S(CompiledFunction::Hamming(n), _)) =
                            anchor_stack.last()
                        {
                            let n = *n;
                            anchor_stack.pop();
                            HammingSearch(Threshold::Count(seq.len() - n))
                        } else if let Some(S(CompiledFunction::Edit(n), _)) = anchor_stack.last() {
                            let n = *n;
                            anchor_stack.pop();
                            EditSearch(Threshold::Count(n))
                        } else {
                            ExactSearch
                        };

                        // Search for anchor with 3-way split
                        // For anchor(), search_label is the original read (position 0)
                        // For anchor_relative(), search_label is the current position
                        graph.add(
                            match_node(
                                Patterns::from_strs([Nucleotide::as_str(&seq)]),
                                &search_label,
                                vec![&prev_label, &anchor_this_label, &next_label_str],
                                match_type,
                            )
                            .retain_label_present(&anchor_this_label),
                        );

                        // Execute any remaining stack functions on anchor
                        execute_stack(
                            anchor_stack,
                            &anchor_this_label,
                            &anchor_size,
                            additional_args,
                            graph,
                        );

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
                                execute_stack(
                                    stack,
                                    &this_label,
                                    &piece_size,
                                    additional_args,
                                    graph,
                                );

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
                let lookahead = geometry_iter.clone();

                for piece in lookahead {
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
                                execute_stack(
                                    stack,
                                    &this_label,
                                    &piece_size,
                                    additional_args,
                                    graph,
                                );

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
    let mut ambiguity_policy = None;
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
            CompiledFunction::AmbiguityPolicy(policy) => {
                ambiguity_policy = Some(policy);
                continue;
            }
            CompiledFunction::Remove => {
                graph.add(trim_node([antisequence::expr::label(label)]));
            }
            CompiledFunction::Hamming(_) => {
                panic!("Hamming requires to be bound to a sequence cannot operate in isolation")
            }
            CompiledFunction::Edit(_) => {
                panic!("Edit requires to be bound to a sequence cannot operate in isolation")
            }
            CompiledFunction::Map(file, fns) => {
                let file_path = parse_additional_args(file, additional_args);
                let patterns = parse_file_match(
                    file_path,
                    ambiguity_policy.take().unwrap_or(AmbiguityPolicy::NoMatch),
                );

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
                let patterns = parse_file_match(
                    file_path,
                    ambiguity_policy.take().unwrap_or(AmbiguityPolicy::NoMatch),
                );

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
            CompiledFunction::MapWithEdit(file, fns, edit_dist) => {
                let file_path = parse_additional_args(file, additional_args);
                let patterns = parse_file_match(
                    file_path,
                    ambiguity_policy.take().unwrap_or(AmbiguityPolicy::NoMatch),
                );

                map(label, patterns, Edit(Threshold::Count(edit_dist)), graph);

                let mut fallback_graph = Graph::new();
                execute_stack(fns, label, size, additional_args, &mut fallback_graph);

                graph.add(SelectOp::new(
                    Expr::from(expr::attr(format!("{label}.{MAPPED}"))).not(),
                    fallback_graph,
                ));
            }
            CompiledFunction::FilterWithinDist(file, mismatch) => {
                let file_path = parse_additional_args(file, additional_args);
                let patterns = parse_file_filter(
                    file_path,
                    ambiguity_policy.take().unwrap_or(AmbiguityPolicy::Accept),
                );

                graph.add(
                    match_node(
                        patterns,
                        label,
                        vec![label],
                        Hamming(Threshold::Count(interval_length - mismatch)),
                    )
                    .retain_attribute_absent([label, ".", FILTER].concat()),
                );
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
                let has_anchor = stack
                    .iter()
                    .any(|s| matches!(s.0, CompiledFunction::Anchor));

                if has_anchor {
                    // Remove Anchor from stack (it's a modifier, not an operation)
                    stack.retain(|s| !matches!(s.0, CompiledFunction::Anchor));

                    // For anchor_relative, we need 3-way split: before, anchor, after
                    // This allows extracting preceding elements relative to the anchor position
                    let prev_label = format!("{cur_label}{NEXT_LEFT}");
                    let labels = vec![prev_label.as_str(), this_label.as_str(), &next_label];

                    // Determine match type based on what's on stack
                    let match_type = if let Some(S(CompiledFunction::Hamming(n), _)) = stack.last()
                    {
                        let n = *n;
                        stack.pop();
                        HammingSearch(Threshold::Count(seq.len() - n))
                    } else if let Some(S(CompiledFunction::Edit(n), _)) = stack.last() {
                        let n = *n;
                        stack.pop();
                        EditSearch(Threshold::Count(n))
                    } else {
                        ExactSearch
                    };

                    graph.add(
                        match_node(
                            Patterns::from_strs([Nucleotide::as_str(&seq)]),
                            &init_label,
                            labels,
                            match_type,
                        )
                        .retain_label_present(&this_label),
                    );
                } else {
                    // Original prefix matching behavior
                    let labels = vec![this_label.as_str(), &next_label];

                    // Determine how we should perform the prefix match based on the top of the stack:
                    let match_type = if let Some(S(CompiledFunction::Hamming(n), _)) = stack.last()
                    {
                        let n = *n;
                        stack.pop();
                        HammingPrefix(Threshold::Count(seq.len() - n))
                    } else if let Some(S(CompiledFunction::Edit(n), _)) = stack.last() {
                        let n = *n;
                        stack.pop();
                        EditPrefix(Threshold::Count(n))
                    } else if !stack.is_empty() {
                        PrefixAln {
                            identity: 1.0,
                            overlap: 1.0,
                        }
                    } else {
                        ExactPrefix
                    };

                    graph.add(
                        match_node(
                            Patterns::from_strs([Nucleotide::as_str(&seq)]),
                            &init_label,
                            labels,
                            match_type,
                        )
                        .retain_label_present(&this_label),
                    );
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

                graph.add(
                    match_node(
                        Patterns::from_strs([Nucleotide::as_str(&seq)]),
                        &init_label,
                        vec![&prev_label, &this_label, &next_label],
                        match_type,
                    )
                    .retain_label_present(&this_label),
                );

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
    // Check for Hamming or Edit on the stack
    let hamming_dist = if let Some(S(CompiledFunction::Hamming(n), _)) = stack.last() {
        let n = *n;
        stack.pop();
        Some(n)
    } else {
        None
    };

    let edit_dist = if let Some(S(CompiledFunction::Edit(n), _)) = stack.last() {
        let n = *n;
        stack.pop();
        Some(n)
    } else {
        None
    };

    // Logic based on predecessor type
    match offset_len {
        Some(offset) => {
            if let Some(n) = edit_dist {
                EditBoundedMatch {
                    threshold: Threshold::Count(n),
                    from: *range_start,
                    to: *range_start + seq_len + offset,
                }
            } else if let Some(n) = hamming_dist {
                HammingBoundedMatch {
                    threshold: Threshold::Count(seq_len - n),
                    from: *range_start,
                    to: *range_start + seq_len + offset,
                }
            } else {
                ExactBoundedMatch {
                    from: *range_start,
                    to: *range_start + seq_len + offset,
                }
            }
        }
        None => {
            if let Some(n) = edit_dist {
                EditSearch(Threshold::Count(n))
            } else if let Some(n) = hamming_dist {
                HammingSearch(Threshold::Count(seq_len - n))
            } else {
                ExactSearch
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::compile_geom;

    #[test]
    fn test_labels_single() {
        let (init, cur) = labels(&["seq1."]);
        assert_eq!(init, "seq1.*");
        assert_eq!(cur, "seq1.");
    }

    #[test]
    fn test_labels_multiple() {
        let (init, cur) = labels(&["seq1.", "_r"]);
        assert_eq!(init, "seq1._r");
        assert_eq!(cur, "seq1._r");
    }

    #[test]
    fn test_interpret_simple_barcode_read() {
        let data = compile_geom("1{b[16]u[10]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
        // Graph should have nodes for cut+validate barcode, cut+validate umi, cut+set read
    }

    #[test]
    fn test_interpret_two_reads() {
        let data = compile_geom("1{b[16]u[10]r:}2{r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_discard() {
        let data = compile_geom("1{x[5]b[16]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_fixed_seq() {
        let data = compile_geom("1{f[ACGT]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_hamming() {
        let data = compile_geom("1{hamming(f[ACGT], 1)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_edit() {
        let data = compile_geom("1{edit(f[ACGT], 1)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_rev() {
        let data = compile_geom("1{rev(b[16])r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_revcomp() {
        let data = compile_geom("1{revcomp(b[16])r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_trunc() {
        let data = compile_geom("1{trunc(b[16], 2)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_trunc_to() {
        let data = compile_geom("1{trunc_to(b[16], 10)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_remove() {
        let data = compile_geom("1{remove(f[ACGT])r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_pad() {
        let data = compile_geom("1{pad(b[16], 4, A)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_pad_to() {
        let data = compile_geom("1{pad_to(b[16], 20, A)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_trunc_left() {
        let data = compile_geom("1{trunc_left(b[16], 2)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_trunc_to_left() {
        let data = compile_geom("1{trunc_to_left(b[16], 10)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_pad_left() {
        let data = compile_geom("1{pad_left(b[16], 4, T)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_pad_to_left() {
        let data = compile_geom("1{pad_to_left(b[16], 20, G)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_remove_hamming() {
        let data = compile_geom("1{remove(hamming(f[CAG], 1))r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_labels() {
        let data = compile_geom("1{b<bc1>[16]u<umi>[10]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_transformation() {
        let data = compile_geom(
            "1{b<bc>[16]u<umi>[10]r<read>:}2{r<read2>:}->1{<bc><umi>}2{<read2>}".to_string(),
        )
        .unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_fixed_output_sequence() {
        let data = compile_geom(
            "header { efgdl = 2 } 1{b<bc>[4]r<read>:}->1{f[AC]<bc>f[T]<read>}".to_string(),
        )
        .unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_composition() {
        let data = compile_geom("1{trunc_to(rev(b[16]), 10)r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_complex_two_read() {
        let data = compile_geom("1{b[16]u[12]r:}2{x[10]b[8]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_with_definitions() {
        let data = compile_geom("bc1 = b[16]\n1{<bc1>r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_get_match_type_exact_search() {
        let mut stack = vec![];
        let mut range_start = 0;
        let mt = get_match_type(None, &mut stack, 4, &mut range_start);
        assert!(matches!(mt, ExactSearch));
    }

    #[test]
    fn test_get_match_type_hamming_search() {
        let mut stack = vec![S(CompiledFunction::Hamming(1), (0..1).into())];
        let mut range_start = 0;
        let mt = get_match_type(None, &mut stack, 4, &mut range_start);
        assert!(matches!(mt, HammingSearch(_)));
    }

    #[test]
    fn test_get_match_type_edit_search() {
        let mut stack = vec![S(CompiledFunction::Edit(1), (0..1).into())];
        let mut range_start = 0;
        let mt = get_match_type(None, &mut stack, 4, &mut range_start);
        assert!(matches!(mt, EditSearch(_)));
    }

    #[test]
    fn test_get_match_type_exact_bounded() {
        let mut stack = vec![];
        let mut range_start = 0;
        let mt = get_match_type(Some(4), &mut stack, 4, &mut range_start);
        assert!(matches!(mt, ExactBoundedMatch { .. }));
    }

    #[test]
    fn test_get_match_type_hamming_bounded() {
        let mut stack = vec![S(CompiledFunction::Hamming(1), (0..1).into())];
        let mut range_start = 0;
        let mt = get_match_type(Some(4), &mut stack, 4, &mut range_start);
        assert!(matches!(mt, HammingBoundedMatch { .. }));
    }

    #[test]
    fn test_get_match_type_edit_bounded() {
        let mut stack = vec![S(CompiledFunction::Edit(1), (0..1).into())];
        let mut range_start = 0;
        let mt = get_match_type(Some(4), &mut stack, 4, &mut range_start);
        assert!(matches!(mt, EditBoundedMatch { .. }));
    }

    #[test]
    fn test_parse_additional_args_by_index() {
        let args = vec!["file1.txt", "file2.txt"];
        let path = parse_additional_args("0".to_string(), &args);
        assert_eq!(path, PathBuf::from("file1.txt"));
    }

    #[test]
    fn test_parse_additional_args_by_path() {
        let args: Vec<&str> = vec![];
        let path = parse_additional_args("/some/path.txt".to_string(), &args);
        assert_eq!(path, PathBuf::from("/some/path.txt"));
    }

    #[test]
    fn test_interpret_unbounded_then_fixed_seq() {
        // r: followed by f[ACGT] triggers interpret_dual (unbounded -> FixedSeq search)
        let data = compile_geom("1{r:f[ACGT]b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_unbounded_then_hamming_fixed_seq() {
        // r: followed by hamming(f[ACGT], 1) triggers interpret_dual with hamming search
        let data = compile_geom("1{r:hamming(f[ACGT], 1)b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_unbounded_then_edit_fixed_seq() {
        // r: followed by edit(f[ACGT], 1) triggers interpret_dual with edit search
        let data = compile_geom("1{r:edit(f[ACGT], 1)b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_anchor_relative() {
        // anchor_relative triggers the anchor path
        let data = compile_geom("1{r:anchor_relative(f[ACGT])b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_anchor_relative_with_hamming() {
        let data =
            compile_geom("1{r:anchor_relative(hamming(f[ACGT], 1))b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_labeled_discard() {
        let data = compile_geom("1{x<skip>[5]b[16]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_multiple_fixed_then_unbounded() {
        let data = compile_geom("1{b[16]u[10]x[5]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_three_reads() {
        let data = compile_geom("1{b[16]r:}2{u[10]r:}3{r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_fixed_seq_then_unbounded() {
        // f[ACGT] then r: -- FixedSeq followed by unbounded
        let data = compile_geom("1{f[ACGT]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_hamming_then_read() {
        let data = compile_geom("1{hamming(f[ACGT], 1)b[16]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_edit_then_read() {
        let data = compile_geom("1{edit(f[ACGT], 1)b[16]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_remove_fixed_seq() {
        let data = compile_geom("1{remove(f[ACGT])b[16]r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_unbounded_fixed_seq_with_intermediate() {
        // r: u[10] b[16] f[ACGT] -- unbounded, then 2 fixed-len intermediates, then anchor
        let data = compile_geom("1{r:u[10]f[ACGT]b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_fixed_anchor_with_intermediate_fixed() {
        // b[8] u[10] anchor_relative(f[ACGT]) r:
        let data = compile_geom("1{b[8]u[10]anchor_relative(f[ACGT])r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_anchor_relative_with_edit() {
        let data = compile_geom("1{r:anchor_relative(edit(f[ACGT], 1))b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_execute_stack_various_functions() {
        // Test execute_stack through interpret with various function compositions
        let data = compile_geom("1{revcomp(rev(b[16]))r:}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }

    #[test]
    fn test_interpret_unbounded_with_labeled_anchor() {
        let data = compile_geom("1{r:f<linker>[ACGT]b[16]}".to_string()).unwrap();
        let mut graph = Graph::new();
        data.interpret(&mut graph, &[]);
    }
}
