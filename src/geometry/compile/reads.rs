use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        layout::normalize_layout,
        utils::*,
    },
    parser::{Expr, Function, IntervalShape, Read},
    S,
};

/// Observable result of bounded layout normalization for one input read.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReadLayoutReport {
    pub read_index: usize,
    pub used_layout_algebra: bool,
    pub alternatives: usize,
    pub max_segments: usize,
}

pub type StandardizedLayoutAlternatives = Vec<Vec<Vec<GeometryMeta>>>;

/// One statically resolved occurrence of a public capture name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CaptureOccurrence {
    /// One-based occurrence index used by EFGDL `<capture[N]>` references.
    pub index: usize,
    /// Input read containing this occurrence.
    pub read_index: usize,
    /// Short internal interval name produced by compiler lowering.
    pub physical_label: String,
}

/// Public capture name to its source-ordered, fixed-cardinality occurrences.
pub type CaptureRegistry = BTreeMap<String, Vec<CaptureOccurrence>>;

pub type CompiledReadLayouts = (
    HashMap<String, GeometryMeta>,
    Geometry,
    StandardizedLayoutAlternatives,
    Vec<ReadLayoutReport>,
    CaptureRegistry,
);

fn capture_name(expr: &Expr) -> Option<&S<String>> {
    match expr {
        Expr::Label(label) | Expr::LabeledGeomPiece(label, _) => Some(label),
        Expr::Function(_, inner) => capture_name(&inner.0),
        _ => None,
    }
}

fn indexed_capture_span(expr: &Expr) -> Option<crate::Span> {
    match expr {
        Expr::IndexedLabel(_, index) => Some(index.1),
        Expr::Function(_, inner) => indexed_capture_span(&inner.0),
        _ => None,
    }
}

fn capture_counts(alternative: &[S<Expr>]) -> Result<BTreeMap<String, usize>, Error> {
    let mut counts = BTreeMap::new();
    for S(expr, _) in alternative {
        if let Some(span) = indexed_capture_span(expr) {
            return Err(Error {
                span,
                msg: "indexed captures may only be referenced in output reads or output headers"
                    .to_string(),
            });
        }
        if let Some(S(label, _)) = capture_name(expr) {
            *counts.entry(label.clone()).or_default() += 1;
        }
    }
    Ok(counts)
}

fn rewrite_capture(expr: &mut Expr, physical_label: &str) {
    match expr {
        Expr::Label(S(label, _)) | Expr::LabeledGeomPiece(S(label, _), _) => {
            *label = physical_label.to_string();
        }
        Expr::Function(_, inner) => rewrite_capture(&mut inner.0, physical_label),
        _ => unreachable!("capture-bearing expression changed after normalization"),
    }
}

fn allocate_physical_label(reserved: &mut HashSet<String>, next_id: &mut usize) -> String {
    loop {
        let candidate = format!("__c{next_id}");
        *next_id += 1;
        if reserved.insert(candidate.clone()) {
            return candidate;
        }
    }
}

fn lower_indexed_captures(
    normalized: &mut [super::layout::NormalizedLayout],
    reads: &[S<Read>],
    definitions: &mut HashMap<String, GeometryMeta>,
    efgdl_version: usize,
) -> Result<CaptureRegistry, Error> {
    let mut reserved = definitions.keys().cloned().collect::<HashSet<_>>();
    for layout in normalized.iter() {
        for alternative in &layout.alternatives {
            for S(expr, _) in alternative {
                if let Some(S(label, _)) = capture_name(expr) {
                    reserved.insert(label.clone());
                }
            }
        }
    }

    let mut registry = CaptureRegistry::new();
    let mut next_physical_id = 0usize;
    for (read_offset, layout) in normalized.iter_mut().enumerate() {
        let Some(canonical) = layout.alternatives.first() else {
            continue;
        };
        let canonical_counts = capture_counts(canonical)?;
        if efgdl_version < 2 {
            if let Some((label, count)) = canonical_counts.iter().find(|(_, count)| **count > 1) {
                return Err(Error {
                    span: reads[read_offset].1,
                    msg: format!(
                        "`{label}` has already been used; repeated named captures require `header {{ efgdl = 2 }}` and indexed output references ({count} occurrences found)"
                    ),
                });
            }
        }
        for (alternative_offset, alternative) in layout.alternatives.iter().enumerate().skip(1) {
            let counts = capture_counts(alternative)?;
            if counts != canonical_counts {
                return Err(Error {
                    span: reads[read_offset].1,
                    msg: format!(
                        "all alternatives for read {} must expose the same capture cardinality; alternative 1 has {:?}, alternative {} has {:?}",
                        reads[read_offset].0.index.0,
                        canonical_counts,
                        alternative_offset + 1,
                        counts
                    ),
                });
            }
        }

        let mut physical_by_public = BTreeMap::<String, Vec<String>>::new();
        for (public, count) in canonical_counts {
            if let Some(existing) = registry.get(&public) {
                return Err(Error {
                    span: reads[read_offset].1,
                    msg: format!(
                        "capture `{public}` was already bound in input read {}; capture names must be unique across reads",
                        existing[0].read_index
                    ),
                });
            }
            let physical = if count == 1 {
                vec![public.clone()]
            } else {
                (0..count)
                    .map(|_| allocate_physical_label(&mut reserved, &mut next_physical_id))
                    .collect::<Vec<_>>()
            };
            let occurrences = physical
                .iter()
                .enumerate()
                .map(|(offset, physical_label)| CaptureOccurrence {
                    index: offset + 1,
                    read_index: reads[read_offset].0.index.0,
                    physical_label: physical_label.clone(),
                })
                .collect::<Vec<_>>();
            registry.insert(public.clone(), occurrences);
            physical_by_public.insert(public, physical);
        }

        for alternative in &mut layout.alternatives {
            let mut seen = BTreeMap::<String, usize>::new();
            for S(expr, _) in alternative {
                let Some(S(public, _)) = capture_name(expr).cloned() else {
                    continue;
                };
                let occurrence = seen.entry(public.clone()).or_default();
                let physical = &physical_by_public[&public][*occurrence];
                *occurrence += 1;
                if physical != &public {
                    rewrite_capture(expr, physical);
                    if let Some(source) = definitions.get(&public).cloned() {
                        let mut lowered = source;
                        lowered.expr.0.label = Some(physical.clone());
                        definitions.insert(physical.clone(), lowered);
                    }
                }
            }
        }
    }
    Ok(registry)
}

pub fn validate_geometry(
    map: &HashMap<String, GeometryMeta>,
    geom: &[(Interval, usize)],
) -> Result<(), Error> {
    let mut expect_next = vec![
        ReturnType::FixedLen,
        ReturnType::FixedSeq,
        ReturnType::Unbounded,
        ReturnType::Ranged,
    ];

    // Track whether we're in a "needs anchor" state: after Unbounded or Ranged
    // followed by FixedLen, we must eventually see a FixedSeq before the geometry
    // ends or another variable-length segment appears.
    // Note: Unbounded/Ranged at the END of a read is valid (consumes the rest).
    let mut needs_anchor = false;
    let mut last_was_variable = false;

    for (interval, _) in geom {
        let gm = match interval {
            Interval::Named(l) => map.get(l).unwrap(),
            Interval::Temporary(gp_) => gp_,
        };

        let S(gp, span) = &gm.expr;

        let type_ = match gp.size {
            IntervalShape::FixedSeq(_) => ReturnType::FixedSeq,
            IntervalShape::FixedLen(_) => ReturnType::FixedLen,
            IntervalShape::RangedLen(_) => ReturnType::Ranged,
            IntervalShape::UnboundedLen => ReturnType::Unbounded,
        };

        if !expect_next.contains(&type_) {
            return Err(Error {
                span: *span,
                msg: format!("Ambiguous Geometry: expected {expect_next:?}, found: {type_}"),
            });
        }

        // Check if we're in needs_anchor state and hit another variable-length segment
        if needs_anchor && matches!(type_, ReturnType::Unbounded | ReturnType::Ranged) {
            return Err(Error {
                span: *span,
                msg: "Ambiguous Geometry: variable-length segment after Unbounded/Ranged requires a FixedSeq anchor in between".to_string(),
            });
        }

        expect_next = match type_ {
            ReturnType::FixedLen => {
                // FixedLen after a variable-length segment means we need an anchor
                if last_was_variable {
                    needs_anchor = true;
                }
                last_was_variable = false;
                vec![
                    ReturnType::FixedLen,
                    ReturnType::FixedSeq,
                    ReturnType::Unbounded,
                    ReturnType::Ranged,
                ]
            }
            ReturnType::FixedSeq => {
                // FixedSeq resolves any pending anchor requirement
                needs_anchor = false;
                last_was_variable = false;
                vec![
                    ReturnType::FixedLen,
                    ReturnType::FixedSeq,
                    ReturnType::Unbounded,
                    ReturnType::Ranged,
                ]
            }
            ReturnType::Ranged => {
                last_was_variable = true;
                vec![ReturnType::FixedSeq, ReturnType::FixedLen]
            }
            ReturnType::Unbounded => {
                last_was_variable = true;
                vec![ReturnType::FixedLen, ReturnType::FixedSeq]
            }
            ReturnType::Void => unreachable!(),
        };
    }

    // At the end of the geometry, if we still need an anchor, it's invalid
    // (This happens when we have Unbounded/Ranged followed by FixedLen but no FixedSeq)
    if needs_anchor {
        // Get the span of the last interval for the error message
        if let Some((interval, _)) = geom.last() {
            let gm = match interval {
                Interval::Named(l) => map.get(l).unwrap(),
                Interval::Temporary(gp_) => gp_,
            };
            let S(_, span) = &gm.expr;
            return Err(Error {
                span: *span,
                msg: "Ambiguous Geometry: variable-length segment followed by fixed-length segments requires a FixedSeq anchor".to_string(),
            });
        }
    }

    Ok(())
}

pub fn standardize_geometry(
    map: HashMap<String, GeometryMeta>,
    geometry: Geometry,
) -> Vec<Vec<GeometryMeta>> {
    let mut std_geom: Vec<Vec<GeometryMeta>> = Vec::new();

    for read in geometry {
        let mut geom: Vec<GeometryMeta> = Vec::new();
        for interval in read {
            match interval {
                (Interval::Named(l), _) => geom.push(map[&l].clone()),
                (Interval::Temporary(gp), _) => geom.push(gp),
            }
        }

        std_geom.push(geom);
    }

    std_geom
}

// this should take both reads and parse them. Allowing for combined label_map
fn compile_linear_reads(
    S(reads, _): S<Vec<S<Read>>>,
    mut map: HashMap<String, GeometryMeta>,
) -> Result<(HashMap<String, GeometryMeta>, Geometry), Error> {
    let mut err: Option<Error> = None;
    let mut geometry: Geometry = Vec::new();
    let mut labels: Vec<String> = Vec::new();

    // create a vector of labels which have already been used. help with errors
    // labels and span!

    'outer_outer: for S(
        Read {
            index: S(num, _),
            exprs: read_exprs,
            ..
        },
        _,
    ) in reads
    {
        let mut read_geom: Vec<(Interval, usize)> = Vec::new();
        'outer: for mut expr in read_exprs {
            let mut spanned_geom_piece: Option<S<GeometryPiece>> = None;
            let mut compiled_stack: Vec<S<CompiledFunction>> = Vec::new();
            let mut stack: Vec<S<Function>> = Vec::new();
            let mut label: Option<String> = None;

            'inner: loop {
                match expr.0 {
                    Expr::Function(inner_fn, gp) => {
                        expr = gp.unboxed();
                        stack.push(inner_fn);
                    }
                    Expr::LabeledGeomPiece(S(l, span), gp) => {
                        if labels.contains(&l) || map.contains_key(&l) {
                            err = Some(Error {
                                span,
                                msg: format!("Variable: {l}, already defined above."),
                            });

                            break 'outer;
                        }

                        label = Some(l.clone());

                        // maybe return from this and add labeled elements to the map outside of this
                        // would have to unpack labeled values to validate at the end
                        expr = gp.unboxed();

                        for fn_ in stack {
                            compiled_stack.push(compile_fn(fn_, expr.clone())?);
                        }

                        break 'inner;
                    }
                    Expr::Label(S(ref l, ref span)) => {
                        if labels.contains(l) {
                            err = Some(Error {
                                span: *span,
                                msg: format!(
                                    "`{l}` has already been used. Cannot use same variable more than once."
                                ),
                            });

                            break 'outer;
                        } else if let Some(inner_expr) = map.get(l) {
                            label = Some(l.clone());
                            labels.push(l.clone());
                            spanned_geom_piece = Some(inner_expr.expr.clone());

                            for fn_ in stack {
                                compiled_stack.push(compile_fn(
                                    fn_,
                                    S(
                                        Expr::GeomPiece(
                                            inner_expr.expr.0.type_,
                                            inner_expr.expr.0.size.clone(),
                                        ),
                                        inner_expr.expr.1,
                                    ),
                                )?);
                            }

                            compiled_stack = compiled_stack
                                .clone()
                                .into_iter()
                                .chain(inner_expr.stack.clone())
                                .collect::<Vec<_>>();

                            break 'inner;
                        } else {
                            err = Some(Error {
                                span: *span,
                                msg: format!("No variable declared with label: {l}"),
                            });

                            break 'outer;
                        }
                    }
                    Expr::GeomPiece(_, _) => {
                        for fn_ in stack {
                            compiled_stack.push(compile_fn(fn_, expr.clone())?);
                        }

                        break 'inner;
                    }
                    _ => break 'inner,
                }
            }

            // if spanned geom piece is set then expr should not matter
            let expr_span = expr.1;
            let spanned_gp = if let Some(spanned_gp) = spanned_geom_piece {
                spanned_gp
            } else if let S(Expr::GeomPiece(type_, size), span) = expr {
                S(
                    GeometryPiece {
                        type_,
                        size,
                        label: label.clone(),
                    },
                    span,
                )
            } else {
                err = Some(Error {
                    span: expr_span,
                    msg: "this expression cannot appear directly in a read; expected a \
                          geometry piece, label, or function application"
                        .to_string(),
                });

                break 'outer;
            };

            let gm = GeometryMeta {
                expr: spanned_gp,
                stack: compiled_stack,
            };

            if let Err(e) = gm.validate_expr() {
                err = Some(e);
                break 'outer;
            }

            if let Some(l) = label {
                map.insert(l.clone(), gm);
                read_geom.push((Interval::Named(l), num));
            } else {
                read_geom.push((Interval::Temporary(gm), num));
            }
        }

        if let Err(e) = validate_geometry(&map, &read_geom) {
            err = Some(e);
            break 'outer_outer;
        }

        geometry.push(read_geom);
    }

    if let Some(e) = err {
        return Err(e);
    }

    Ok((map, geometry))
}

fn named_labels(geometry: &[(Interval, usize)]) -> Vec<&str> {
    let mut labels = geometry
        .iter()
        .filter_map(|(interval, _)| match interval {
            Interval::Named(label) => Some(label.as_str()),
            Interval::Temporary(_) => None,
        })
        .collect::<Vec<_>>();
    labels.sort_unstable();
    labels.dedup();
    labels
}

/// Compile bounded EFGDL input-layout alternatives while retaining the
/// canonical first alternative for backward-compatible consumers.
pub fn compile_read_layouts(
    S(reads, reads_span): S<Vec<S<Read>>>,
    mut definitions: HashMap<String, GeometryMeta>,
    efgdl_version: usize,
) -> Result<CompiledReadLayouts, Error> {
    let mut normalized = Vec::with_capacity(reads.len());
    let mut reports = Vec::with_capacity(reads.len());
    for S(read, _) in &reads {
        let layout = normalize_layout(read.exprs.clone(), efgdl_version)?;
        reports.push(ReadLayoutReport {
            read_index: read.index.0,
            used_layout_algebra: layout.uses_algebra,
            alternatives: layout.alternatives.len(),
            max_segments: layout.alternatives.iter().map(Vec::len).max().unwrap_or(0),
        });
        normalized.push(layout);
    }
    let capture_registry =
        lower_indexed_captures(&mut normalized, &reads, &mut definitions, efgdl_version)?;

    let make_reads_through = |last_index: usize, selected: &[usize]| {
        reads
            .iter()
            .take(last_index + 1)
            .enumerate()
            .map(|(index, S(read, span))| {
                let mut read = read.clone();
                read.exprs = normalized[index].alternatives[selected[index]].clone();
                S(read, *span)
            })
            .collect::<Vec<_>>()
    };

    let canonical_selection = vec![0; reads.len()];
    let canonical_reads = make_reads_through(reads.len().saturating_sub(1), &canonical_selection);
    let (canonical_map, canonical_geometry) =
        compile_linear_reads(S(canonical_reads, reads_span), definitions.clone())?;

    let mut standardized_alternatives = Vec::with_capacity(reads.len());
    for (read_index, layout) in normalized.iter().enumerate() {
        let canonical_labels = named_labels(&canonical_geometry[read_index]);
        let mut read_alternatives = Vec::with_capacity(layout.alternatives.len());

        for alternative_index in 0..layout.alternatives.len() {
            let mut selection = canonical_selection.clone();
            selection[read_index] = alternative_index;
            let branch_reads = make_reads_through(read_index, &selection);
            let (branch_map, branch_geometry) =
                compile_linear_reads(S(branch_reads, reads_span), definitions.clone())?;
            let branch = &branch_geometry[read_index];
            let branch_labels = named_labels(branch);
            if branch_labels != canonical_labels {
                return Err(Error {
                    span: reads[read_index].1,
                    msg: format!(
                        "all alternatives for read {} must expose the same labels; alternative 1 has {:?}, alternative {} has {:?}",
                        reads[read_index].0.index.0,
                        canonical_labels,
                        alternative_index + 1,
                        branch_labels
                    ),
                });
            }
            for label in &canonical_labels {
                if canonical_map.get(*label) != branch_map.get(*label) {
                    return Err(Error {
                        span: reads[read_index].1,
                        msg: format!(
                            "label `{label}` must have the same interval and functions in every alternative for read {}",
                            reads[read_index].0.index.0
                        ),
                    });
                }
            }

            let standardized = branch
                .iter()
                .map(|(interval, _)| match interval {
                    Interval::Named(label) => branch_map[label].clone(),
                    Interval::Temporary(meta) => meta.clone(),
                })
                .collect();
            read_alternatives.push(standardized);
        }
        standardized_alternatives.push(read_alternatives);
    }

    Ok((
        canonical_map,
        canonical_geometry,
        standardized_alternatives,
        reports,
        capture_registry,
    ))
}

/// Backward-compatible compiler for legacy linear reads.
pub fn compile_reads(
    reads: S<Vec<S<Read>>>,
    map: HashMap<String, GeometryMeta>,
) -> Result<(HashMap<String, GeometryMeta>, Geometry), Error> {
    compile_read_layouts(reads, map, 1).map(|(map, geometry, _, _, _)| (map, geometry))
}
