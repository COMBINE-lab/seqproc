use std::collections::HashMap;

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::{Annotation, AnnotationValueArg, Definition, Expr, ResourceRef},
    S,
};
use antisequence::{AmbiguityPolicy, PositionAmbiguityPolicy};

fn parse_position_ambiguity_policy(
    annotation: &Annotation,
    span: crate::Span,
) -> Result<PositionAmbiguityPolicy, Error> {
    let call_args = annotation
        .args
        .iter()
        .skip(1)
        .map(|value| {
            S(
                AnnotationValueArg {
                    name: None,
                    value: value.clone(),
                },
                value.1,
            )
        })
        .collect::<Vec<_>>();
    let (variant, args): (&str, &[S<AnnotationValueArg>]) = if let Some(value) =
        annotation.value.as_ref()
    {
        (value.0.variant.0.as_str(), &value.0.args)
    } else if !annotation.args.is_empty() {
        (annotation.args[0].0.as_str(), &call_args)
    } else {
        return Err(Error {
            span,
            msg: "write #[position_policy = leftmost] or #[position_policy(leftmost)]".to_string(),
        });
    };
    let no_args = || {
        if args.is_empty() {
            Ok(())
        } else {
            Err(Error {
                span,
                msg: format!("position policy `{variant}` does not accept arguments"),
            })
        }
    };

    match variant {
        "leftmost" | "best" => {
            no_args()?;
            // Every matcher first minimizes distance. `best` makes that
            // invariant explicit and uses the deterministic leftmost
            // placement only to resolve a remaining equal-best tie.
            Ok(PositionAmbiguityPolicy::Leftmost)
        }
        "rightmost" => {
            no_args()?;
            Ok(PositionAmbiguityPolicy::Rightmost)
        }
        "quality" => {
            let min_delta = if args.is_empty() {
                1
            } else if args.len() == 1 {
                let arg = &args[0].0;
                if let Some(name) = &arg.name {
                    if name.0 != "min_delta" {
                        return Err(Error {
                            span,
                            msg: format!(
                                "unknown `{}` argument for position policy `quality`; expected `min_delta`",
                                name.0
                            ),
                        });
                    }
                }
                let value = arg.value.0.parse::<u64>().map_err(|_| Error {
                    span,
                    msg: "`min_delta` for position policy `quality` must be a non-negative integer"
                        .to_string(),
                })?;
                u8::try_from(value).map_err(|_| Error {
                    span,
                    msg: "`min_delta` for position policy `quality` must be between 0 and 255"
                        .to_string(),
                })?
            } else {
                return Err(Error {
                    span,
                    msg: "position policy `quality` accepts at most one `min_delta` argument"
                        .to_string(),
                });
            };
            Ok(PositionAmbiguityPolicy::Quality { min_delta })
        }
        "no_match" => {
            no_args()?;
            Ok(PositionAmbiguityPolicy::NoMatch)
        }
        "error" => {
            no_args()?;
            Ok(PositionAmbiguityPolicy::Error)
        }
        variant => Err(Error {
            span,
            msg: format!(
                "unknown position policy `{variant}`; expected best, leftmost, rightmost, quality, no_match, or error"
            ),
        }),
    }
}

fn parse_ambiguity_policy(
    annotation: &Annotation,
    span: crate::Span,
) -> Result<AmbiguityPolicy, Error> {
    let call_args = annotation
        .args
        .iter()
        .skip(1)
        .map(|value| {
            S(
                AnnotationValueArg {
                    name: None,
                    value: value.clone(),
                },
                value.1,
            )
        })
        .collect::<Vec<_>>();
    let (variant, args): (&str, &[S<AnnotationValueArg>]) =
        if let Some(value) = annotation.value.as_ref() {
            (value.0.variant.0.as_str(), &value.0.args)
        } else if !annotation.args.is_empty() {
            (annotation.args[0].0.as_str(), &call_args)
        } else {
            return Err(Error {
                span,
                msg: "write #[ambig_policy = no_match] or #[ambig_policy(no_match)]".to_string(),
            });
        };
    let no_args = || {
        if args.is_empty() {
            Ok(())
        } else {
            Err(Error {
                span,
                msg: format!("ambiguity policy `{variant}` does not accept arguments"),
            })
        }
    };
    let numeric_arg = |expected_name: &str, default: u64| -> Result<u64, Error> {
        if args.is_empty() {
            return Ok(default);
        }
        if args.len() != 1 {
            return Err(Error {
                span,
                msg: format!(
                    "ambiguity policy `{variant}` accepts at most one `{expected_name}` argument"
                ),
            });
        }
        let arg = &args[0].0;
        if let Some(name) = &arg.name {
            if name.0 != expected_name {
                return Err(Error {
                    span,
                    msg: format!(
                        "unknown `{}` argument for ambiguity policy `{variant}`; expected `{expected_name}`",
                        name.0
                    ),
                });
            }
        }
        arg.value.0.parse::<u64>().map_err(|_| Error {
            span,
            msg: format!(
                "`{expected_name}` for ambiguity policy `{variant}` must be a non-negative integer"
            ),
        })
    };

    match variant {
        "accept" => {
            no_args()?;
            Ok(AmbiguityPolicy::Accept)
        }
        "no_match" => {
            no_args()?;
            Ok(AmbiguityPolicy::NoMatch)
        }
        "first" => {
            no_args()?;
            Ok(AmbiguityPolicy::First)
        }
        "error" => {
            no_args()?;
            Ok(AmbiguityPolicy::Error)
        }
        "random" => Ok(AmbiguityPolicy::Random {
            seed: numeric_arg("seed", 0)?,
        }),
        "quality" => {
            let min_delta = numeric_arg("min_delta", 1)?;
            let min_delta = u8::try_from(min_delta).map_err(|_| Error {
                span,
                msg: "`min_delta` for ambiguity policy `quality` must be between 0 and 255"
                    .to_string(),
            })?;
            Ok(AmbiguityPolicy::Quality { min_delta })
        }
        _ => Err(Error {
            span,
            msg: format!(
                "unknown ambiguity policy `{variant}`; expected accept, no_match, first, quality, random, or error"
            ),
        }),
    }
}

/// validate definitions there should be no labels, just labeled geom pieces and functions
fn validate_definition(mut expr: S<Expr>, label: &str) -> Result<GeometryMeta, Error> {
    let mut stack: Vec<S<CompiledFunction>> = vec![];

    loop {
        // Peel off function wrapper
        match expr.0 {
            Expr::Function(fn_, gp) => {
                expr = gp.unboxed(); // Unbox the geometry piece
                stack.push(compile_fn(fn_, expr.clone())?); // Compile the function we just peeled off
            }
            Expr::Label(_) => {
                return Err(Error {
                    // Err since there was a label in the definition block
                    span: expr.1,
                    msg: "Unexpected label in definition block".to_string(),
                });
            }
            Expr::Self_ => {
                // Err since 'self' is reserved for 'map' transformation
                return Err(Error {
                    span: expr.1,
                    msg: "Unexpected reference to 'self' in definition. 'self' is reserved for 'map' transformation.".to_string(),
                });
            }
            Expr::LabeledGeomPiece(S(label, span), _) => {
                // Err since there was a labeled interval in the definition block
                // Inline label bindings inside defs are forbidden; the label should be the left-hand definition identifier
                return Err(Error {
                    span,
                    msg: format!("Unexpected labeled interval in a definition block. Remove <{label}>, to make this a valid definition.")
                });
            }
            _ => break,
        }
    }

    let expr_span = expr.1;
    let gp = if let S(Expr::GeomPiece(type_, size), span) = expr {
        S(
            GeometryPiece {
                type_,
                size,
                label: Some(label.to_owned()), // Attach the label to the geometry piece as the expr base
            },
            span,
        )
    } else {
        return Err(Error {
            span: expr_span,
            msg: format!(
                "definition `{label}` must be a geometry piece, optionally wrapped in \
                 functions; indexed or compound references are not valid here"
            ),
        });
    };

    let gp = GeometryMeta { expr: gp, stack }; // Now we have the geometry piece and the stack of functions

    gp.validate_expr().map(|()| gp) // Validate the function composition against the geometry piece
}

/// Converts all matching-modifier annotations on a definition into
/// compiled functions that should be appended to the definition's stack.
///
/// Supported annotations:
/// - `#[hamming(N)]` -> `CompiledFunction::Hamming(N)`
/// - `#[edit(N)]`    -> `CompiledFunction::Edit(N)`
/// - `#[search(relative)]` -> `CompiledFunction::Anchor`
///
/// Returns an error if a recognized annotation has malformed arguments
/// (e.g., `#[hamming(abc)]`, `#[edit()]`, `#[search(absolute)]`).
///
/// Multiple annotations compose (stacking). For same-name annotations,
/// last-wins is enforced by resolve_annotations upstream.
fn annotations_to_compiled_functions(
    annotations: &[S<Annotation>],
) -> Result<Vec<S<CompiledFunction>>, Error> {
    let mut result = Vec::new();

    for S(ann, span) in annotations.iter() {
        match ann.name.0.as_str() {
            "hamming" => {
                if ann.value.is_some() {
                    return Err(Error {
                        span: *span,
                        msg: "`hamming` uses call syntax; write #[hamming(N)]".to_string(),
                    });
                }
                let arg = ann.args.first().ok_or_else(|| Error {
                    span: *span,
                    msg: "#[hamming(N)] requires a numeric argument, e.g. #[hamming(1)]"
                        .to_string(),
                })?;
                let n = arg.0.parse::<usize>().map_err(|_| Error {
                    span: *span,
                    msg: format!(
                        "#[hamming({})] argument must be a non-negative integer",
                        arg.0
                    ),
                })?;
                result.push(S(CompiledFunction::Hamming(n), *span));
            }
            "edit" => {
                if ann.value.is_some() {
                    return Err(Error {
                        span: *span,
                        msg: "`edit` uses call syntax; write #[edit(N)]".to_string(),
                    });
                }
                let arg = ann.args.first().ok_or_else(|| Error {
                    span: *span,
                    msg: "#[edit(N)] requires a numeric argument, e.g. #[edit(1)]".to_string(),
                })?;
                let n = arg.0.parse::<usize>().map_err(|_| Error {
                    span: *span,
                    msg: format!("#[edit({})] argument must be a non-negative integer", arg.0),
                })?;
                result.push(S(CompiledFunction::Edit(n), *span));
            }
            "search" => {
                if ann.value.is_some() {
                    return Err(Error {
                        span: *span,
                        msg: "`search` uses call syntax; write #[search(relative)]".to_string(),
                    });
                }
                let arg = ann.args.first().map(|S(a, _)| a.as_str());
                if arg != Some("relative") {
                    return Err(Error {
                        span: *span,
                        msg: format!(
                            "#[search({})] is not a recognized search mode; \
                             expected #[search(relative)]",
                            arg.unwrap_or("")
                        ),
                    });
                }
                result.push(S(CompiledFunction::Anchor, *span));
            }
            "anchor_set" => {
                if ann.value.is_some() || ann.args.len() != 1 {
                    return Err(Error {
                        span: *span,
                        msg: "`anchor_set` uses call syntax with one path: #[anchor_set($0)]"
                            .to_string(),
                    });
                }
                let value = &ann.args[0].0;
                let resource = if let Some(name) = value.strip_prefix('$') {
                    match name.parse::<usize>() {
                        Ok(index) => ResourceRef::Positional(index),
                        Err(_) => ResourceRef::Named(name.to_owned()),
                    }
                } else {
                    ResourceRef::Literal(value.clone())
                };
                result.push(S(CompiledFunction::AnchorSet(resource), *span));
            }
            "ambig_policy" => result.push(S(
                CompiledFunction::AmbiguityPolicy(parse_ambiguity_policy(ann, *span)?),
                *span,
            )),
            "position_policy" => result.push(S(
                CompiledFunction::PositionAmbiguityPolicy(parse_position_ambiguity_policy(
                    ann, *span,
                )?),
                *span,
            )),
            unknown => {
                return Err(Error {
                    span: *span,
                    msg: format!(
                        "unknown definition annotation `{unknown}`; expected hamming, edit, search, anchor_set, ambig_policy, or position_policy"
                    ),
                });
            }
        }
    }

    Ok(result)
}

/// Check whether annotation-derived compiled functions conflict with
/// functions already in the definition's stack from old function-call syntax.
///
/// Conflict rules:
/// - A distance annotation (#[hamming(N)] or #[edit(N)]) conflicts if the
///   stack already contains Hamming(_) or Edit(_) from function syntax.
/// - A search annotation (#[search(relative)]) conflicts if the stack
///   already contains Anchor from anchor_relative() function syntax.
/// - A distance annotation on a definition that uses anchor_relative() is
///   NOT a conflict -- that's the intended transitional syntax:
///   `#[edit(6)] l1 = anchor_relative(f[SEQ])`.
fn check_annotation_conflicts(
    stack: &[S<CompiledFunction>],
    ann_fns: &[S<CompiledFunction>],
) -> bool {
    let stack_has_distance = stack
        .iter()
        .any(|S(f, _)| matches!(f, CompiledFunction::Hamming(_) | CompiledFunction::Edit(_)));
    let stack_has_anchor = stack
        .iter()
        .any(|S(f, _)| matches!(f, CompiledFunction::Anchor));

    for S(af, _) in ann_fns {
        match af {
            CompiledFunction::Hamming(_) | CompiledFunction::Edit(_) => {
                if stack_has_distance {
                    return true;
                }
            }
            CompiledFunction::Anchor if stack_has_anchor => return true,
            _ => {}
        }
    }
    false
}

fn validate_ambiguity_policy_target(
    stack: &[S<CompiledFunction>],
    definition: &str,
    span: crate::Span,
) -> Result<(), Error> {
    let policies: Vec<_> = stack
        .iter()
        .filter_map(|S(function, _)| match function {
            CompiledFunction::AmbiguityPolicy(policy) => Some(*policy),
            _ => None,
        })
        .collect();
    if policies.is_empty() {
        return Ok(());
    }
    if policies.len() > 1 {
        return Err(Error {
            span,
            msg: format!("definition `{definition}` specifies `ambig_policy` more than once"),
        });
    }

    let target = stack.iter().find_map(|S(function, _)| match function {
        CompiledFunction::Map(..) => Some("map"),
        CompiledFunction::MapWithMismatch(..) => Some("map_with_mismatch"),
        CompiledFunction::MapWithEdit(..) => Some("map_with_edit"),
        CompiledFunction::FilterWithinDist(..) => Some("filter_within_dist"),
        CompiledFunction::AnchorSet(..) => Some("anchor_set"),
        _ => None,
    });
    let Some(target) = target else {
        return Err(Error {
            span,
            msg: format!(
                "#[ambig_policy = ...] on definition `{definition}` requires a map, filter, or anchor_set operation"
            ),
        });
    };

    if matches!(policies[0], AmbiguityPolicy::Quality { .. })
        && matches!(target, "map_with_edit" | "map")
    {
        return Err(Error {
            span,
            msg: format!(
                "quality ambiguity resolution on `{target}` is not supported; use equal-length Hamming matching"
            ),
        });
    }
    if matches!(policies[0], AmbiguityPolicy::Quality { .. })
        && target == "anchor_set"
        && stack
            .iter()
            .any(|S(function, _)| matches!(function, CompiledFunction::Edit(_)))
    {
        return Err(Error {
            span,
            msg: "quality ambiguity resolution on `anchor_set` requires exact or Hamming search; edit-distance gap qualities are not defined"
                .to_string(),
        });
    }
    Ok(())
}

fn validate_position_policy_target(
    stack: &[S<CompiledFunction>],
    definition: &str,
    span: crate::Span,
) -> Result<(), Error> {
    let policies = stack
        .iter()
        .filter(|S(function, _)| matches!(function, CompiledFunction::PositionAmbiguityPolicy(_)))
        .count();
    if policies == 0 {
        return Ok(());
    }
    if policies > 1 {
        return Err(Error {
            span,
            msg: format!("definition `{definition}` specifies `position_policy` more than once"),
        });
    }
    if !stack
        .iter()
        .any(|S(function, _)| matches!(function, CompiledFunction::Anchor))
    {
        return Err(Error {
            span,
            msg: format!(
                "#[position_policy = ...] on definition `{definition}` requires #[search(relative)]"
            ),
        });
    }
    if stack.iter().any(|S(function, _)| {
        matches!(
            function,
            CompiledFunction::PositionAmbiguityPolicy(PositionAmbiguityPolicy::Quality { .. })
        )
    }) && stack
        .iter()
        .any(|S(function, _)| matches!(function, CompiledFunction::Edit(_)))
    {
        return Err(Error {
            span,
            msg: format!(
                "position quality resolution on definition `{definition}` requires exact or Hamming search; edit-distance gap qualities are not defined"
            ),
        });
    }
    Ok(())
}

pub fn compile_definitions(
    S(defs, _): S<Vec<S<Definition>>>,
) -> Result<(HashMap<String, GeometryMeta>, Vec<String>), Error> {
    let mut map = HashMap::new();
    let mut warnings: Vec<String> = Vec::new();

    let mut err: Option<Error> = None;

    for S(
        Definition {
            annotations: def_annotations,
            label: S(label_str, label_span),
            expr,
        },
        _,
    ) in defs
    {
        let res = validate_definition(expr, &label_str);
        if let Err(e) = res {
            err = Some(e);
            break;
        }
        let mut gm = res.unwrap();

        // LANG-DEPRECATE: Check for old function-call matching modifiers
        // in the stack and emit deprecation warnings.
        for S(fn_, _) in &gm.stack {
            match fn_ {
                CompiledFunction::Hamming(n) => {
                    warnings.push(format!(
                        "deprecated: hamming({}) on definition '{}'; \
                         use #[hamming({})] annotation instead",
                        n, label_str, n
                    ));
                }
                CompiledFunction::Edit(n) => {
                    warnings.push(format!(
                        "deprecated: edit({}) on definition '{}'; \
                         use #[edit({})] annotation instead",
                        n, label_str, n
                    ));
                }
                CompiledFunction::Anchor => {
                    warnings.push(format!(
                        "deprecated: anchor_relative() on definition '{}'; \
                         use #[search(relative)] annotation instead",
                        label_str
                    ));
                }
                _ => {}
            }
        }

        // LANG-MIGRATE-HAMMING / LANG-MIGRATE-EDIT / LANG-MIGRATE-SEARCH:
        // Convert matching-modifier annotations into compiled functions and
        // inject them into the definition's stack. This makes, e.g.,
        //   #[search(relative)] #[edit(6)] l1 = f[SEQ]
        // compile identically to:
        //   l1 = anchor_relative(edit(f[SEQ], 6))
        // Definition-level annotations are also extracted separately in
        // compile() (mod.rs) into element_annotations for hierarchical
        // scoping.
        let ann_fns = match annotations_to_compiled_functions(&def_annotations) {
            Ok(fns) => fns,
            Err(e) => {
                err = Some(e);
                break;
            }
        };
        if !ann_fns.is_empty() {
            let annotation_distance_count = ann_fns
                .iter()
                .filter(|S(function, _)| {
                    matches!(
                        function,
                        CompiledFunction::Hamming(_) | CompiledFunction::Edit(_)
                    )
                })
                .count();
            if annotation_distance_count > 1 {
                err = Some(Error {
                    span: label_span,
                    msg: format!(
                        "definition `{label_str}` must choose exactly one distance metric; #[hamming] and #[edit] cannot be combined"
                    ),
                });
                break;
            }
            // Detect conflict: definition already has a matching modifier
            // from old function-call syntax (e.g., hamming(), edit(),
            // anchor_relative()), and the annotation tries to add another.
            if check_annotation_conflicts(&gm.stack, &ann_fns) {
                err = Some(Error {
                    span: label_span,
                    msg: format!(
                        "definition '{}' has conflicting matching modifiers: \
                         the old function syntax (hamming/edit/anchor_relative) \
                         and annotation syntax (#[hamming], #[edit], #[search]) \
                         cannot be combined on the same definition; \
                         use one or the other",
                        label_str
                    ),
                });
                break;
            }
            gm.stack.extend(ann_fns);
            if let Err(e) = validate_ambiguity_policy_target(&gm.stack, &label_str, label_span) {
                err = Some(e);
                break;
            }
            if let Err(e) = validate_position_policy_target(&gm.stack, &label_str, label_span) {
                err = Some(e);
                break;
            }
            // Re-validate after injecting annotation-derived functions.
            if let Err(e) = gm.validate_expr() {
                err = Some(e);
                break;
            }
        }

        if map.insert(label_str.clone(), gm).is_some() {
            err = Some(Error {
                // span labels
                span: label_span,
                msg: format!("Repeated label in definition block: \"{label_str}\" already defined"),
            });
            break;
        }
    }

    if let Some(e) = err {
        return Err(e);
    }

    Ok((map, warnings))
}

#[cfg(test)]
mod tests {
    use crate::execute::compile_geom;

    #[test]
    fn test_compile_definitions_simple() {
        let data = compile_geom("bc1 = b[16]\n1{<bc1>r:}".to_string()).unwrap();
        assert_eq!(data.geometry.len(), 1);
    }

    #[test]
    fn test_compile_definitions_multiple() {
        let data = compile_geom("bc1 = b[16]\numi1 = u[10]\n1{<bc1><umi1>r:}".to_string()).unwrap();
        assert_eq!(data.geometry.len(), 1);
    }

    #[test]
    fn test_compile_definitions_with_function() {
        let data = compile_geom("bc1 = rev(b[16])\n1{<bc1>r:}".to_string()).unwrap();
        assert_eq!(data.geometry.len(), 1);
    }

    #[test]
    fn test_hamming_annotation_compiles_like_function() {
        // LANG-MIGRATE-HAMMING: The new annotation syntax
        //   #[hamming(1)] anchor = f[CATAGC]
        // should compile to the same geometry as the old function syntax
        //   anchor = hamming(f[CATAGC], 1)
        // Specifically, the compiled function stack should contain Hamming(1).
        use super::CompiledFunction;

        let old = compile_geom("anchor = hamming(f[CATAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string())
            .expect("old hamming syntax should compile");
        let new =
            compile_geom("#[hamming(1)] anchor = f[CATAGC]\n1{x:<anchor>x:}2{r:}".to_string())
                .expect("new hamming annotation syntax should compile");

        // Find the anchor geometry piece (labeled "anchor") in each
        let old_anchor = old.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("anchor"))
            .expect("old geometry should have 'anchor' label");
        let new_anchor = new.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("anchor"))
            .expect("new geometry should have 'anchor' label");

        // The old syntax should have Hamming(1) in its stack
        assert!(
            old_anchor
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Hamming(1))),
            "old syntax stack should have Hamming(1), got: {:?}",
            old_anchor.stack
        );
        // The new annotation syntax should also have Hamming(1)
        assert!(
            new_anchor
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Hamming(1))),
            "new annotation syntax stack should have Hamming(1), got: {:?}",
            new_anchor.stack
        );
        // The function stacks should be identical (ignoring spans which differ
        // because old syntax and annotation syntax have different source positions).
        let old_fns: Vec<_> = old_anchor.stack.iter().map(|s| &s.0).collect();
        let new_fns: Vec<_> = new_anchor.stack.iter().map(|s| &s.0).collect();
        assert_eq!(
            old_fns, new_fns,
            "function stacks should be identical: old={:?}, new={:?}",
            old_fns, new_fns,
        );
    }

    #[test]
    fn test_hamming_annotation_on_anchor_relative() {
        // LANG-MIGRATE-HAMMING: anchor_relative + hamming annotation
        //   #[hamming(1)] l1 = anchor_relative(f[CAGAGC])
        // should compile identically to:
        //   l1 = anchor_relative(hamming(f[CAGAGC], 1))
        use super::CompiledFunction;

        let old = compile_geom(
            "l1 = anchor_relative(hamming(f[CAGAGC], 1))\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
        )
        .expect("old anchor+hamming syntax should compile");
        let new = compile_geom(
            "#[hamming(1)] l1 = anchor_relative(f[CAGAGC])\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
        )
        .expect("new anchor+hamming annotation syntax should compile");

        let old_l1 = old.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .expect("old geometry should have 'l1' label");
        let new_l1 = new.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .expect("new geometry should have 'l1' label");

        // Old should have both Anchor and Hamming(1)
        assert!(
            old_l1
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Hamming(1))),
            "old stack should have Hamming(1), got: {:?}",
            old_l1.stack
        );
        // New annotation syntax should also have Hamming(1)
        assert!(
            new_l1
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Hamming(1))),
            "new annotation stack should have Hamming(1), got: {:?}",
            new_l1.stack
        );
        let old_fns: Vec<_> = old_l1.stack.iter().map(|s| &s.0).collect();
        let new_fns: Vec<_> = new_l1.stack.iter().map(|s| &s.0).collect();
        assert_eq!(
            old_fns, new_fns,
            "function stacks should be identical: old={:?}, new={:?}",
            old_fns, new_fns,
        );
    }

    #[test]
    fn test_edit_annotation_compiles_like_function() {
        // LANG-MIGRATE-EDIT: #[edit(1)] anchor = f[CATAGC] should compile
        // identically to anchor = edit(f[CATAGC], 1).
        use super::CompiledFunction;

        let old = compile_geom("anchor = edit(f[CATAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string())
            .expect("old edit syntax should compile");
        let new = compile_geom("#[edit(1)] anchor = f[CATAGC]\n1{x:<anchor>x:}2{r:}".to_string())
            .expect("new edit annotation syntax should compile");

        let old_anchor = old.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("anchor"))
            .expect("old geometry should have 'anchor'");
        let new_anchor = new.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("anchor"))
            .expect("new geometry should have 'anchor'");

        assert!(
            new_anchor
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Edit(1))),
            "new annotation stack should have Edit(1), got: {:?}",
            new_anchor.stack
        );
        let old_fns: Vec<_> = old_anchor.stack.iter().map(|s| &s.0).collect();
        let new_fns: Vec<_> = new_anchor.stack.iter().map(|s| &s.0).collect();
        assert_eq!(old_fns, new_fns);
    }

    #[test]
    fn test_edit_annotation_on_anchor_relative() {
        // LANG-MIGRATE-EDIT: #[edit(6)] l1 = anchor_relative(f[SEQ]) should
        // compile identically to l1 = anchor_relative(edit(f[SEQ], 6)).
        use super::CompiledFunction;

        let old = compile_geom(
            "l1 = anchor_relative(edit(f[CAGAGC], 1))\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
        )
        .expect("old anchor+edit should compile");
        let new = compile_geom(
            "#[edit(1)] l1 = anchor_relative(f[CAGAGC])\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
        )
        .expect("new anchor+edit annotation should compile");

        let old_l1 = old.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .unwrap();
        let new_l1 = new.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .unwrap();

        assert!(
            new_l1
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Edit(1))),
            "new stack should have Edit(1), got: {:?}",
            new_l1.stack
        );
        let old_fns: Vec<_> = old_l1.stack.iter().map(|s| &s.0).collect();
        let new_fns: Vec<_> = new_l1.stack.iter().map(|s| &s.0).collect();
        assert_eq!(old_fns, new_fns);
    }

    #[test]
    fn test_search_relative_annotation_compiles_like_anchor_relative() {
        // LANG-MIGRATE-SEARCH: #[search(relative)] l1 = f[CAGAGC] should
        // compile identically to l1 = anchor_relative(f[CAGAGC]).
        use super::CompiledFunction;

        let old =
            compile_geom("l1 = anchor_relative(f[CAGAGC])\n1{x[2]b[8]<l1>r:}2{r:}".to_string())
                .expect("old anchor_relative syntax should compile");
        let new =
            compile_geom("#[search(relative)] l1 = f[CAGAGC]\n1{x[2]b[8]<l1>r:}2{r:}".to_string())
                .expect("new search(relative) annotation should compile");

        let old_l1 = old.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .unwrap();
        let new_l1 = new.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .unwrap();

        assert!(
            new_l1
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Anchor)),
            "new stack should have Anchor, got: {:?}",
            new_l1.stack
        );
        let old_fns: Vec<_> = old_l1.stack.iter().map(|s| &s.0).collect();
        let new_fns: Vec<_> = new_l1.stack.iter().map(|s| &s.0).collect();
        assert_eq!(old_fns, new_fns);
    }

    #[test]
    fn test_stacked_search_edit_annotation() {
        // LANG-MIGRATE-SEARCH + EDIT stacking:
        //   #[search(relative)] #[edit(1)] l1 = f[CAGAGC]
        // should compile identically to:
        //   l1 = anchor_relative(edit(f[CAGAGC], 1))
        use super::CompiledFunction;

        let old = compile_geom(
            "l1 = anchor_relative(edit(f[CAGAGC], 1))\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
        )
        .expect("old stacked syntax should compile");
        let new = compile_geom(
            "#[search(relative)] #[edit(1)] l1 = f[CAGAGC]\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
        )
        .expect("new stacked annotation syntax should compile");

        let old_l1 = old.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .unwrap();
        let new_l1 = new.geometry[0]
            .iter()
            .find(|g| g.get_label().as_deref() == Some("l1"))
            .unwrap();

        assert!(
            new_l1
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Edit(1))),
            "stacked: should have Edit(1), got: {:?}",
            new_l1.stack
        );
        assert!(
            new_l1
                .stack
                .iter()
                .any(|s| matches!(s.0, CompiledFunction::Anchor)),
            "stacked: should have Anchor, got: {:?}",
            new_l1.stack
        );
        let old_fns: Vec<_> = old_l1.stack.iter().map(|s| &s.0).collect();
        let new_fns: Vec<_> = new_l1.stack.iter().map(|s| &s.0).collect();
        assert_eq!(
            old_fns, new_fns,
            "stacked annotations should produce identical stack: old={:?}, new={:?}",
            old_fns, new_fns,
        );
    }

    #[test]
    fn test_conflicting_annotation_and_function_hamming_rejected() {
        // BUG 1: If a definition uses the old function syntax AND has a
        // conflicting annotation, the compiler should reject it. Otherwise
        // the annotation silently appends a second matching modifier to
        // the stack and the interpreter uses the wrong one.
        // E.g., #[hamming(3)] anchor = edit(f[CATAGC], 1) -- edit(1) from
        // function + hamming(3) from annotation = conflicting.
        let result = compile_geom(
            "#[hamming(3)] anchor = edit(f[CATAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string(),
        );
        assert!(
            result.is_err(),
            "conflicting annotation #[hamming(3)] + edit() function should be rejected"
        );
    }

    #[test]
    fn test_conflicting_annotation_and_function_search_rejected() {
        // BUG 1: #[search(relative)] on a definition that already uses
        // anchor_relative() function syntax should be rejected.
        let result = compile_geom(
            "#[search(relative)] l1 = anchor_relative(f[CAGAGC])\n1{x[2]b[8]<l1>r:}2{r:}"
                .to_string(),
        );
        assert!(
            result.is_err(),
            "conflicting #[search(relative)] + anchor_relative() should be rejected"
        );
    }

    #[test]
    fn test_malformed_hamming_annotation_non_numeric_rejected() {
        // BUG 2: #[hamming(abc)] silently compiles as if no annotation,
        // producing exact match instead of hamming. Should be a compile error.
        let result =
            compile_geom("#[hamming(abc)] anchor = f[CATAGC]\n1{x:<anchor>x:}2{r:}".to_string());
        assert!(
            result.is_err(),
            "#[hamming(abc)] with non-numeric arg should be rejected"
        );
    }

    #[test]
    fn test_malformed_hamming_annotation_no_args_rejected() {
        // BUG 2: #[hamming()] with no arguments silently compiles as exact
        // match. Should be a compile error.
        let result =
            compile_geom("#[hamming()] anchor = f[CATAGC]\n1{x:<anchor>x:}2{r:}".to_string());
        assert!(
            result.is_err(),
            "#[hamming()] with no args should be rejected"
        );
    }

    #[test]
    fn test_malformed_edit_annotation_non_numeric_rejected() {
        // BUG 2: Same as hamming but for edit.
        let result =
            compile_geom("#[edit(xyz)] anchor = f[CATAGC]\n1{x:<anchor>x:}2{r:}".to_string());
        assert!(
            result.is_err(),
            "#[edit(xyz)] with non-numeric arg should be rejected"
        );
    }

    #[test]
    fn test_malformed_search_annotation_bad_arg_rejected() {
        // BUG 2: #[search(absolute)] is not a recognized search mode.
        // Should be rejected rather than silently ignored.
        let result = compile_geom(
            "#[search(absolute)] anchor = f[CATAGC]\n1{x:<anchor>x:}2{r:}".to_string(),
        );
        assert!(
            result.is_err(),
            "#[search(absolute)] with unrecognized arg should be rejected"
        );
    }

    #[test]
    fn test_deprecation_warning_for_old_hamming_syntax() {
        // LANG-DEPRECATE: Old hamming() function syntax should emit a
        // deprecation warning suggesting the new #[hamming(N)] annotation.
        let data = compile_geom("anchor = hamming(f[CATAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string())
            .expect("old hamming syntax should still compile");
        assert!(
            data.warnings
                .iter()
                .any(|w| w.contains("deprecated") && w.contains("hamming")),
            "should emit deprecation warning for hamming(), got: {:?}",
            data.warnings
        );
    }

    #[test]
    fn test_deprecation_warning_for_old_edit_syntax() {
        let data = compile_geom("anchor = edit(f[CATAGC], 1)\n1{x:<anchor>x:}2{r:}".to_string())
            .expect("old edit syntax should still compile");
        assert!(
            data.warnings
                .iter()
                .any(|w| w.contains("deprecated") && w.contains("edit")),
            "should emit deprecation warning for edit(), got: {:?}",
            data.warnings
        );
    }

    #[test]
    fn test_deprecation_warning_for_old_anchor_relative_syntax() {
        let data =
            compile_geom("l1 = anchor_relative(f[CAGAGC])\n1{x[2]b[8]<l1>r:}2{r:}".to_string())
                .expect("old anchor_relative syntax should still compile");
        assert!(
            data.warnings
                .iter()
                .any(|w| w.contains("deprecated") && w.contains("anchor_relative")),
            "should emit deprecation warning for anchor_relative(), got: {:?}",
            data.warnings
        );
    }

    #[test]
    fn test_no_deprecation_warning_for_new_annotation_syntax() {
        // New annotation syntax should NOT emit any deprecation warnings.
        let data =
            compile_geom("#[hamming(1)] anchor = f[CATAGC]\n1{x:<anchor>x:}2{r:}".to_string())
                .expect("new annotation syntax should compile");
        assert!(
            data.warnings.is_empty(),
            "new annotation syntax should not emit warnings, got: {:?}",
            data.warnings
        );
    }

    #[test]
    fn test_no_deprecation_warning_for_new_stacked_syntax() {
        let data = compile_geom(
            "#[search(relative)] #[edit(1)] l1 = f[CAGAGC]\n1{x[2]b[8]<l1>r:}2{r:}".to_string(),
        )
        .expect("stacked annotation syntax should compile");
        assert!(
            data.warnings.is_empty(),
            "stacked annotation syntax should not emit warnings, got: {:?}",
            data.warnings
        );
    }
}
