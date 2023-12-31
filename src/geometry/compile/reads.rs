use std::collections::HashMap;

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::{Expr, Function, IntervalShape},
    S,
};

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

    for (interval, _) in geom {
        let gm = match interval {
            Interval::Named(l) => map.get(l).unwrap(),
            Interval::Temporary(gp_) => gp_,
        };

        let S(gp, span) = gm.expr.clone();

        let type_ = match gp.size {
            IntervalShape::FixedSeq(_) => ReturnType::FixedSeq,
            IntervalShape::FixedLen(_) => ReturnType::FixedLen,
            IntervalShape::RangedLen(_) => ReturnType::Ranged,
            IntervalShape::UnboundedLen => ReturnType::Unbounded,
        };

        if !expect_next.contains(&type_) {
            return Err(Error {
                span,
                msg: format!("Ambiguous Geometry: expected {expect_next:?}, found: {type_}"),
            });
        }

        expect_next = match type_ {
            ReturnType::FixedLen | ReturnType::FixedSeq => {
                vec![
                    ReturnType::FixedLen,
                    ReturnType::FixedSeq,
                    ReturnType::Unbounded,
                    ReturnType::Ranged,
                ]
            }
            ReturnType::Ranged | ReturnType::Unbounded => {
                vec![ReturnType::FixedSeq]
            }
            ReturnType::Void => unreachable!(),
        };
    }

    Ok(())
}

pub fn standardize_geometry(
    map: &mut HashMap<String, GeometryMeta>,
    geometry: Geometry,
) -> Vec<Vec<GeometryMeta>> {
    let mut std_geom: Vec<Vec<GeometryMeta>> = Vec::new();

    for read in geometry {
        let mut geom: Vec<GeometryMeta> = Vec::new();
        for interval in read {
            match interval {
                (Interval::Named(l), _) => geom.push(map.get(&l).unwrap().clone()),
                (Interval::Temporary(gp), _) => geom.push(gp.clone()),
            }
        }

        std_geom.push(geom);
    }

    std_geom
}

// this should take both reads and parse them. Allowing for combined label_map
pub fn compile_reads(
    exprs: S<Vec<Expr>>,
    map: &mut HashMap<String, GeometryMeta>,
) -> Result<(HashMap<String, GeometryMeta>, Geometry), Error> {
    let mut err: Option<Error> = None;
    let mut geometry: Geometry = Vec::new();
    let mut labels: Vec<String> = Vec::new();

    // create a vector of labels which have already been used. help with errors
    // labels and span!

    let S(exprs, span) = exprs;

    'outer_outer: for read in exprs {
        let Expr::Read(S(num, _), read) = read else {
            return Err(Error {
                span,
                msg: format!("Expected a Read found {read}"),
            });
        };

        let mut read_geom: Vec<(Interval, usize)> = Vec::new();
        'outer: for expr in read {
            let mut expr = expr;
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
                    Expr::LabeledGeomPiece(l, gp) => {
                        if let Expr::Label(S(l, span)) = &*l {
                            if labels.contains(l) || map.clone().contains_key(l) {
                                err = Some(Error {
                                    span: span.clone(),
                                    msg: format!("Variable: {l}, already defined above."),
                                });

                                break 'outer;
                            }

                            label = Some(l.clone());
                        }
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
                                span: span.clone(),
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
                                        inner_expr.expr.1.clone(),
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
                                span: span.clone(),
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
                unreachable!()
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
                map.insert(l.clone(), gm.clone());
                read_geom.push((Interval::Named(l), num));
            } else {
                read_geom.push((Interval::Temporary(gm), num));
            }
        }

        if let Err(e) = validate_geometry(map, &read_geom) {
            err = Some(e);
            break 'outer_outer;
        }

        geometry.push(read_geom);
    }

    if let Some(e) = err {
        return Err(e);
    }

    Ok((map.clone(), geometry))
}
