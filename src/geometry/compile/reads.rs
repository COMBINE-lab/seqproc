use super::utils::*;

use std::{collections::HashMap, ops::Deref};

use crate::parser::{Expr, Function, Size, Spanned};

pub fn validate_geometry(
    map: HashMap<String, GeometryMeta>,
    geom: Vec<(Interval, i32)>,
) -> Result<(), Error> {
    let mut expect_next = vec![
        ReturnType::FixedLen,
        ReturnType::FixedSeq,
        ReturnType::Unbounded,
        ReturnType::Ranged,
    ];
    let mut iter = geom.iter();

    loop {
        let next = iter.next();

        if next.is_none() {
            break;
        }

        let gm = match next.unwrap() {
            (Interval::Named(l), _) => map.get(l).unwrap(),
            (Interval::Temporary(gp_), _) => gp_,
        };

        let (gp, span) = gm.expr.clone();

        let type_ = match gp.size {
            Size::FixedSeq(_) => ReturnType::FixedSeq,
            Size::FixedLen(_) => ReturnType::FixedLen,
            Size::RangedLen(_) => ReturnType::Ranged,
            Size::UnboundedLen => ReturnType::Unbounded,
        };

        if !expect_next.contains(&type_) {
            return Err(Error {
                span,
                msg: format!(
                    "Ambiguous Geometry: expected {:?}, found: {}",
                    expect_next, type_
                ),
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
            _ => unreachable!(),
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
    exprs: Spanned<Vec<Expr>>,
    map: &mut HashMap<String, GeometryMeta>,
) -> Result<(HashMap<String, GeometryMeta>, Geometry), Error> {
    let mut err: Option<Error> = None;
    let mut geometry: Geometry = Vec::new();
    let mut labels: Vec<String> = Vec::new();

    // create a vector of labels which have already been used. help with errors
    // labels and span!

    let (exprs, span) = exprs;

    'outer_outer: for read in exprs {
        let (read, num) = if let Expr::Read((num, _), read) = read {
            (read, num)
        } else {
            return Err(Error {
                span,
                msg: format!("Expected a Read found {}", read),
            });
        };

        let mut read_geom: Vec<(Interval, i32)> = Vec::new();
        'outer: for expr in read {
            let mut expr = expr;
            let mut spanned_geom_piece: Option<Spanned<GeometryPiece>> = None;
            let mut stack: Vec<Spanned<Function>> = Vec::new();
            let mut label: Option<String> = None;

            'inner: loop {
                match expr.0 {
                    Expr::Function(fn_, gp) => {
                        expr = gp.deref().clone();
                        stack.insert(0, fn_);
                    }
                    Expr::LabeledGeomPiece(l, gp) => {
                        if let Expr::Label((l, span)) = l.deref() {
                            if labels.contains(l) || map.clone().contains_key(l) {
                                err = Some(Error {
                                    span: span.clone(),
                                    msg: format!("Variable: {}, already defined above.", l),
                                });

                                break 'outer;
                            }

                            label = Some(l.clone());
                        }
                        // maybe return from this and add labeled elements to the map outside of this
                        // would have to unpack labeled values to validate at the end
                        expr = gp.deref().clone();

                        break 'inner;
                    }
                    Expr::Label((ref l, ref span)) => {
                        if labels.contains(l) {
                            err = Some(Error {
                                span: span.clone(),
                                msg: format!(
                                    "`{}` has already been used. Cannot use same variable more than once.",
                                    l
                                ),
                            });

                            break 'outer;
                        } else if let Some(inner_expr) = map.get(l) {
                            label = Some(l.clone());
                            labels.push(l.clone());
                            spanned_geom_piece = Some(inner_expr.expr.clone());
                            stack = inner_expr
                                .stack
                                .clone()
                                .into_iter()
                                .chain(stack)
                                .collect::<Vec<_>>();

                            break 'inner;
                        } else {
                            err = Some(Error {
                                span: span.clone(),
                                msg: format!("No variable declared with label: {}", l),
                            });

                            break 'outer;
                        }
                    }
                    _ => break 'inner,
                }
            }

            // if spanned geom piece is set then expr should not matter
            let spanned_gp = if let Some(spanned_gp) = spanned_geom_piece {
                spanned_gp
            } else if let (Expr::GeomPiece(type_, size), span) = expr {
                (GeometryPiece { type_, size }, span)
            } else {
                unreachable!()
            };

            let gm = GeometryMeta {
                expr: spanned_gp,
                stack,
            };

            if let Err(e) = validate_expr(gm.clone()) {
                err = Some(e);
                break 'outer;
            }

            if let Some(l) = label {
                map.insert(l.clone(), gm.clone());
                read_geom.push((Interval::Named(l), num))
            } else {
                read_geom.push((Interval::Temporary(gm), num));
            }
        }

        if let Err(e) = validate_geometry(map.clone(), read_geom.clone()) {
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
