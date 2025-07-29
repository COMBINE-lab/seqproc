use std::collections::HashMap;

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::{Expr, Function, Read},
    S,
};

/// Takes the map, all labels should be in the map
/// Validate any further compositions
/// Update the map with the new geometry pieces
/// Return the new map and a list of labels which represents the final transformation
pub fn compile_transformation(
    S(reads, span): S<Vec<S<Read>>>,
    mut map: HashMap<String, GeometryMeta>,
    read_intervals: &[(Interval, usize)],
) -> Result<(Transformation, HashMap<String, GeometryMeta>), Error> {
    let mut transformation: Transformation = Vec::new();
    let read_labels = read_intervals
        .iter()
        .filter_map(|(i, _)| match i {
            Interval::Named(n) => Some(n),
            Interval::Temporary(_) => None,
        })
        .collect::<Vec<_>>();

    for S(Read { exprs, .. }, _) in reads {
        let mut inner_transformation: Vec<String> = Vec::new();

        for expr in exprs {
            let mut expr = expr;
            let mut stack: Vec<S<Function>> = Vec::new();
            let mut compiled_stack: Vec<S<CompiledFunction>> = Vec::new();
            let label: Option<S<String>>;

            'inner: loop {
                let generic_transformation_msg =
                    "Only labels and transformed labels can be referenced in transformations";
                match expr.0 {
                    Expr::Function(fn_, gp) => {
                        expr = gp.unboxed();
                        stack.push(fn_);
                    }
                    Expr::Label(ref l) => {
                        label = Some(l.clone());
                        break 'inner;
                    }
                    Expr::LabeledGeomPiece(_, _) | Expr::GeomPiece(_, _) => return Err(Error {
                        span: expr.1,
                        msg: format!("{generic_transformation_msg} - Cannot construct intervals in a transformation")
                    }),
                    Expr::Self_ => return Err(Error {
                        span: expr.1,
                        msg: format!("{generic_transformation_msg} - Misplaced reference of 'self', this is a reserved token for the 'map' function."),
                    })
                }
            }

            let Some(S(label, label_span)) = label else {
                return Err(Error {
                    span,
                    msg: "Transformations must only reference previously defined labels"
                        .to_string(),
                });
            };

            let Some(gp) = map.get(&label) else {
                return Err(Error {
                    span: label_span,
                    msg: format!("Variable with name \"{label}\" not found"),
                });
            };

            if !read_labels.contains(&&label) {
                return Err(Error {
                    span: label_span,
                    msg: format!("Cannot transform a non-matched label: variable with name \"{label}\" was defined but never matched in read.")
                });
            }

            for fn_ in stack {
                compiled_stack.push(compile_fn(fn_, expr.clone())?);
            }

            for fn_ in &gp.stack {
                if let S(CompiledFunction::Remove, span) = fn_ {
                    return Err(Error {
                        span: *span,
                        msg: "Cannot reference a void interval after '->' - if you want to keep this interval then remove the 'remove' transformation.".to_string()
                    });
                }
            }

            // if label is removed just remove the label from the transformation
            if let Some(S(fn_, _)) = compiled_stack.first() {
                if &CompiledFunction::Remove != fn_ {
                    inner_transformation.push(label.clone());
                };
            } else {
                inner_transformation.push(label.clone());
            }

            let gp = GeometryMeta {
                expr: gp.expr.clone(),
                stack: compiled_stack
                    .clone()
                    .into_iter()
                    .chain(gp.stack.clone())
                    .collect::<Vec<_>>(),
            };

            gp.validate_expr()?;

            map.insert(label, gp);
        }

        transformation.push(inner_transformation);
    }

    Ok((transformation, map))
}

fn find_num(l: &str, list: &[(Interval, usize)]) -> String {
    for (interval, n) in list {
        if let Interval::Named(name) = interval {
            if name == l {
                return n.to_string();
            }
        }
    }

    unreachable!()
}

pub fn label_transformation(
    transformation: Transformation,
    numbered_labels: &[(Interval, usize)],
) -> Transformation {
    let mut numbered_transformation: Transformation = Vec::new();

    for t in transformation {
        let mut inner_transformation: Vec<String> = Vec::new();

        for l in t {
            let num = find_num(&l, numbered_labels);

            inner_transformation.push(format!("seq{num}.{l}"));
        }

        numbered_transformation.push(inner_transformation);
    }

    numbered_transformation
}
