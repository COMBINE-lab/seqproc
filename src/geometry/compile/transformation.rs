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
) -> Result<(Transformation, HashMap<String, GeometryMeta>), Error> {
    let mut transformation: Transformation = Vec::new();

    for S(Read { exprs, .. }, _) in reads {
        let mut inner_transformation: Vec<String> = Vec::new();

        for expr in exprs {
            let mut expr = expr;
            let mut stack: Vec<S<Function>> = Vec::new();
            let mut compiled_stack: Vec<S<CompiledFunction>> = Vec::new();
            let label: Option<S<String>>;

            'inner: loop {
                match expr.0 {
                    Expr::Function(fn_, gp) => {
                        expr = gp.unboxed();
                        stack.push(fn_);
                    }
                    Expr::Label(ref l) => {
                        label = Some(l.clone());
                        break 'inner;
                    }
                    _ => unimplemented!(),
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

            for fn_ in stack {
                compiled_stack.push(compile_fn(fn_, expr.clone())?);
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

            map.insert(label.clone(), gp);

            inner_transformation.push(label);
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
