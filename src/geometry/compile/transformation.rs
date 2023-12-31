use std::{collections::HashMap, ops::Deref};

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::{Expr, Function},
    S,
};

pub fn compile_transformation(
    transformation: S<Expr>,
    map: &mut HashMap<String, GeometryMeta>,
) -> Result<(Transformation, &mut HashMap<String, GeometryMeta>), Error> {
    let S(expr, span) = transformation;

    let exprs = if let Expr::Transform(exprs) = expr {
        exprs
    } else {
        return Err(Error {
            span,
            msg: format!("Expected a transformation expression found: {expr}"),
        });
    };

    /*
       Although this is similar to the compile function for reads it is slightly different
       This should only allow labels and functions

       Thus the method should have different validation and should be unique
       Also there is no need to confirm geometry

       Lets see if we can make this a bit cleaner
    */
    compile(S(exprs, span), map)
}

/*
   Takes the map, all labels should be in the map
   Validate any further compositions
   Update the map with the new geometry pieces
   Return the new map and a list of labels which represents the final transformation
*/
fn compile(
    exprs: S<Vec<Expr>>,
    map: &mut HashMap<String, GeometryMeta>,
) -> Result<(Transformation, &mut HashMap<String, GeometryMeta>), Error> {
    let mut transformation: Transformation = Vec::new();

    let S(exprs, span) = exprs;

    for read in exprs {
        let read = match read {
            Expr::Read(S(_num, _), read) => read,
            _ => {
                return Err(Error {
                    span,
                    msg: format!("Expected a Read found {}", read),
                });
            }
        };

        let mut inner_transformation: Vec<String> = Vec::new();

        for expr in read {
            let mut expr = expr;
            let mut stack: Vec<S<Function>> = Vec::new();
            let mut compiled_stack: Vec<S<CompiledFunction>> = Vec::new();
            let label: Option<S<String>>;

            'inner: loop {
                match expr.0 {
                    Expr::Function(fn_, gp) => {
                        expr = gp.deref().clone();
                        stack.push(fn_);
                    }
                    Expr::Label(ref l) => {
                        label = Some(l.clone());
                        break 'inner;
                    }
                    _ => unimplemented!(),
                }
            }

            let S(label, label_span) = if let Some(l) = label {
                l
            } else {
                return Err(Error {
                    span,
                    msg: "Transformations must only reference previously defined labels"
                        .to_string(),
                });
            };

            let gp = if let Some(gp) = map.get(&label) {
                for fn_ in stack {
                    compiled_stack.push(compile_fn(fn_, expr.clone())?)
                }

                GeometryMeta {
                    expr: gp.expr.clone(),
                    stack: compiled_stack
                        .clone()
                        .into_iter()
                        .chain(gp.stack.clone())
                        .collect::<Vec<_>>(),
                }
            } else {
                return Err(Error {
                    span: label_span,
                    msg: format!("Variable with name \"{}\" found", label),
                });
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

            inner_transformation.push(format!("seq{num}.{}", l));
        }

        numbered_transformation.push(inner_transformation);
    }

    numbered_transformation
}
