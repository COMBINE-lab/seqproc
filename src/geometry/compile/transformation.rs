use std::{collections::HashMap, ops::Deref};

use super::utils::*;

use crate::parser::{Expr, Function, Spanned};

pub fn compile_transformation(
    transformation: Spanned<Expr>,
    map: &mut HashMap<String, GeometryPiece>,
) -> Result<(Transformation, &mut HashMap<String, GeometryPiece>), Error> {
    let (expr, span) = transformation;

    let exprs = if let Expr::Transform(exprs) = expr {
        exprs
    } else {
        return Err(Error {
            span: span.clone(),
            msg: format!("Expected a transformation expression found: {}", expr),
        });
    };

    /*
       Although this is similar to the compile function for reads it is slightly different
       This should only allow labels and functions

       Thus the method should have different validation and should be unique
       Also there is no need to confirm geometry

       Lets see if we can make this a bit cleaner
    */
    compile((exprs, span), map)
}

/*
   Takes the map, all labels should be in the map
   Validate any further compositions
   Update the map with the new geometry pieces
   Return the new map and a list of labels which represents the final transformation
*/
fn compile(
    exprs: Spanned<Vec<Expr>>,
    map: &mut HashMap<String, GeometryPiece>,
) -> Result<(Transformation, &mut HashMap<String, GeometryPiece>), Error> {
    let mut transformation: Transformation = Vec::new();

    let (exprs, span) = exprs;

    for read in exprs {
        let read = match read {
            Expr::Read((_num, _), read) => read,
            _ => {
                return Err(Error {
                    span: span.clone(),
                    msg: format!("Expected a Read found {}", read),
                });
            }
        };

        let mut inner_transformation: Vec<String> = Vec::new();

        for expr in read {
            let mut expr = expr;
            let mut stack: Vec<Spanned<Function>> = Vec::new();
            let label: Option<Spanned<String>>;

            'inner: loop {
                match expr.0 {
                    Expr::Function(fn_, gp) => {
                        expr = gp.deref().clone();
                        stack.insert(0, fn_);
                    }
                    Expr::Label(ref l) => {
                        label = Some(l.clone());
                        break 'inner;
                    }
                    _ => unimplemented!(),
                }
            }

            let (label, label_span) = if let Some(l) = label {
                l
            } else {
                return Err(Error {
                    span: span.clone(),
                    msg: "Transformations must only reference previously defined labels"
                        .to_string(),
                });
            };

            let gp = if let Some(gp) = map.get(&label) {
                GeometryPiece {
                    expr: gp.expr.clone(),
                    stack: gp
                        .stack
                        .clone()
                        .into_iter()
                        .chain(stack)
                        .collect::<Vec<_>>(),
                }
            } else {
                return Err(Error {
                    span: label_span,
                    msg: format!("Variable with name \"{}\" found", label),
                });
            };

            validate_expr(gp.clone())?;

            map.insert(label.clone(), gp);

            inner_transformation.push(label);
        }

        transformation.push(inner_transformation);
    }

    Ok((transformation, map))
}

fn find_num(l: String, list: Vec<(Interval, i32)>) -> String {
    for (interval, n) in &list {
        if interval.clone() == Interval::Named(l.clone()) {
            return n.to_string();
        }
    }

    unreachable!()
}

pub fn label_transformation(
    transformation: Transformation,
    numbered_labels: Vec<(Interval, i32)>,
) -> Transformation {
    let mut numbered_transformation: Transformation = Vec::new();

    for t in transformation {
        let mut inner_transformation: Vec<String> = Vec::new();

        for l in t {
            let num = find_num(l.clone(), numbered_labels.clone());

            inner_transformation.push(format!("seq{num}.{}", l));
        }

        numbered_transformation.push(inner_transformation);
    }

    numbered_transformation
}
