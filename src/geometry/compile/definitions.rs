use std::{collections::HashMap, ops::Deref};

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::Expr,
    S,
};

/// validate definitions there should be no labels, just labeled geom peices and functions
fn validate_definition(mut expr: S<Expr>, label: &str) -> Result<GeometryMeta, Error> {
    let mut stack: Vec<S<CompiledFunction>> = vec![];

    loop {
        match expr.0 {
            Expr::Function(fn_, gp) => {
                // parse the function and validate it
                expr = *gp;
                stack.push(compile_fn(fn_, expr.clone())?); // here is where we can compile the functions
            }
            Expr::Label(_) => {
                return Err(Error {
                    span: expr.1,
                    msg: "Unexpected label in definition block".to_string(),
                })
            }
            _ => break,
        }
    }

    let gp = if let S(Expr::GeomPiece(type_, size), span) = expr {
        S(
            GeometryPiece {
                type_,
                size,
                label: Some(label.to_owned()),
            },
            span,
        )
    } else {
        unreachable!()
    };

    let gp = GeometryMeta { expr: gp, stack };

    gp.validate_expr().map(|()| gp)
}

pub fn compile_definitions(
    S(expr, expr_span): S<Expr>,
) -> Result<HashMap<String, GeometryMeta>, Error> {
    if let Expr::Definitions(defs) = expr {
        let mut map = HashMap::new();

        let mut err: Option<Error> = None;

        for S(def, def_span) in defs {
            if let Expr::LabeledGeomPiece(label, expr) = def.clone() {
                let expr = expr.deref().clone();
                let label = label.deref().clone();

                if let Expr::Label(S(l, span)) = label {
                    let res = validate_definition(expr, &l);
                    if let Err(e) = res {
                        err = Some(e);
                        break;
                    } else if map.insert(l.clone(), res.ok().unwrap()).is_some() {
                        err = Some(Error {
                            // span labels
                            span,
                            msg: format!(
                                "Repeated label in definition block: \"{l}\" already defined"
                            ),
                        });
                        break;
                    }
                } else {
                    err = Some(Error {
                        span: def_span,
                        msg: format!("Expected a Labeled Geometry piece, found: {def}"),
                    })
                }
            }
        }

        if let Some(e) = err {
            return Err(e);
        }

        Ok(map)
    } else {
        Err(Error {
            span: expr_span,
            msg: format!("Expected a definition block, found: {expr}"),
        })
    }
}
