use std::collections::HashMap;

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::{Definition, Expr},
    S,
};

/// validate definitions there should be no labels, just labeled geom peices and functions
fn validate_definition(mut expr: S<Expr>, label: &str) -> Result<GeometryMeta, Error> {
    let mut stack: Vec<S<CompiledFunction>> = vec![];

    loop {
        match expr.0 {
            Expr::Function(fn_, gp) => {
                // parse the function and validate it
                expr = gp.unboxed();
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
    S(defs, _): S<Vec<S<Definition>>>,
) -> Result<HashMap<String, GeometryMeta>, Error> {
    let mut map = HashMap::new();

    let mut err: Option<Error> = None;

    for S(
        Definition {
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
        } else if map.insert(label_str.clone(), res.ok().unwrap()).is_some() {
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

    Ok(map)
}
