use std::collections::HashMap;

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::{Definition, Expr},
    S,
};

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
        unreachable!()
    };

    let gp = GeometryMeta { expr: gp, stack }; // Now we have the geometry piece and the stack of functions

    gp.validate_expr().map(|()| gp) // Validate the function composition against the geometry piece
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
}
