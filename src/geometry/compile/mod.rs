pub mod definitions;
pub mod reads;
mod transformation;
mod utils;

use definitions::compile_definitions;
use reads::compile_reads;
use transformation::compile_transformation;
use utils::{Error, Geometry};

use std::{collections::HashMap, ops::Deref};

use crate::parser::Expr;

// this should be more of a compile and also should return a kind of
pub fn compile(expr: Expr) -> Result<Geometry, Error> {
    if let Expr::Description(d, r, t) = expr {
        // validate defintion block
        let mut map = if let Some(expr) = d.deref() {
            let def_res = compile_definitions(expr.clone());

            if let Err(e) = def_res {
                return Err(e);
            } else {
                def_res.ok().unwrap()
            }
        } else {
            HashMap::new()
        };

        let validate_read_res = compile_reads(r, map.clone());

        let (geometry, read_map) = if let Ok((g, m)) = validate_read_res {
            (g, m)
        } else {
            return Err(validate_read_res.err().unwrap());
        };

        map = map.into_iter().chain(read_map).collect();

        // this needs a bit more thought
        let _transformation = if let Some(transform) = t.deref() {
            let res = compile_transformation(transform.clone(), map);

            if let Err(e) = res {
                return Err(e);
            } else {
                res.ok().unwrap()
            }
        };

        Ok(geometry)
    } else {
        unreachable!()
    }
}
