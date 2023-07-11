pub mod definitions;
pub mod reads;
mod transformation;
mod utils;

use definitions::compile_definitions;
use reads::compile_reads;
use transformation::compile_transformation;
use utils::Error;

use std::{collections::HashMap, ops::Deref};

use crate::parser::Expr;

use self::{
    reads::standardize_geometry,
    transformation::label_transformation,
    utils::{GeometryMeta, Interval, Transformation},
};

#[derive(Debug)]
pub struct CompiledData {
    pub geometry: Vec<Vec<GeometryMeta>>,
    pub transformation: Option<Transformation>,
}

// this should be more of a compile and also should return a kind of
pub fn compile(expr: Expr) -> Result<CompiledData, Error> {
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

        let validate_read_res = compile_reads(r, &mut map);

        let (mut map, geometry) = if let Ok(cd) = validate_read_res {
            cd
        } else {
            return Err(validate_read_res.err().unwrap());
        };

        // this needs a bit more thought
        let compiled_transformation = if let Some(transform) = t.deref() {
            let res = compile_transformation(transform.clone(), &mut map);

            if let Err(e) = res {
                return Err(e);
            } else {
                res.ok()
            }
        } else {
            None
        };

        /*
           At this point we have
               - map
               - geometry
               - transformation

           What needs to happen
               - labels in geometry replaced by their value in map
               - labels in transformation add their read number from geometry DONE
        */

        let numbered_labels = geometry
            .iter()
            .flatten()
            .filter(|e| matches!(e.0, Interval::Named(_)))
            .map(|e| e.clone())
            .collect::<Vec<_>>();

        if let Some((transformation, map)) = compiled_transformation {
            let transformation = label_transformation(transformation, numbered_labels);

            let geometry = standardize_geometry(&mut map.clone(), geometry);

            Ok(CompiledData {
                geometry,
                transformation: Some(transformation),
            })
        } else {
            let geometry = standardize_geometry(&mut map, geometry);

            Ok(CompiledData {
                geometry,
                transformation: None,
            })
        }
    } else {
        unreachable!()
    }
}
