pub mod definitions;
pub mod functions;
pub mod reads;
mod transformation;
pub mod utils;

use definitions::compile_definitions;
use reads::compile_reads;
use transformation::compile_transformation;
use utils::Error;

use crate::{parser::Description, S};

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
pub fn compile(
    Description {
        definitions,
        reads,
        transforms,
    }: Description,
) -> Result<CompiledData, Error> {
    // validate defintion block
    let map = {
        let def_res = compile_definitions(definitions);

        if let Err(e) = def_res {
            return Err(e);
        } else {
            def_res.ok().unwrap()
        }
    };
    let validate_read_res = compile_reads(reads, map);

    let Ok((map, geometry)) = validate_read_res else {
        return Err(validate_read_res.err().unwrap());
    };

    let numbered_labels = geometry
        .iter()
        .flatten()
        .filter(|e| matches!(e.0, Interval::Named(_)))
        .cloned()
        .collect::<Vec<_>>();

    // this needs a bit more thought

    /*
       At this point we have
           - map
           - geometry
           - transformation

       What needs to happen
           - labels in geometry replaced by their value in map
           - labels in transformation add their read number from geometry DONE
    */

    if let Some(S(transform, span)) = transforms {
        let (transformation, map) = compile_transformation(S(transform, span), map)?;

        let transformation = label_transformation(transformation, &numbered_labels);

        let geometry = standardize_geometry(map, geometry);

        Ok(CompiledData {
            geometry,
            transformation: Some(transformation),
        })
    } else {
        let geometry = standardize_geometry(map, geometry);

        Ok(CompiledData {
            geometry,
            transformation: None,
        })
    }
}
