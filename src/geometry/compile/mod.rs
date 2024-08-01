pub mod definitions;
pub mod functions;
pub mod reads;
mod transformation;
pub mod utils;

use std::{collections::HashMap, fmt::Write};

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

impl CompiledData {
    // v1: FixedSeq or RangedLen
    pub fn is_complex_geometry(&self) -> bool {
        self.geometry.iter().flatten().any(|x| x.is_complex())
    }

    // normalize variable length segments, remove anchors, update lengths
    pub fn get_simplified_description_string(self) -> String {
        if self.transformation.is_some() {
            let mut map: HashMap<String, String> = HashMap::new();

            for geom in self.geometry.iter().flatten() {
                let label = geom.get_label();

                let desc = geom.get_simplified_description_string();

                if let Some(label) = label {
                    if !desc.is_empty() {
                        map.insert(label, desc);
                    }
                }
            }

            self.transformation.unwrap().into_iter().enumerate().fold(
                String::new(),
                |mut acc, (i, labels)| {
                    let geom_desc = labels
                        .into_iter()
                        .map(|l| {
                            let key = l
                                .split('.')
                                .collect::<Vec<&str>>()
                                .get(1)
                                .unwrap()
                                .to_string();

                            map.get(&key).unwrap().clone()
                        })
                        .collect::<String>();

                    write!(&mut acc, "{}{{{}}}", i + 1, geom_desc)
                        .expect("Should have been able to format!");

                    acc
                },
            )
        } else {
            self.geometry
                .into_iter()
                .enumerate()
                .fold(String::new(), |mut acc, (i, geom)| {
                    write!(
                        &mut acc,
                        "{}{{{}}}",
                        i + 1,
                        geom.into_iter()
                            .map(|g| g.get_simplified_description_string())
                            .collect::<String>()
                    )
                    .expect("Should have been able to format!");
                    acc
                })
        }
    }
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
