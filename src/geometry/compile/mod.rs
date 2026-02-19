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

use crate::{
    parser::{Annotation, Description, TransformOutput},
    S,
};

use self::{
    reads::standardize_geometry,
    transformation::label_transformation,
    utils::{GeometryMeta, Interval, Transformation},
};

/// Per-read annotation data extracted from the parsed AST.
#[derive(Debug, Clone)]
pub struct ReadAnnotations {
    /// Read index (1-based, e.g., 1 for seq1).
    pub read_idx: usize,
    /// Annotations attached to this read.
    pub annotations: Vec<S<Annotation>>,
}

/// Compiled conditional output (match block).
#[derive(Debug)]
pub struct CompiledMatchBlock {
    /// Which read's attribute to branch on (1-based index).
    pub read_ref: usize,
    /// Attribute name to branch on (e.g., "ori").
    pub attr: String,
    /// Transformation labels for the forward arm.
    pub fw_transformation: Transformation,
    /// Transformation labels for the reverse complement arm.
    pub rc_transformation: Transformation,
}

#[derive(Debug)]
pub struct CompiledData {
    pub geometry: Vec<Vec<GeometryMeta>>,
    pub transformation: Option<Transformation>,
    /// Per-read annotations from the input geometry.
    pub read_annotations: Vec<ReadAnnotations>,
    /// Conditional output match block, if present.
    pub match_block: Option<CompiledMatchBlock>,
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

/// Converts a parsed Description into CompiledData, performing validation and optional transformation.
///
/// Calls compile_definitions, then compile_reads to build a geometry map.
/// If transforms are present, runs compile_transformation and labels it.
///
/// Returns CompiledData { geometry, transformation } or Error.
pub fn compile(
    Description {
        definitions,
        reads,
        transforms,
    }: Description,
) -> Result<CompiledData, Error> {
    // Extract per-read annotations before compiling reads.
    let read_annotations: Vec<ReadAnnotations> = reads
        .0
        .iter()
        .map(|S(r, _)| ReadAnnotations {
            read_idx: r.index.0,
            annotations: r.annotations.clone(),
        })
        .filter(|ra| !ra.annotations.is_empty())
        .collect();

    // Validate read indices fit in u8, required by TryOrientationOp.
    for ra in &read_annotations {
        if u8::try_from(ra.read_idx).is_err() {
            let span = reads
                .0
                .iter()
                .find(|S(r, _)| r.index.0 == ra.read_idx)
                .map(|S(_, s)| *s)
                .unwrap_or_default();
            return Err(Error {
                span,
                msg: format!(
                    "read index {} exceeds maximum (255); \
                     annotated reads must have indices that fit in a u8",
                    ra.read_idx
                ),
            });
        }
    }

    // validate definition block
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

    match transforms {
        Some(S(TransformOutput::Direct(transform_reads), span)) => {
            let (transformation, map) =
                compile_transformation(S(transform_reads, span), map, &numbered_labels)?;

            let transformation = label_transformation(transformation, &numbered_labels);

            let geometry = standardize_geometry(map, geometry);

            Ok(CompiledData {
                geometry,
                transformation: Some(transformation),
                read_annotations,
                match_block: None,
            })
        }
        Some(S(
            TransformOutput::Match {
                read_ref,
                attr,
                fw_arm,
                rc_arm,
            },
            span,
        )) => {
            // Compile both arms as separate transformations.
            let (fw_transformation, fw_map) =
                compile_transformation(S(fw_arm, span), map.clone(), &numbered_labels)?;
            let (rc_transformation, _rc_map) =
                compile_transformation(S(rc_arm, span), map, &numbered_labels)?;

            let fw_transformation = label_transformation(fw_transformation, &numbered_labels);
            let rc_transformation = label_transformation(rc_transformation, &numbered_labels);

            // Validate: fw and rc arms must produce identical transformations.
            // The forward-orientation invariant (TryOrientationOp RCs input
            // before geometry matching) means all extracted intervals are in
            // forward orientation regardless of original read orientation.
            // Therefore the interpreter always applies the fw arm. Divergent
            // arms would cause the rc arm to be silently ignored, which is a
            // data-correctness bug.
            if fw_transformation != rc_transformation {
                return Err(Error {
                    span,
                    msg: "match block fw and rc arms must produce identical transformations; \
                          the forward-orientation invariant means extracted data is always \
                          in forward orientation, so different arms would be silently ignored"
                        .to_string(),
                });
            }

            let geometry = standardize_geometry(fw_map, geometry);

            Ok(CompiledData {
                geometry,
                transformation: Some(fw_transformation.clone()),
                read_annotations,
                match_block: Some(CompiledMatchBlock {
                    read_ref: read_ref.0,
                    attr: attr.0,
                    fw_transformation,
                    rc_transformation,
                }),
            })
        }
        None => {
            let geometry = standardize_geometry(map, geometry);

            Ok(CompiledData {
                geometry,
                transformation: None,
                read_annotations,
                match_block: None,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Annotation, Description, Expr, IntervalKind, IntervalShape, Read};

    fn make_annotated_read(read_idx: usize) -> S<Read> {
        let span = (0..1).into();
        let annotation = S(
            Annotation {
                name: S("match_ori".to_string(), span),
                args: vec![S("either".to_string(), span)],
            },
            span,
        );
        S(
            Read {
                annotations: vec![annotation],
                index: S(read_idx, span),
                exprs: vec![S(
                    Expr::GeomPiece(IntervalKind::Barcode, IntervalShape::FixedLen(S(10, span))),
                    span,
                )],
            },
            span,
        )
    }

    #[test]
    fn bug3_annotated_read_index_overflow_rejected() {
        // BUG 3: read_idx was cast to u8 with `as u8`, silently truncating
        // values > 255. The compiler should reject annotated reads whose
        // index exceeds u8::MAX.
        let span = (0..1).into();
        let desc = Description {
            definitions: S(vec![], span),
            reads: S(vec![make_annotated_read(256)], span),
            transforms: None,
        };
        let result = compile(desc);
        assert!(
            result.is_err(),
            "Annotated read with index 256 should be rejected (u8 overflow)"
        );
        let err_msg = result.unwrap_err().msg;
        assert!(
            err_msg.contains("256") && err_msg.contains("255"),
            "Error should mention the offending index and the limit: {}",
            err_msg
        );
    }

    #[test]
    fn bug3_annotated_read_index_255_accepted() {
        // Read index 255 is the max u8 value and should not be rejected
        // by the u8 overflow check (it may fail for other reasons like
        // missing geometry, but not due to index overflow).
        let span = (0..1).into();
        let desc = Description {
            definitions: S(vec![], span),
            reads: S(vec![make_annotated_read(255)], span),
            transforms: None,
        };
        let result = compile(desc);
        // Should not fail with u8 overflow error (may fail for other reasons)
        if let Err(e) = &result {
            assert!(
                !e.msg.contains("exceeds maximum"),
                "Read index 255 should not trigger u8 overflow: {}",
                e.msg
            );
        }
    }
}
