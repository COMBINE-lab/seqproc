use std::collections::HashMap;

use crate::{
    compile::{
        functions::{compile_fn, CompiledFunction},
        utils::*,
    },
    parser::{
        Expr, Function, IntervalKind, IntervalShape, OutputHeader, OutputHeaderMode,
        OutputHeaderPart, Read,
    },
    Nucleotide, S,
};

fn compile_output_header(
    output_header: Option<S<OutputHeader>>,
    map: &HashMap<String, GeometryMeta>,
    read_labels: &[&String],
    efgdl_version: usize,
) -> Result<Option<HeaderTransformation>, Error> {
    let Some(S(output_header, header_span)) = output_header else {
        return Ok(None);
    };
    if efgdl_version < 2 {
        return Err(Error {
            span: header_span,
            msg: "output FASTQ-header modification requires `header { efgdl = 2 }`".to_string(),
        });
    }

    let mode = match output_header.mode.0 {
        OutputHeaderMode::Append => CompiledHeaderMode::Append,
        OutputHeaderMode::Prepend => CompiledHeaderMode::Prepend,
        OutputHeaderMode::Replace => CompiledHeaderMode::Replace,
    };
    let mut parts = Vec::with_capacity(output_header.parts.len());
    for S(part, part_span) in output_header.parts {
        match part {
            OutputHeaderPart::Literal(literal) => {
                if literal.bytes().any(|byte| matches!(byte, b'\n' | b'\r')) {
                    return Err(Error {
                        span: part_span,
                        msg: "FASTQ-header literals cannot contain newlines".to_string(),
                    });
                }
                parts.push(HeaderSegment::Literal(literal.into_bytes()));
            }
            OutputHeaderPart::Label(S(label, label_span)) => {
                if !map.contains_key(&label) {
                    return Err(Error {
                        span: label_span,
                        msg: format!("Variable with name \"{label}\" not found"),
                    });
                }
                if !read_labels.contains(&&label) {
                    return Err(Error {
                        span: label_span,
                        msg: format!(
                            "Cannot place non-matched label \"{label}\" in an output FASTQ header"
                        ),
                    });
                }
                parts.push(HeaderSegment::Label(label));
            }
        }
    }

    Ok(Some(HeaderTransformation { mode, parts }))
}

/// Takes the map, all labels should be in the map
/// Validate any further compositions
/// Update the map with the new geometry pieces
/// Return the new map and a list of labels which represents the final transformation
pub fn compile_transformation(
    S(reads, span): S<Vec<S<Read>>>,
    mut map: HashMap<String, GeometryMeta>,
    read_intervals: &[(Interval, usize)],
    efgdl_version: usize,
) -> Result<(Transformation, HashMap<String, GeometryMeta>), Error> {
    let mut transformation: Transformation = Vec::new();
    let read_labels = read_intervals
        .iter()
        .filter_map(|(i, _)| match i {
            Interval::Named(n) => Some(n),
            Interval::Temporary(_) => None,
        })
        .collect::<Vec<_>>();

    for S(
        Read {
            exprs,
            output_header,
            ..
        },
        _,
    ) in reads
    {
        let mut inner_transformation: Vec<TransformSegment> = Vec::new();

        for expr in exprs {
            let mut expr = expr;

            if let Expr::GeomPiece(
                IntervalKind::FixedSeq,
                IntervalShape::FixedSeq(S(sequence, _)),
            ) = &expr.0
            {
                if efgdl_version < 2 {
                    return Err(Error {
                        span: expr.1,
                        msg: "constructing fixed sequences in output reads requires `header { efgdl = 2 }`"
                            .to_string(),
                    });
                }
                inner_transformation.push(TransformSegment::Literal(
                    Nucleotide::as_str(sequence).as_bytes().to_vec(),
                ));
                continue;
            }

            let mut stack: Vec<S<Function>> = Vec::new();
            let mut compiled_stack: Vec<S<CompiledFunction>> = Vec::new();
            let label: Option<S<String>>;

            'inner: loop {
                let generic_transformation_msg =
                    "Only labels and transformed labels can be referenced in transformations";
                match expr.0 {
                    Expr::Function(fn_, gp) => {
                        expr = gp.unboxed();
                        stack.push(fn_);
                    }
                    Expr::Label(ref l) => {
                        label = Some(l.clone());
                        break 'inner;
                    }
                    Expr::LabeledGeomPiece(_, _) | Expr::GeomPiece(_, _) => return Err(Error {
                        span: expr.1,
                        msg: format!("{generic_transformation_msg} - Cannot construct intervals in a transformation")
                    }),
                    Expr::Self_ => return Err(Error {
                        span: expr.1,
                        msg: format!("{generic_transformation_msg} - Misplaced reference of 'self', this is a reserved token for the 'map' function."),
                    })
                }
            }

            let Some(S(label, label_span)) = label else {
                return Err(Error {
                    span,
                    msg: "Transformations must only reference previously defined labels"
                        .to_string(),
                });
            };

            let Some(gp) = map.get(&label) else {
                return Err(Error {
                    span: label_span,
                    msg: format!("Variable with name \"{label}\" not found"),
                });
            };

            if !read_labels.contains(&&label) {
                return Err(Error {
                    span: label_span,
                    msg: format!("Cannot transform a non-matched label: variable with name \"{label}\" was defined but never matched in read.")
                });
            }

            for fn_ in stack {
                compiled_stack.push(compile_fn(fn_, expr.clone())?);
            }

            for fn_ in &gp.stack {
                if let S(CompiledFunction::Remove, span) = fn_ {
                    return Err(Error {
                        span: *span,
                        msg: "Cannot reference a void interval after '->' - if you want to keep this interval then remove the 'remove' transformation.".to_string()
                    });
                }
            }

            // if label is removed just remove the label from the transformation
            if let Some(S(fn_, _)) = compiled_stack.first() {
                if &CompiledFunction::Remove != fn_ {
                    inner_transformation.push(TransformSegment::Label(label.clone()));
                };
            } else {
                inner_transformation.push(TransformSegment::Label(label.clone()));
            }

            let gp = GeometryMeta {
                expr: gp.expr.clone(),
                stack: compiled_stack
                    .clone()
                    .into_iter()
                    .chain(gp.stack.clone())
                    .collect::<Vec<_>>(),
            };

            gp.validate_expr()?;

            map.insert(label, gp);
        }

        let header = compile_output_header(output_header, &map, &read_labels, efgdl_version)?;
        transformation.push(ReadTransformation {
            sequence: inner_transformation,
            header,
        });
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
        let mut inner_transformation: Vec<TransformSegment> = Vec::new();

        for segment in t.sequence {
            match segment {
                TransformSegment::Label(label) => {
                    let num = find_num(&label, numbered_labels);
                    inner_transformation.push(TransformSegment::Label(format!("seq{num}.{label}")));
                }
                TransformSegment::Literal(bytes) => {
                    inner_transformation.push(TransformSegment::Literal(bytes));
                }
            }
        }

        let header = t.header.map(|header| HeaderTransformation {
            mode: header.mode,
            parts: header
                .parts
                .into_iter()
                .map(|part| match part {
                    HeaderSegment::Label(label) => {
                        let num = find_num(&label, numbered_labels);
                        HeaderSegment::Label(format!("seq{num}.{label}"))
                    }
                    HeaderSegment::Literal(bytes) => HeaderSegment::Literal(bytes),
                })
                .collect(),
        });

        numbered_transformation.push(ReadTransformation {
            sequence: inner_transformation,
            header,
        });
    }

    numbered_transformation
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::compile_geom;

    #[test]
    fn test_compile_transformation_basic() {
        let data = compile_geom(
            "1{b<bc>[16]u<umi>[10]r<read>:}2{r<read2>:}->1{<bc><umi>}2{<read2>}".to_string(),
        )
        .unwrap();
        assert!(data.transformation.is_some());
        let tr = data.transformation.unwrap();
        assert_eq!(tr.len(), 2);
    }

    #[test]
    fn test_compile_transformation_with_function() {
        let data = compile_geom(
            "1{b<bc>[16]u<umi>[10]r<read>:}2{r<read2>:}->1{rev(<bc>)<umi>}2{<read2>}".to_string(),
        )
        .unwrap();
        assert!(data.transformation.is_some());
    }

    #[test]
    fn test_label_transformation() {
        let labels = vec![
            (Interval::Named("bc".to_string()), 1),
            (Interval::Named("umi".to_string()), 1),
        ];
        let tr = vec![ReadTransformation {
            sequence: vec![
                TransformSegment::Label("bc".to_string()),
                TransformSegment::Label("umi".to_string()),
            ],
            header: None,
        }];
        let result = label_transformation(tr, &labels);
        assert_eq!(
            result[0].sequence[0],
            TransformSegment::Label("seq1.bc".to_string())
        );
        assert_eq!(
            result[0].sequence[1],
            TransformSegment::Label("seq1.umi".to_string())
        );
    }

    #[test]
    fn test_find_num() {
        let labels = vec![
            (Interval::Named("bc".to_string()), 1),
            (Interval::Named("umi".to_string()), 2),
        ];
        assert_eq!(find_num("bc", &labels), "1");
        assert_eq!(find_num("umi", &labels), "2");
    }
}
