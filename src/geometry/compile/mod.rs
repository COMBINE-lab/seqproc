pub mod definitions;
pub mod functions;
pub mod layout;
pub mod reads;
mod transformation;
pub mod utils;

use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use definitions::compile_definitions;
use reads::{
    compile_read_layouts, CaptureRegistry, ReadLayoutReport, StandardizedLayoutAlternatives,
};
use transformation::compile_transformation;
use utils::Error;

use crate::{
    parser::{
        Annotation, Description, DocumentHeader, HeaderValue, ResourceDeclaration, ResourceRef,
        TransformOutput,
    },
    S,
};

use self::{
    reads::standardize_geometry,
    transformation::label_transformation,
    utils::{GeometryMeta, Interval, TransformSegment, Transformation},
};

/// Identifies the element an annotation is attached to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElementId {
    /// A read declaration (1-based index, e.g., 1 for seq1).
    Read(usize),
    /// A named definition (e.g., "linker1").
    Definition(String),
}

/// Per-element annotation data extracted from the parsed AST.
/// Covers both read-level and definition-level annotations.
#[derive(Debug, Clone)]
pub struct ElementAnnotations {
    /// Which element the annotations are attached to.
    pub element_id: ElementId,
    /// Annotations attached to this element.
    pub annotations: Vec<S<Annotation>>,
}

/// Backward-compatible alias.
pub type ReadAnnotations = ElementAnnotations;

/// Resolve the effective annotations for a definition used within a read.
///
/// Implements hierarchical scoping:
/// - Child (definition-level) annotations override parent (read-level) annotations
///   when they share the same annotation name.
/// - Parent annotations that are not overridden by the child are inherited.
/// - The returned list is ordered: inherited parent annotations first, then child annotations.
///
/// If neither parent nor child has annotations, returns an empty vec.
pub fn resolve_annotations(
    element_annotations: &[ElementAnnotations],
    read_idx: usize,
    def_label: &str,
) -> Vec<S<Annotation>> {
    // Collect parent (read-level) annotations.
    let parent: Vec<&S<Annotation>> = element_annotations
        .iter()
        .filter(|ea| ea.element_id == ElementId::Read(read_idx))
        .flat_map(|ea| ea.annotations.iter())
        .collect();

    // Collect child (definition-level) annotations.
    let child: Vec<&S<Annotation>> = element_annotations
        .iter()
        .filter(|ea| matches!(&ea.element_id, ElementId::Definition(s) if s == def_label))
        .flat_map(|ea| ea.annotations.iter())
        .collect();

    if parent.is_empty() && child.is_empty() {
        return Vec::new();
    }

    // Child annotation names (used for override check).
    let child_names: Vec<&str> = child.iter().map(|S(a, _)| a.name.0.as_str()).collect();

    // Inherited: parent annotations whose name is NOT in child_names.
    let mut resolved: Vec<S<Annotation>> = parent
        .iter()
        .filter(|S(a, _)| !child_names.contains(&a.name.0.as_str()))
        .map(|a| (*a).clone())
        .collect();

    // Then append all child annotations.
    resolved.extend(child.iter().map(|a| (*a).clone()));

    // Deduplicate same-name annotations using last-wins semantics:
    // iterate in reverse, keeping only the first (i.e., last-in-order)
    // occurrence of each name.
    let mut seen = Vec::<&str>::new();
    let mut deduped = Vec::with_capacity(resolved.len());
    for ann in resolved.iter().rev() {
        let name = ann.0.name.0.as_str();
        if !seen.contains(&name) {
            seen.push(name);
            deduped.push(ann.clone());
        }
    }
    deduped.reverse();
    deduped
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
    /// Per-label geometry maps for each arm, containing arm-specific
    /// function stacks (e.g., ReverseComp on bc for the rc arm).
    /// Used by the interpreter to apply arm-specific transformations.
    pub fw_map: HashMap<String, GeometryMeta>,
    pub rc_map: HashMap<String, GeometryMeta>,
    /// Base geometry map before any transformation (used to diff
    /// arm-specific functions).
    pub base_map: HashMap<String, GeometryMeta>,
}

#[derive(Debug)]
pub struct CompiledData {
    /// Effective EFGDL language version. Headerless legacy documents are v1.
    pub efgdl_version: usize,
    /// User-provided document metadata, if an EFGDL header was present.
    pub document_header: Option<DocumentHeader>,
    /// Declared EFGDL 2 runtime resources.
    pub resource_declarations: Vec<ResourceDeclaration>,
    pub geometry: Vec<Vec<GeometryMeta>>,
    /// Bounded, source-ordered alternatives for each input read. Linear
    /// geometries contain exactly one alternative.
    pub layout_alternatives: StandardizedLayoutAlternatives,
    /// Observable normalization details for validation/explain tooling.
    pub layout_report: Vec<ReadLayoutReport>,
    /// Fixed-cardinality public captures and their compiler-lowered labels.
    pub capture_registry: CaptureRegistry,
    pub transformation: Option<Transformation>,
    /// Per-element annotations (reads and definitions) from the input geometry.
    pub element_annotations: Vec<ElementAnnotations>,
    /// Conditional output match block, if present.
    pub match_block: Option<CompiledMatchBlock>,
    /// Deprecation warnings collected during compilation.
    pub warnings: Vec<String>,
}

fn validate_document_header(header: &Option<S<DocumentHeader>>) -> Result<usize, Error> {
    let Some(S(header, header_span)) = header else {
        return Ok(1);
    };

    let mut seen = HashSet::with_capacity(header.fields.len());
    let mut version = None;
    for S(field, field_span) in &header.fields {
        if !seen.insert(field.name.0.as_str()) {
            return Err(Error {
                span: *field_span,
                msg: format!("duplicate EFGDL header field `{}`", field.name.0),
            });
        }
        if field.name.0 == "efgdl" {
            match field.value.0 {
                HeaderValue::Number(value) => version = Some((value, field.value.1)),
                _ => {
                    return Err(Error {
                        span: field.value.1,
                        msg: "EFGDL header field `efgdl` must be an integer version".to_string(),
                    });
                }
            }
        }
    }

    let Some((version, span)) = version else {
        return Err(Error {
            span: *header_span,
            msg: "EFGDL header must contain `efgdl = 2`".to_string(),
        });
    };
    if version != 2 {
        return Err(Error {
            span,
            msg: format!(
                "unsupported EFGDL version {version}; this seqproc build supports EFGDL 2, while headerless files use legacy EFGDL 1 semantics"
            ),
        });
    }
    Ok(version)
}

fn validate_resource_declarations(
    resources: Option<S<Vec<S<ResourceDeclaration>>>>,
    efgdl_version: usize,
) -> Result<Vec<ResourceDeclaration>, Error> {
    let Some(S(resources, span)) = resources else {
        return Ok(Vec::new());
    };
    if efgdl_version != 2 {
        return Err(Error {
            span,
            msg: "named resources require `header { efgdl = 2 }`".to_owned(),
        });
    }
    let mut seen = HashSet::with_capacity(resources.len());
    let mut declarations = Vec::with_capacity(resources.len());
    for S(declaration, declaration_span) in resources {
        if !seen.insert(declaration.name.0.clone()) {
            return Err(Error {
                span: declaration_span,
                msg: format!("duplicate resource declaration `{}`", declaration.name.0),
            });
        }
        declarations.push(declaration);
    }
    Ok(declarations)
}

fn visit_function_resources(
    function: &functions::CompiledFunction,
    visit: &mut impl FnMut(&ResourceRef),
) {
    use functions::CompiledFunction;
    let fallback = match function {
        CompiledFunction::Map(resource, fallback)
        | CompiledFunction::MapWithMismatch(resource, fallback, _)
        | CompiledFunction::MapWithEdit(resource, fallback, _) => {
            visit(resource);
            Some(fallback)
        }
        CompiledFunction::FilterWithinDist(resource, _) | CompiledFunction::AnchorSet(resource) => {
            visit(resource);
            None
        }
        _ => None,
    };
    if let Some(fallback) = fallback {
        for S(function, _) in fallback {
            visit_function_resources(function, visit);
        }
    }
}

fn validate_resource_references(
    geometry: &[Vec<utils::GeometryMeta>],
    declarations: &[ResourceDeclaration],
) -> Result<(), Error> {
    let declared = declarations
        .iter()
        .map(|declaration| declaration.name.0.as_str())
        .collect::<HashSet<_>>();
    for meta in geometry.iter().flatten() {
        for S(function, span) in &meta.stack {
            let mut error = None;
            visit_function_resources(function, &mut |resource| {
                if let ResourceRef::Named(name) = resource {
                    if !declared.contains(name.as_str()) {
                        error = Some(Error {
                            span: *span,
                            msg: format!(
                                "resource `${name}` is referenced but not declared in the `resources` block"
                            ),
                        });
                    }
                }
            });
            if let Some(error) = error {
                return Err(error);
            }
        }
    }
    Ok(())
}

impl CompiledData {
    // v1: FixedSeq or RangedLen
    pub fn is_complex_geometry(&self) -> bool {
        self.geometry.iter().flatten().any(|x| x.is_complex())
    }

    // normalize variable length segments, remove anchors, update lengths
    pub fn get_simplified_description_string(self) -> String {
        if let Some(transformation) = self.transformation {
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

            transformation.into_iter().enumerate().fold(
                String::new(),
                |mut acc, (i, read_transform)| {
                    let geom_desc = read_transform
                        .sequence
                        .into_iter()
                        .map(|segment| match segment {
                            TransformSegment::Label(label) => {
                                let key =
                                    label.split_once('.').map(|(_, key)| key).unwrap_or(&label);
                                // Labels whose simplified description is empty (fixed
                                // sequences and anchors) are normalized away entirely.
                                map.get(key).cloned().unwrap_or_default()
                            }
                            TransformSegment::Literal(bytes) => format!(
                                "f[{}]",
                                std::str::from_utf8(&bytes)
                                    .expect("compiled EFGDL literals are valid ASCII")
                            ),
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
        header,
        resources,
        definitions,
        reads,
        transforms,
    }: Description,
) -> Result<CompiledData, Error> {
    let efgdl_version = validate_document_header(&header)?;
    let document_header = header.map(|S(header, _)| header);
    let resource_declarations = validate_resource_declarations(resources, efgdl_version)?;
    // Extract per-element annotations (reads + definitions).
    let mut element_annotations: Vec<ElementAnnotations> = Vec::new();

    // The interpreter addresses input lanes by their declaration position.
    // Enforce the corresponding 1-based spelling so annotations and runtime
    // metadata cannot silently attach to a different lane.
    for (position, S(read, span)) in reads.0.iter().enumerate() {
        if u8::try_from(read.index.0).is_err() {
            return Err(Error {
                span: *span,
                msg: format!(
                    "read index {} exceeds the supported lane-metadata maximum of {}",
                    read.index.0,
                    u8::MAX
                ),
            });
        }
        let expected = position + 1;
        if read.index.0 != expected {
            return Err(Error {
                span: *span,
                msg: format!(
                    "input reads must be numbered contiguously in declaration order; expected read {expected}, found read {}",
                    read.index.0
                ),
            });
        }
    }

    // Read-level annotations.
    for S(r, _) in reads.0.iter() {
        let mut seen = std::collections::HashSet::new();
        for S(annotation, span) in &r.annotations {
            if !seen.insert(annotation.name.0.as_str()) {
                return Err(Error {
                    span: *span,
                    msg: format!(
                        "read {} specifies annotation `{}` more than once",
                        r.index.0, annotation.name.0
                    ),
                });
            }
            match annotation.name.0.as_str() {
                "match_ori" => {
                    if annotation.value.is_some()
                        || annotation.args.len() != 1
                        || annotation.args[0].0 != "either"
                    {
                        return Err(Error {
                            span: *span,
                            msg: "`match_ori` requires exactly #[match_ori(either)]".to_string(),
                        });
                    }
                }
                "ambig_policy" => {
                    return Err(Error {
                        span: *span,
                        msg: "`ambig_policy` must be attached to the definition containing the map or filter operation"
                            .to_string(),
                    });
                }
                unknown => {
                    return Err(Error {
                        span: *span,
                        msg: format!("unknown read annotation `{unknown}`; expected match_ori"),
                    });
                }
            }
        }
        if !r.annotations.is_empty() {
            element_annotations.push(ElementAnnotations {
                element_id: ElementId::Read(r.index.0),
                annotations: r.annotations.clone(),
            });
        }
    }

    // Definition-level annotations.
    for S(def, _) in definitions.0.iter() {
        if !def.annotations.is_empty() {
            element_annotations.push(ElementAnnotations {
                element_id: ElementId::Definition(def.label.0.clone()),
                annotations: def.annotations.clone(),
            });
        }
    }

    // Validate read indices fit in u8, required by TryOrientationOp.
    for ea in &element_annotations {
        if let ElementId::Read(idx) = &ea.element_id {
            if u8::try_from(*idx).is_err() {
                let span = reads
                    .0
                    .iter()
                    .find(|S(r, _)| r.index.0 == *idx)
                    .map(|S(_, s)| *s)
                    .unwrap_or_default();
                return Err(Error {
                    span,
                    msg: format!(
                        "read index {} exceeds maximum (255); \
                         annotated reads must have indices that fit in a u8",
                        idx
                    ),
                });
            }
        }
    }

    // validate definition block
    let (map, warnings) = {
        let def_res = compile_definitions(definitions);

        if let Err(e) = def_res {
            return Err(e);
        } else {
            def_res.ok().unwrap()
        }
    };
    let validate_read_res = compile_read_layouts(reads, map, efgdl_version);

    let Ok((map, geometry, layout_alternatives, layout_report, capture_registry)) =
        validate_read_res
    else {
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
            let (transformation, map) = compile_transformation(
                S(transform_reads, span),
                map,
                &numbered_labels,
                efgdl_version,
                &capture_registry,
            )?;

            let transformation = label_transformation(transformation, &numbered_labels);

            let geometry = standardize_geometry(map, geometry);
            validate_resource_references(&geometry, &resource_declarations)?;

            Ok(CompiledData {
                efgdl_version,
                document_header,
                resource_declarations: resource_declarations.clone(),
                geometry,
                layout_alternatives,
                layout_report,
                capture_registry,
                transformation: Some(transformation),
                element_annotations,
                match_block: None,
                warnings,
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
            let (fw_transformation, fw_map) = compile_transformation(
                S(fw_arm, span),
                map.clone(),
                &numbered_labels,
                efgdl_version,
                &capture_registry,
            )?;
            let (rc_transformation, rc_map) = compile_transformation(
                S(rc_arm, span),
                map.clone(),
                &numbered_labels,
                efgdl_version,
                &capture_registry,
            )?;
            let base_map = map;

            let fw_transformation = label_transformation(fw_transformation, &numbered_labels);
            let rc_transformation = label_transformation(rc_transformation, &numbered_labels);

            // LANG-COND-OUTPUT: Different fw and rc arms are now supported.
            // The interpreter uses SelectOp to conditionally apply the
            // correct arm based on the runtime attribute value (e.g., ori).
            // When arms are identical, the behavior is the same as before.

            // Runtime branch metadata currently has one producer: the `ori`
            // lane attribute emitted by `#[match_ori(either)]`. Accepting an
            // arbitrary spelling here would compile a switch whose arms can
            // never be selected, silently passing through untransformed reads.
            if attr.0 != "ori" {
                return Err(Error {
                    span,
                    msg: format!(
                        "match blocks currently support only the 'ori' attribute; \
                         '{}.{}' has no runtime producer",
                        read_ref.0, attr.0
                    ),
                });
            }
            if u8::try_from(read_ref.0).is_err() {
                return Err(Error {
                    span,
                    msg: format!(
                        "match block read index {} exceeds the supported lane-metadata range 0..={}",
                        read_ref.0,
                        u8::MAX
                    ),
                });
            }
            let read_has_match_ori = element_annotations.iter().any(|ea| {
                ea.element_id == ElementId::Read(read_ref.0)
                    && ea.annotations.iter().any(|S(ann, _)| {
                        ann.name.0 == "match_ori"
                            && ann.args.first().map(|a| a.0.as_str()) == Some("either")
                    })
            });
            if !read_has_match_ori {
                return Err(Error {
                    span,
                    msg: format!(
                        "match block branches on '{}.{}' but read {} does not have \
                         #[match_ori(either)] annotation; the '{}' attribute would \
                         never be set at runtime",
                        read_ref.0, attr.0, read_ref.0, attr.0
                    ),
                });
            }

            let geometry = standardize_geometry(fw_map.clone(), geometry);
            validate_resource_references(&geometry, &resource_declarations)?;

            Ok(CompiledData {
                efgdl_version,
                document_header,
                resource_declarations: resource_declarations.clone(),
                geometry,
                layout_alternatives,
                layout_report,
                capture_registry,
                transformation: Some(fw_transformation.clone()),
                element_annotations,
                match_block: Some(CompiledMatchBlock {
                    read_ref: read_ref.0,
                    attr: attr.0,
                    fw_transformation,
                    rc_transformation,
                    fw_map,
                    rc_map,
                    base_map,
                }),
                warnings,
            })
        }
        None => {
            let geometry = standardize_geometry(map, geometry);
            validate_resource_references(&geometry, &resource_declarations)?;

            Ok(CompiledData {
                efgdl_version,
                document_header,
                resource_declarations,
                geometry,
                layout_alternatives,
                layout_report,
                capture_registry,
                transformation: None,
                element_annotations,
                match_block: None,
                warnings,
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
                value: None,
            },
            span,
        );
        S(
            Read {
                annotations: vec![annotation],
                output_header: None,
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
            header: None,
            resources: None,
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
            header: None,
            resources: None,
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

    fn make_annotation(name: &str, args: &[&str]) -> S<Annotation> {
        let span = (0..1).into();
        S(
            Annotation {
                name: S(name.to_string(), span),
                args: args.iter().map(|a| S(a.to_string(), span)).collect(),
                value: None,
            },
            span,
        )
    }

    #[test]
    fn resolve_annotations_empty() {
        let eas: Vec<ElementAnnotations> = vec![];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert!(resolved.is_empty());
    }

    #[test]
    fn resolve_annotations_parent_only() {
        // Read has edit(3), definition has nothing -> inherits edit(3)
        let eas = vec![ElementAnnotations {
            element_id: ElementId::Read(1),
            annotations: vec![make_annotation("edit", &["3"])],
        }];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0].0.name.0, "edit");
        assert_eq!(resolved[0].0.args[0].0, "3");
    }

    #[test]
    fn resolve_annotations_child_only() {
        // Read has nothing, definition has edit(5) -> just edit(5)
        let eas = vec![ElementAnnotations {
            element_id: ElementId::Definition("linker1".to_string()),
            annotations: vec![make_annotation("edit", &["5"])],
        }];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0].0.name.0, "edit");
        assert_eq!(resolved[0].0.args[0].0, "5");
    }

    #[test]
    fn resolve_annotations_child_overrides_parent() {
        // Read has edit(3), definition has edit(5) -> child wins: edit(5)
        let eas = vec![
            ElementAnnotations {
                element_id: ElementId::Read(1),
                annotations: vec![make_annotation("edit", &["3"])],
            },
            ElementAnnotations {
                element_id: ElementId::Definition("linker1".to_string()),
                annotations: vec![make_annotation("edit", &["5"])],
            },
        ];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0].0.name.0, "edit");
        assert_eq!(resolved[0].0.args[0].0, "5");
    }

    #[test]
    fn resolve_annotations_inheritance_different_names() {
        // Read has match_ori(either), definition has edit(5) -> both present
        let eas = vec![
            ElementAnnotations {
                element_id: ElementId::Read(1),
                annotations: vec![make_annotation("match_ori", &["either"])],
            },
            ElementAnnotations {
                element_id: ElementId::Definition("linker1".to_string()),
                annotations: vec![make_annotation("edit", &["5"])],
            },
        ];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert_eq!(resolved.len(), 2);
        // Inherited parent first, then child
        assert_eq!(resolved[0].0.name.0, "match_ori");
        assert_eq!(resolved[1].0.name.0, "edit");
    }

    #[test]
    fn resolve_annotations_stacking() {
        // Definition has two annotations stacked
        let eas = vec![ElementAnnotations {
            element_id: ElementId::Definition("linker1".to_string()),
            annotations: vec![
                make_annotation("search", &["relative"]),
                make_annotation("edit", &["5"]),
            ],
        }];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert_eq!(resolved.len(), 2);
        assert_eq!(resolved[0].0.name.0, "search");
        assert_eq!(resolved[1].0.name.0, "edit");
    }

    #[test]
    fn resolve_annotations_wrong_read_idx() {
        // Read 2 has annotation, but we resolve for read 1 -> no parent inheritance
        let eas = vec![
            ElementAnnotations {
                element_id: ElementId::Read(2),
                annotations: vec![make_annotation("edit", &["3"])],
            },
            ElementAnnotations {
                element_id: ElementId::Definition("linker1".to_string()),
                annotations: vec![make_annotation("edit", &["5"])],
            },
        ];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0].0.args[0].0, "5");
    }

    #[test]
    fn resolve_annotations_dedup_parent_same_name() {
        // BUG 6: If the parent read has duplicate annotations with the same
        // name (e.g., #[edit(3)] #[edit(5)]), resolve_annotations should
        // deduplicate using last-wins semantics, returning only edit(5).
        // Currently it returns both, which violates the stacking rule.
        let eas = vec![ElementAnnotations {
            element_id: ElementId::Read(1),
            annotations: vec![
                make_annotation("edit", &["3"]),
                make_annotation("edit", &["5"]),
            ],
        }];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        // Last-wins: only edit(5) should survive
        assert_eq!(
            resolved.len(),
            1,
            "duplicate parent annotations should be deduped"
        );
        assert_eq!(resolved[0].0.name.0, "edit");
        assert_eq!(resolved[0].0.args[0].0, "5");
    }

    #[test]
    fn resolve_annotations_dedup_child_same_name() {
        // BUG 6: If the child definition has duplicate annotations with the
        // same name, resolve_annotations should deduplicate using last-wins.
        let eas = vec![ElementAnnotations {
            element_id: ElementId::Definition("linker1".to_string()),
            annotations: vec![
                make_annotation("edit", &["3"]),
                make_annotation("edit", &["7"]),
            ],
        }];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        assert_eq!(
            resolved.len(),
            1,
            "duplicate child annotations should be deduped"
        );
        assert_eq!(resolved[0].0.name.0, "edit");
        assert_eq!(resolved[0].0.args[0].0, "7");
    }

    #[test]
    fn resolve_annotations_dedup_preserves_different_names() {
        // BUG 6: Dedup should only collapse same-name annotations, not
        // different-name ones. edit + search should both survive.
        let eas = vec![ElementAnnotations {
            element_id: ElementId::Read(1),
            annotations: vec![
                make_annotation("edit", &["3"]),
                make_annotation("search", &["relative"]),
                make_annotation("edit", &["5"]),
            ],
        }];
        let resolved = resolve_annotations(&eas, 1, "linker1");
        // edit(3) is overridden by edit(5); search(relative) is kept
        assert_eq!(
            resolved.len(),
            2,
            "different-name annotations should be preserved"
        );
        assert_eq!(resolved[0].0.name.0, "search");
        assert_eq!(resolved[1].0.name.0, "edit");
        assert_eq!(resolved[1].0.args[0].0, "5");
    }

    #[test]
    fn compile_extracts_definition_annotations() {
        // Compile a geometry with an annotated definition and verify
        // element_annotations contains both read-level and definition-level entries.
        use crate::execute::compile_geom;
        let geom =
            "#[edit(3)] linker1 = f[CAGAGC]\n#[match_ori(either)] 1{b[8]<linker1>r:}".to_string();
        let data = compile_geom(geom).expect("should compile");
        // Should have two element_annotations: one for read 1 and one for linker1
        assert_eq!(data.element_annotations.len(), 2);
        let read_ann = data
            .element_annotations
            .iter()
            .find(|ea| ea.element_id == ElementId::Read(1))
            .expect("should have read annotation");
        assert_eq!(read_ann.annotations[0].0.name.0, "match_ori");
        let def_ann = data
            .element_annotations
            .iter()
            .find(|ea| ea.element_id == ElementId::Definition("linker1".to_string()))
            .expect("should have definition annotation");
        assert_eq!(def_ann.annotations[0].0.name.0, "edit");
    }
}
