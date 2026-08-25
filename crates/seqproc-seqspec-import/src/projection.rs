use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::{
    diagnostic::{DiagnosticCategory, ImportDiagnostic, ImportStatus, Severity},
    model::{Assay, Onlist, ReadSpec, Region},
    render::{render_geometry, GeometryPlan, ProjectedRegion, ProjectedSequenceKind},
    ImportError,
};

pub const SEQSPEC_IMPORT_REPORT_VERSION: &str = "1.0.0";
pub const DEFAULT_MAX_LANES: usize = 8;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OnlistPolicy {
    #[default]
    Exact,
    Capture,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PatternOrientation {
    Forward,
    ReverseComplement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PatternProjection {
    Prefix { max_len: usize },
    Suffix { max_len: usize },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportOptions {
    #[serde(default)]
    pub modalities: Vec<String>,
    #[serde(default)]
    pub reads: Vec<String>,
    #[serde(default)]
    pub onlist_policy: OnlistPolicy,
    #[serde(default = "default_max_lanes")]
    pub max_lanes: usize,
}

fn default_max_lanes() -> usize {
    DEFAULT_MAX_LANES
}

impl Default for ImportOptions {
    fn default() -> Self {
        Self {
            modalities: Vec::new(),
            reads: Vec::new(),
            onlist_policy: OnlistPolicy::Exact,
            max_lanes: DEFAULT_MAX_LANES,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputFile {
    pub file_id: String,
    pub filename: String,
    pub filetype: String,
    pub filesize: i64,
    pub locator: String,
    pub locator_type: String,
    pub md5: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputLane {
    pub lane: usize,
    pub read_id: String,
    pub name: String,
    pub strand: String,
    pub min_len: usize,
    pub max_len: usize,
    pub files: Vec<InputFile>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResourceRequirement {
    pub name: String,
    pub region_id: String,
    pub filename: String,
    pub filetype: String,
    pub filesize: i64,
    pub locator: String,
    pub locator_type: String,
    pub md5: String,
    pub orientation: PatternOrientation,
    pub projection: Option<PatternProjection>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratedGeometry {
    pub modality: String,
    pub read_ids: Vec<String>,
    pub status: ImportStatus,
    pub efgdl: String,
    pub efgdl_digest: String,
    pub resources: Vec<ResourceRequirement>,
    pub inputs: Vec<InputLane>,
    pub diagnostics: Vec<ImportDiagnostic>,
    #[serde(skip)]
    plan: Option<GeometryPlan>,
}

impl GeneratedGeometry {
    /// Render the same deterministic geometry with geometry-relative default
    /// paths for resources that were resolved by the CLI layer.
    pub fn render_with_resource_defaults(&self, defaults: &BTreeMap<String, String>) -> String {
        self.plan
            .as_ref()
            .map(|plan| render_geometry(plan, defaults))
            .unwrap_or_else(|| self.efgdl.clone())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportAssessment {
    pub report_schema_version: String,
    pub source_name: String,
    pub source_digest: String,
    pub seqspec_version: Option<String>,
    pub assay_id: Option<String>,
    pub assay_name: Option<String>,
    pub geometries: Vec<GeneratedGeometry>,
    pub diagnostics: Vec<ImportDiagnostic>,
}

impl ImportAssessment {
    pub fn has_blocking_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .chain(
                self.geometries
                    .iter()
                    .flat_map(|geometry| &geometry.diagnostics),
            )
            .any(|diagnostic| diagnostic.severity == Severity::Error)
    }
}

pub fn assess(
    source: &str,
    source_name: impl Into<String>,
    options: &ImportOptions,
) -> Result<ImportAssessment, ImportError> {
    let source_name = source_name.into();
    let tagged: serde_yaml::Value = serde_yaml::from_str(source).map_err(|error| {
        let location = error.location();
        ImportError::Yaml {
            message: error.to_string(),
            line: location.as_ref().map(|location| location.line()),
            column: location.as_ref().map(|location| location.column()),
        }
    })?;
    let assay: Assay = serde_yaml::from_value(strip_yaml_tags(tagged)).map_err(|error| {
        let location = error.location();
        ImportError::Yaml {
            message: error.to_string(),
            line: location.as_ref().map(|location| location.line()),
            column: location.as_ref().map(|location| location.column()),
        }
    })?;
    let source_digest = format!("blake3:{}", blake3::hash(source.as_bytes()).to_hex());
    let mut diagnostics = validate_assay(&assay, options);

    let modalities = selected_modalities(&assay, options, &mut diagnostics);
    let global_status = blocking_status(&diagnostics);
    let mut geometries = Vec::with_capacity(modalities.len());
    for modality in modalities {
        let mut geometry = assess_modality(&assay, &modality, &source_digest, options);
        if let Some(status) = global_status {
            geometry.status = status;
            geometry.efgdl.clear();
            geometry.efgdl_digest.clear();
            geometry.resources.clear();
            geometry.plan = None;
        }
        geometries.push(geometry);
    }

    Ok(ImportAssessment {
        report_schema_version: SEQSPEC_IMPORT_REPORT_VERSION.to_owned(),
        source_name,
        source_digest,
        seqspec_version: (!assay.seqspec_version.is_empty()).then(|| assay.seqspec_version.clone()),
        assay_id: (!assay.assay_id.is_empty()).then(|| assay.assay_id.clone()),
        assay_name: (!assay.name.is_empty()).then(|| assay.name.clone()),
        geometries,
        diagnostics,
    })
}

fn blocking_status(diagnostics: &[ImportDiagnostic]) -> Option<ImportStatus> {
    let errors = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == Severity::Error)
        .collect::<Vec<_>>();
    if errors
        .iter()
        .any(|diagnostic| diagnostic.category == DiagnosticCategory::SourceInvalid)
    {
        Some(ImportStatus::BlockedSourceInvalid)
    } else if errors
        .iter()
        .any(|diagnostic| diagnostic.category == DiagnosticCategory::UnsupportedCapability)
    {
        Some(ImportStatus::BlockedSeqprocCapability)
    } else if errors
        .iter()
        .any(|diagnostic| diagnostic.category == DiagnosticCategory::NeedsSelection)
    {
        Some(ImportStatus::NeedsUserPolicy)
    } else {
        None
    }
}

fn strip_yaml_tags(value: serde_yaml::Value) -> serde_yaml::Value {
    match value {
        serde_yaml::Value::Tagged(tagged) => strip_yaml_tags(tagged.value),
        serde_yaml::Value::Sequence(values) => {
            serde_yaml::Value::Sequence(values.into_iter().map(strip_yaml_tags).collect())
        }
        serde_yaml::Value::Mapping(values) => serde_yaml::Value::Mapping(
            values
                .into_iter()
                .map(|(key, value)| (strip_yaml_tags(key), strip_yaml_tags(value)))
                .collect(),
        ),
        value => value,
    }
}

fn validate_assay(assay: &Assay, options: &ImportOptions) -> Vec<ImportDiagnostic> {
    let mut diagnostics = Vec::new();
    if !(assay.seqspec_version.starts_with("0.3.") || assay.seqspec_version.starts_with("0.4.")) {
        diagnostics.push(
            ImportDiagnostic::error(
                "unsupported_seqspec_version",
                DiagnosticCategory::UnsupportedCapability,
                format!(
                    "seqspec version `{}` is unsupported; this importer supports 0.3.x and 0.4.x",
                    assay.seqspec_version
                ),
            )
            .at("$.seqspec_version")
            .requiring("seqspec_version_adapter"),
        );
    }
    if assay.sequence_spec.is_empty() {
        diagnostics.push(
            ImportDiagnostic::error(
                "missing_sequence_spec",
                DiagnosticCategory::SourceInvalid,
                "seqspec document contains no sequencing reads",
            )
            .at("$.sequence_spec"),
        );
    }
    if assay.library_spec.is_empty() {
        diagnostics.push(
            ImportDiagnostic::error(
                "missing_library_spec",
                DiagnosticCategory::SourceInvalid,
                "seqspec document contains no library specification",
            )
            .at("$.library_spec"),
        );
    }
    if !options.reads.is_empty() && options.modalities.len() != 1 {
        diagnostics.push(
            ImportDiagnostic::error(
                "read_selection_requires_one_modality",
                DiagnosticCategory::NeedsSelection,
                "explicit read selection requires exactly one --modality",
            )
            .remediate("select one modality and repeat --read for the desired input lanes"),
        );
    }
    if !assay.extra.is_empty() {
        diagnostics.push(
            ImportDiagnostic::warning(
                "unknown_top_level_metadata",
                DiagnosticCategory::Informational,
                format!(
                    "observed and ignored unrecognized top-level metadata fields: {}",
                    assay.extra.keys().cloned().collect::<Vec<_>>().join(", ")
                ),
            )
            .at("$"),
        );
    }
    diagnostics
}

fn selected_modalities(
    assay: &Assay,
    options: &ImportOptions,
    diagnostics: &mut Vec<ImportDiagnostic>,
) -> Vec<String> {
    let requested = if options.modalities.is_empty() {
        if !assay.modalities.is_empty() {
            assay.modalities.clone()
        } else {
            diagnostics.push(
                ImportDiagnostic::warning(
                    "modalities_derived_from_library",
                    DiagnosticCategory::LossyNormalization,
                    "modalities is empty; deriving modality identifiers from top-level library regions",
                )
                .at("$.modalities"),
            );
            assay
                .library_spec
                .iter()
                .map(|region| region.region_id.clone())
                .collect()
        }
    } else {
        options.modalities.clone()
    };

    let mut seen = BTreeSet::new();
    requested
        .into_iter()
        .filter(|modality| {
            if modality.is_empty() {
                diagnostics.push(
                    ImportDiagnostic::error(
                        "empty_modality",
                        DiagnosticCategory::SourceInvalid,
                        "modality identifiers cannot be empty",
                    )
                    .at("$.modalities"),
                );
                false
            } else {
                seen.insert(modality.clone())
            }
        })
        .collect()
}

fn assess_modality(
    assay: &Assay,
    modality: &str,
    source_digest: &str,
    options: &ImportOptions,
) -> GeneratedGeometry {
    let mut diagnostics = Vec::new();
    let library_candidates = assay
        .library_spec
        .iter()
        .filter(|region| region.region_id == modality)
        .collect::<Vec<_>>();
    let library = match library_candidates.as_slice() {
        [library] => Some(*library),
        [] => {
            diagnostics.push(
                ImportDiagnostic::error(
                    "modality_library_missing",
                    DiagnosticCategory::SourceInvalid,
                    format!("modality `{modality}` has no matching top-level library region"),
                )
                .at("$.library_spec")
                .for_modality(modality)
                .remediate(
                    "correct the modality/library region identifier or select another modality",
                ),
            );
            None
        }
        _ => {
            diagnostics.push(
                ImportDiagnostic::error(
                    "modality_library_duplicate",
                    DiagnosticCategory::SourceInvalid,
                    format!("modality `{modality}` has multiple matching library regions"),
                )
                .at("$.library_spec")
                .for_modality(modality),
            );
            None
        }
    };

    let reads = select_reads(assay, modality, options, &mut diagnostics);
    if reads.len() > options.max_lanes {
        diagnostics.push(
            ImportDiagnostic::error(
                "input_lane_limit",
                DiagnosticCategory::UnsupportedCapability,
                format!(
                    "modality `{modality}` requires {} reads, exceeding this build's {}-lane limit",
                    reads.len(),
                    options.max_lanes
                ),
            )
            .for_modality(modality)
            .requiring("additional_input_lanes")
            .remediate(
                "select at most eight reads explicitly or extend seqproc's bounded lane interface",
            ),
        );
    }

    let mut projected_reads = Vec::new();
    if let Some(library) = library {
        if reads.len() <= options.max_lanes {
            for (lane, read) in reads.iter().enumerate() {
                match project_read(library, read, lane + 1) {
                    Ok((projection, mut read_diagnostics)) => {
                        diagnostics.append(&mut read_diagnostics);
                        projected_reads.push(projection);
                    }
                    Err(diagnostic) => diagnostics.push((*diagnostic).for_modality(modality)),
                }
            }
        }
    }

    let has_source_error = diagnostics.iter().any(|diagnostic| {
        diagnostic.severity == Severity::Error
            && diagnostic.category == DiagnosticCategory::SourceInvalid
    });
    let has_capability_error = diagnostics.iter().any(|diagnostic| {
        diagnostic.severity == Severity::Error
            && diagnostic.category == DiagnosticCategory::UnsupportedCapability
    });
    let has_selection_error = diagnostics.iter().any(|diagnostic| {
        diagnostic.severity == Severity::Error
            && diagnostic.category == DiagnosticCategory::NeedsSelection
    });

    let mut status = if has_source_error {
        ImportStatus::BlockedSourceInvalid
    } else if has_capability_error {
        ImportStatus::BlockedSeqprocCapability
    } else if has_selection_error {
        ImportStatus::NeedsUserPolicy
    } else if !options.reads.is_empty() {
        ImportStatus::SupportedWithSelection
    } else {
        ImportStatus::Supported
    };

    let projected_bounds = projected_reads
        .iter()
        .map(|read| (read.lane, (read.min_len, read.max_len)))
        .collect::<BTreeMap<_, _>>();
    let plan = if status.generated() {
        Some(GeometryPlan::new(
            &assay.seqspec_version,
            &assay.assay_id,
            &assay.name,
            modality,
            source_digest,
            projected_reads,
            options.onlist_policy,
        ))
    } else {
        None
    };
    let efgdl = plan
        .as_ref()
        .map(|plan| render_geometry(plan, &BTreeMap::new()))
        .unwrap_or_default();
    let efgdl_digest = if efgdl.is_empty() {
        String::new()
    } else {
        format!("blake3:{}", blake3::hash(efgdl.as_bytes()).to_hex())
    };
    let resources = plan
        .as_ref()
        .map(GeometryPlan::resources)
        .unwrap_or_default();
    // Assessment does not touch the filesystem or network. A locator is only
    // a resolution hint, not proof that a runtime resource is present, so any
    // exact-onlist geometry remains binding-dependent until the CLI workflow
    // resolves and verifies it.
    if options.onlist_policy == OnlistPolicy::Exact && !resources.is_empty() && status.generated() {
        status = ImportStatus::SupportedRequiresBinding;
    }
    let inputs = reads
        .iter()
        .enumerate()
        .filter_map(|(lane, read)| {
            input_lane(
                lane + 1,
                read,
                projected_bounds.get(&(lane + 1)).copied(),
                &mut diagnostics,
            )
        })
        .collect();

    GeneratedGeometry {
        modality: modality.to_owned(),
        read_ids: reads.iter().map(|read| read.read_id.clone()).collect(),
        status,
        efgdl,
        efgdl_digest,
        resources,
        inputs,
        diagnostics,
        plan,
    }
}

fn select_reads<'a>(
    assay: &'a Assay,
    modality: &str,
    options: &ImportOptions,
    diagnostics: &mut Vec<ImportDiagnostic>,
) -> Vec<&'a ReadSpec> {
    let reads = if options.reads.is_empty() {
        let library = assay
            .library_spec
            .iter()
            .find(|region| region.region_id == modality);
        assay
            .sequence_spec
            .iter()
            .filter(|read| {
                read.modality == modality
                    || library.is_some_and(|library| {
                        library
                            .find_descendant(&read.read_id)
                            .is_some_and(|region| region.region_type.eq_ignore_ascii_case("fastq"))
                    })
            })
            .collect::<Vec<_>>()
    } else if options.modalities.len() == 1 && options.modalities[0] == modality {
        options
            .reads
            .iter()
            .filter_map(|read_id| {
                let matches = assay
                    .sequence_spec
                    .iter()
                    .filter(|read| read.read_id == *read_id)
                    .collect::<Vec<_>>();
                match matches.as_slice() {
                    [read] => Some(*read),
                    [] => {
                        diagnostics.push(
                            ImportDiagnostic::error(
                                "selected_read_missing",
                                DiagnosticCategory::NeedsSelection,
                                format!("selected read `{read_id}` was not found"),
                            )
                            .at("$.sequence_spec")
                            .for_modality(modality)
                            .for_read(read_id),
                        );
                        None
                    }
                    _ => {
                        diagnostics.push(
                            ImportDiagnostic::error(
                                "selected_read_duplicate",
                                DiagnosticCategory::SourceInvalid,
                                format!("read identifier `{read_id}` is not unique"),
                            )
                            .at("$.sequence_spec")
                            .for_modality(modality)
                            .for_read(read_id),
                        );
                        None
                    }
                }
            })
            .collect()
    } else {
        Vec::new()
    };
    if reads.is_empty() {
        diagnostics.push(
            ImportDiagnostic::error(
                "modality_reads_missing",
                DiagnosticCategory::NeedsSelection,
                format!("modality `{modality}` has no unambiguously associated reads"),
            )
            .at("$.sequence_spec")
            .for_modality(modality)
            .remediate("select the modality and its read IDs explicitly"),
        );
    }
    reads
}

fn input_lane(
    lane: usize,
    read: &ReadSpec,
    projected_bounds: Option<(usize, usize)>,
    diagnostics: &mut Vec<ImportDiagnostic>,
) -> Option<InputLane> {
    let (min_len, max_len) = match projected_bounds {
        Some(bounds) => bounds,
        None => (
            usize::try_from(read.min_len).ok()?,
            usize::try_from(read.max_len).ok()?,
        ),
    };
    if !read.extra.is_empty() {
        diagnostics.push(
            ImportDiagnostic::warning(
                "unknown_read_metadata",
                DiagnosticCategory::Informational,
                format!(
                    "observed and ignored unrecognized metadata for read `{}`: {}",
                    read.read_id,
                    read.extra.keys().cloned().collect::<Vec<_>>().join(", ")
                ),
            )
            .for_read(&read.read_id),
        );
    }
    Some(InputLane {
        lane,
        read_id: read.read_id.clone(),
        name: read.name.clone(),
        strand: read.strand.clone(),
        min_len,
        max_len,
        files: read
            .files
            .iter()
            .map(|file| InputFile {
                file_id: file.file_id.clone(),
                filename: file.filename.clone(),
                filetype: file.filetype.clone(),
                filesize: file.filesize,
                locator: if file.url.is_empty() {
                    file.filename.clone()
                } else {
                    file.url.clone()
                },
                locator_type: if file.urltype.is_empty() {
                    "local".to_owned()
                } else {
                    file.urltype.clone()
                },
                md5: file.md5.clone(),
            })
            .collect(),
    })
}

fn project_read(
    library: &Region,
    read: &ReadSpec,
    lane: usize,
) -> Result<(crate::render::ProjectedRead, Vec<ImportDiagnostic>), Box<ImportDiagnostic>> {
    if read.read_id.is_empty() {
        return Err(Box::new(
            ImportDiagnostic::error(
                "empty_read_id",
                DiagnosticCategory::SourceInvalid,
                "read identifier cannot be empty",
            )
            .at("$.sequence_spec[].read_id"),
        ));
    }
    if read.primer_id.is_empty() {
        return Err(Box::new(
            ImportDiagnostic::error(
                "empty_primer_id",
                DiagnosticCategory::SourceInvalid,
                "read primer identifier cannot be empty",
            )
            .for_read(&read.read_id),
        ));
    }
    if read.min_len < 0 || read.max_len < 0 || read.min_len > read.max_len {
        return Err(Box::new(
            ImportDiagnostic::error(
                "invalid_read_length",
                DiagnosticCategory::SourceInvalid,
                format!(
                    "read `{}` has invalid length range {}..{}",
                    read.read_id, read.min_len, read.max_len
                ),
            )
            .for_read(&read.read_id),
        ));
    }
    // seqspec 0.3 commonly materialized each observed FASTQ as a nested
    // `region_type: fastq` region. That representation is already in observed
    // read order and is more precise than its generic-primer metadata (which
    // often declares the sequencer maximum of 250 bases). Prefer it when
    // present; 0.4 documents generally use the primer/window representation
    // handled below.
    if let Some(fastq_region) = library
        .find_descendant(&read.read_id)
        .filter(|region| region.region_type.eq_ignore_ascii_case("fastq"))
    {
        return project_materialized_fastq(fastq_region, read, lane);
    }
    let reverse = match read.strand.as_str() {
        "pos" => false,
        "neg" => true,
        other => {
            return Err(Box::new(
                ImportDiagnostic::error(
                    "invalid_read_strand",
                    DiagnosticCategory::SourceInvalid,
                    format!("read `{}` has unsupported strand `{other}`", read.read_id),
                )
                .for_read(&read.read_id),
            ))
        }
    };

    let mut leaves = Vec::new();
    library.flattened_with_primer(&read.primer_id, &mut leaves);
    let primer_positions = leaves
        .iter()
        .enumerate()
        .filter_map(|(index, region)| (region.region_id == read.primer_id).then_some(index))
        .collect::<Vec<_>>();
    let primer_index = match primer_positions.as_slice() {
        [index] => *index,
        [] => {
            return Err(Box::new(
                ImportDiagnostic::error(
                    "primer_not_found",
                    DiagnosticCategory::SourceInvalid,
                    format!(
                        "primer `{}` for read `{}` was not found in modality library",
                        read.primer_id, read.read_id
                    ),
                )
                .for_read(&read.read_id),
            ))
        }
        _ => {
            return Err(Box::new(
                ImportDiagnostic::error(
                    "primer_not_unique",
                    DiagnosticCategory::SourceInvalid,
                    format!(
                        "primer `{}` for read `{}` occurs more than once",
                        read.primer_id, read.read_id
                    ),
                )
                .for_read(&read.read_id),
            ))
        }
    };

    let ordered = if reverse {
        leaves[..primer_index].iter().rev().collect::<Vec<_>>()
    } else {
        leaves[primer_index + 1..].iter().collect::<Vec<_>>()
    };
    let mut remaining = usize::try_from(read.max_len).unwrap_or(0);
    let read_minimum = usize::try_from(read.min_len).unwrap_or(0);
    let mut consumed_maximum = 0usize;
    let mut projected = Vec::new();
    let mut diagnostics = Vec::new();
    for region in ordered {
        if remaining == 0 {
            break;
        }
        let min_len = usize::try_from(region.min_len).map_err(|_| {
            ImportDiagnostic::error(
                "invalid_region_length",
                DiagnosticCategory::SourceInvalid,
                format!(
                    "region `{}` has a negative minimum length",
                    region.region_id
                ),
            )
            .for_read(&read.read_id)
        })?;
        let max_len = usize::try_from(region.max_len).map_err(|_| {
            ImportDiagnostic::error(
                "invalid_region_length",
                DiagnosticCategory::SourceInvalid,
                format!(
                    "region `{}` has a negative maximum length",
                    region.region_id
                ),
            )
            .for_read(&read.read_id)
        })?;
        if min_len > max_len {
            return Err(Box::new(
                ImportDiagnostic::error(
                    "invalid_region_length",
                    DiagnosticCategory::SourceInvalid,
                    format!(
                        "region `{}` has invalid length range {min_len}..{max_len}",
                        region.region_id
                    ),
                )
                .for_read(&read.read_id),
            ));
        }
        if max_len == 0 {
            if region.sequence_type == "onlist" {
                return Err(Box::new(
                    ImportDiagnostic::error(
                        "zero_length_onlist",
                        DiagnosticCategory::SourceInvalid,
                        format!("onlist region `{}` has zero length", region.region_id),
                    )
                    .for_read(&read.read_id),
                ));
            }
            diagnostics.push(
                ImportDiagnostic::warning(
                    "zero_length_region_ignored",
                    DiagnosticCategory::LossyNormalization,
                    format!("ignored zero-length region `{}`", region.region_id),
                )
                .for_read(&read.read_id),
            );
            continue;
        }
        let observed_max = remaining.min(max_len);
        let clipped = observed_max < max_len;
        let minimum_observed = read_minimum
            .saturating_sub(consumed_maximum)
            .min(observed_max);
        let observed_min = min_len.min(observed_max).min(minimum_observed);
        let kind = projected_kind(region, reverse, observed_max, &read.read_id)?;
        projected.push(ProjectedRegion {
            source_region_id: region.region_id.clone(),
            region_type: region.region_type.clone(),
            min_len: observed_min,
            max_len: observed_max,
            source_min_len: min_len,
            kind,
            onlist: region.onlist.clone(),
            orientation: if reverse {
                PatternOrientation::ReverseComplement
            } else {
                PatternOrientation::Forward
            },
            pattern_projection: if region.sequence_type == "onlist" && clipped {
                Some(if reverse {
                    PatternProjection::Suffix {
                        max_len: observed_max,
                    }
                } else {
                    PatternProjection::Prefix {
                        max_len: observed_max,
                    }
                })
            } else {
                None
            },
        });
        consumed_maximum = consumed_maximum.saturating_add(observed_max);
        remaining = remaining.saturating_sub(observed_max);
    }
    if remaining > 0 {
        return Err(Box::new(
            ImportDiagnostic::error(
                "read_window_exceeds_library",
                DiagnosticCategory::SourceInvalid,
                format!(
                    "read `{}` extends {remaining} bases beyond the described library",
                    read.read_id
                ),
            )
            .for_read(&read.read_id),
        ));
    }

    validate_read_window_variability(read, &projected, &mut diagnostics);
    validate_projected_regions(read, &projected, &mut diagnostics);

    Ok((
        crate::render::ProjectedRead {
            lane,
            read_id: read.read_id.clone(),
            min_len: usize::try_from(read.min_len).unwrap_or(0),
            max_len: usize::try_from(read.max_len).unwrap_or(0),
            regions: projected,
        },
        diagnostics,
    ))
}

fn project_materialized_fastq(
    fastq: &Region,
    read: &ReadSpec,
    lane: usize,
) -> Result<(crate::render::ProjectedRead, Vec<ImportDiagnostic>), Box<ImportDiagnostic>> {
    let min_len = usize::try_from(fastq.min_len).map_err(|_| {
        ImportDiagnostic::error(
            "invalid_fastq_region_length",
            DiagnosticCategory::SourceInvalid,
            format!(
                "FASTQ region `{}` has a negative minimum length",
                fastq.region_id
            ),
        )
        .for_read(&read.read_id)
    })?;
    let max_len = usize::try_from(fastq.max_len).map_err(|_| {
        ImportDiagnostic::error(
            "invalid_fastq_region_length",
            DiagnosticCategory::SourceInvalid,
            format!(
                "FASTQ region `{}` has a negative maximum length",
                fastq.region_id
            ),
        )
        .for_read(&read.read_id)
    })?;
    if min_len > max_len {
        return Err(Box::new(
            ImportDiagnostic::error(
                "invalid_fastq_region_length",
                DiagnosticCategory::SourceInvalid,
                format!("FASTQ region `{}` has invalid bounds", fastq.region_id),
            )
            .for_read(&read.read_id),
        ));
    }
    let mut leaves = Vec::new();
    for region in &fastq.regions {
        flatten_leaves(region, &mut leaves);
    }
    if leaves.is_empty() {
        leaves.push(fastq);
    }
    let mut projected = Vec::with_capacity(leaves.len());
    for region in leaves {
        let region_min = usize::try_from(region.min_len).map_err(|_| {
            ImportDiagnostic::error(
                "invalid_region_length",
                DiagnosticCategory::SourceInvalid,
                format!(
                    "region `{}` has a negative minimum length",
                    region.region_id
                ),
            )
            .for_read(&read.read_id)
        })?;
        let region_max = usize::try_from(region.max_len).map_err(|_| {
            ImportDiagnostic::error(
                "invalid_region_length",
                DiagnosticCategory::SourceInvalid,
                format!(
                    "region `{}` has a negative maximum length",
                    region.region_id
                ),
            )
            .for_read(&read.read_id)
        })?;
        let kind = projected_kind(region, false, region_max, &read.read_id)?;
        projected.push(ProjectedRegion {
            source_region_id: region.region_id.clone(),
            region_type: region.region_type.clone(),
            min_len: region_min,
            max_len: region_max,
            source_min_len: region_min,
            kind,
            onlist: region.onlist.clone(),
            orientation: PatternOrientation::Forward,
            pattern_projection: None,
        });
    }
    let mut diagnostics = Vec::new();
    validate_projected_regions(read, &projected, &mut diagnostics);
    Ok((
        crate::render::ProjectedRead {
            lane,
            read_id: read.read_id.clone(),
            min_len,
            max_len,
            regions: projected,
        },
        diagnostics,
    ))
}

fn flatten_leaves<'a>(region: &'a Region, output: &mut Vec<&'a Region>) {
    if region.regions.is_empty() {
        output.push(region);
    } else {
        for child in &region.regions {
            flatten_leaves(child, output);
        }
    }
}

fn projected_kind(
    region: &Region,
    reverse: bool,
    observed_max: usize,
    read_id: &str,
) -> Result<ProjectedSequenceKind, Box<ImportDiagnostic>> {
    match region.sequence_type.as_str() {
        "fixed" => {
            let sequence = region.sequence.as_bytes();
            if region.min_len != region.max_len
                || usize::try_from(region.max_len).ok() != Some(sequence.len())
            {
                return Err(Box::new(
                    ImportDiagnostic::error(
                        "fixed_sequence_length_mismatch",
                        DiagnosticCategory::SourceInvalid,
                        format!(
                            "fixed region `{}` has sequence length {} but declares {}..{}; fixed regions must have one consistent length",
                            region.region_id,
                            sequence.len(),
                            region.min_len,
                            region.max_len
                        ),
                    )
                    .for_read(read_id),
                ));
            }
            // In IUPAC, an all-N fixed field imposes no sequence constraint.
            // Representing it as a length-constrained observed interval is
            // exact and avoids constructing a useless 4^N pattern expansion.
            if sequence.iter().all(|base| *base == b'N') {
                return Ok(ProjectedSequenceKind::Random);
            }
            if !sequence
                .iter()
                .all(|base| matches!(base, b'A' | b'C' | b'G' | b'T'))
            {
                return Err(Box::new(
                    ImportDiagnostic::error(
                        "degenerate_fixed_sequence",
                        DiagnosticCategory::UnsupportedCapability,
                        format!(
                            "fixed region `{}` contains non-ACGT sequence and cannot be represented exactly",
                            region.region_id
                        ),
                    )
                    .for_read(read_id)
                    .requiring("iupac_fixed_matching"),
                ));
            }
            if sequence.len() < observed_max {
                return Err(Box::new(
                    ImportDiagnostic::error(
                        "fixed_sequence_too_short",
                        DiagnosticCategory::SourceInvalid,
                        format!(
                            "fixed region `{}` declares {} observed bases but provides only {}",
                            region.region_id,
                            observed_max,
                            sequence.len()
                        ),
                    )
                    .for_read(read_id),
                ));
            }
            let slice = if reverse {
                &sequence[sequence.len() - observed_max..]
            } else {
                &sequence[..observed_max]
            };
            let literal = if reverse {
                reverse_complement(slice)
            } else {
                String::from_utf8(slice.to_vec()).expect("validated ASCII DNA")
            };
            Ok(ProjectedSequenceKind::Fixed { literal })
        }
        "random" => Ok(ProjectedSequenceKind::Random),
        "onlist" => {
            if region.onlist.is_none() {
                return Err(Box::new(
                    ImportDiagnostic::error(
                        "onlist_metadata_missing",
                        DiagnosticCategory::SourceInvalid,
                        format!(
                            "onlist region `{}` has no onlist metadata",
                            region.region_id
                        ),
                    )
                    .for_read(read_id),
                ));
            }
            Ok(ProjectedSequenceKind::Onlist)
        }
        "joined" if region.regions.is_empty() => Err(Box::new(
            ImportDiagnostic::error(
                "joined_leaf",
                DiagnosticCategory::SourceInvalid,
                format!("joined region `{}` has no child regions", region.region_id),
            )
            .for_read(read_id),
        )),
        other => Err(Box::new(
            ImportDiagnostic::error(
                "unknown_sequence_type",
                DiagnosticCategory::SourceInvalid,
                format!(
                    "region `{}` uses unsupported sequence_type `{other}`",
                    region.region_id
                ),
            )
            .for_read(read_id),
        )),
    }
}

fn validate_read_window_variability(
    read: &ReadSpec,
    projected: &[ProjectedRegion],
    diagnostics: &mut Vec<ImportDiagnostic>,
) {
    if read.min_len == read.max_len || projected.is_empty() {
        return;
    }
    let minimum = usize::try_from(read.min_len).unwrap_or(0);
    let mut cumulative = 0usize;
    let mut variable_boundary_index = None;
    for (index, region) in projected.iter().enumerate() {
        cumulative = cumulative.saturating_add(region.max_len);
        if minimum <= cumulative {
            variable_boundary_index = Some(index);
            break;
        }
    }
    if let Some(index) = variable_boundary_index {
        let prior_maximum = projected[..index]
            .iter()
            .map(|region| region.max_len)
            .sum::<usize>();
        let minimum_observed = minimum
            .saturating_sub(prior_maximum)
            .min(projected[index].max_len);
        if index + 1 < projected.len() {
            diagnostics.push(
                ImportDiagnostic::error(
                    "read_window_spans_regions",
                    DiagnosticCategory::UnsupportedCapability,
                    format!(
                        "read `{}` may end before later projected regions, requiring a nested optional suffix layout",
                        read.read_id
                    ),
                )
                .for_read(&read.read_id)
                .requiring("variable_read_window_layout"),
            );
        }
        if minimum_observed < projected[index].max_len {
            match projected[index].kind {
                ProjectedSequenceKind::Fixed { .. } => diagnostics.push(
                    ImportDiagnostic::error(
                        "variable_partial_fixed_region",
                        DiagnosticCategory::UnsupportedCapability,
                        format!(
                            "read `{}` can terminate within fixed region `{}`",
                            read.read_id, projected[index].source_region_id
                        ),
                    )
                    .for_read(&read.read_id)
                    .requiring("partial_fixed_prefix_matching"),
                ),
                ProjectedSequenceKind::Onlist
                    if minimum_observed < projected[index].source_min_len =>
                {
                    diagnostics.push(
                        ImportDiagnostic::error(
                            "variable_partial_onlist_region",
                            DiagnosticCategory::UnsupportedCapability,
                            format!(
                                "read `{}` can terminate within onlist region `{}`",
                                read.read_id, projected[index].source_region_id
                            ),
                        )
                        .for_read(&read.read_id)
                        .requiring("partial_onlist_window_matching"),
                    );
                }
                ProjectedSequenceKind::Onlist => {}
                ProjectedSequenceKind::Random => {}
            }
        }
    }
}

fn validate_projected_regions(
    read: &ReadSpec,
    projected: &[ProjectedRegion],
    diagnostics: &mut Vec<ImportDiagnostic>,
) {
    for (index, region) in projected.iter().enumerate() {
        let is_terminal = index + 1 == projected.len();
        if region.min_len != region.max_len && !is_terminal {
            let has_exact_anchor = projected
                .get(index + 1)
                .is_some_and(|next| matches!(next.kind, ProjectedSequenceKind::Fixed { .. }));
            if !has_exact_anchor && !matches!(region.kind, ProjectedSequenceKind::Onlist) {
                diagnostics.push(
                    ImportDiagnostic::error(
                        "undelimited_variable_region",
                        DiagnosticCategory::UnsupportedCapability,
                        format!(
                            "variable region `{}` in read `{}` has no exact following anchor",
                            region.source_region_id, read.read_id
                        ),
                    )
                    .for_read(&read.read_id)
                    .requiring("general_variable_boundary_matching"),
                );
            }
        }
    }
}

fn reverse_complement(sequence: &[u8]) -> String {
    sequence
        .iter()
        .rev()
        .map(|base| match base {
            b'A' => 'T',
            b'C' => 'G',
            b'G' => 'C',
            b'T' => 'A',
            _ => unreachable!("fixed sequence was validated"),
        })
        .collect()
}

pub(crate) fn onlist_requirement(
    name: String,
    region: &ProjectedRegion,
    onlist: &Onlist,
) -> ResourceRequirement {
    ResourceRequirement {
        name,
        region_id: region.source_region_id.clone(),
        filename: onlist.filename.clone(),
        filetype: onlist.filetype.clone(),
        filesize: onlist.filesize,
        locator: if onlist.url.is_empty() {
            onlist.filename.clone()
        } else {
            onlist.url.clone()
        },
        locator_type: if onlist.urltype.is_empty() {
            "local".to_owned()
        } else {
            onlist.urltype.clone()
        },
        md5: onlist.md5.clone(),
        orientation: region.orientation,
        projection: region.pattern_projection,
    }
}
