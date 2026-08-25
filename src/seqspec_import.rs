//! Filesystem and resource workflow for conservative seqspec import.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs::{self, File},
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

use md5::{Digest, Md5};
use seqproc_seqspec_import::{
    assess, ImportAssessment, ImportDiagnostic, ImportOptions, ImportStatus, OnlistPolicy,
    ResourceRequirement,
};
use serde::Serialize;
use tempfile::TempDir;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceMode {
    /// Use local resources and download HTTP(S) resources; leave unavailable
    /// resources as explicit runtime bindings.
    Auto,
    /// Never access the network. Resolve only local resources.
    Offline,
    /// Require every onlist resource to resolve and verify during import.
    Required,
}

#[derive(Debug, Clone)]
pub struct SeqspecImportConfig {
    pub source: PathBuf,
    pub output_dir: Option<PathBuf>,
    pub modalities: Vec<String>,
    pub reads: Vec<String>,
    pub onlist_policy: OnlistPolicy,
    pub resource_mode: ResourceMode,
    pub allow_partial: bool,
    pub check_only: bool,
    pub max_lanes: usize,
}

impl SeqspecImportConfig {
    pub fn new(source: PathBuf) -> Self {
        Self {
            source,
            output_dir: None,
            modalities: Vec::new(),
            reads: Vec::new(),
            onlist_policy: OnlistPolicy::Exact,
            resource_mode: ResourceMode::Auto,
            allow_partial: false,
            check_only: false,
            max_lanes: crate::io_config::MAX_INPUT_LANES,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ResourceResolutionStatus {
    Resolved,
    RequiresBinding,
}

#[derive(Debug, Clone, Serialize)]
pub struct ResourceResolution {
    pub geometry: String,
    pub resource: String,
    pub locator: String,
    pub status: ResourceResolutionStatus,
    pub output_path: Option<String>,
    pub stored_byte_count: Option<u64>,
    pub content_byte_count: Option<u64>,
    /// seqspec's MD5 domain: uncompressed file content.
    pub content_md5: Option<String>,
    /// Digest of the exact bytes retained in the import bundle.
    pub stored_blake3: Option<String>,
    /// Digest of the uncompressed resource content consumed by seqproc.
    pub content_blake3: Option<String>,
    pub message: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct SeqspecImportReport {
    pub workflow_schema_version: &'static str,
    pub assessment: ImportAssessment,
    pub resources: Vec<ResourceResolution>,
    pub emitted_geometries: Vec<String>,
}

#[derive(Debug, Error)]
pub enum SeqspecImportError {
    #[error("could not read seqspec source `{path}`: {source}")]
    ReadSource {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error(transparent)]
    Assessment(#[from] seqproc_seqspec_import::ImportError),
    #[error("seqspec import is blocked; inspect the structured diagnostics (use --allow-partial to emit independently supported modalities)")]
    Blocked,
    #[error("--output-dir is required unless --check-only is used")]
    MissingOutput,
    #[error("refusing to replace existing import output `{0}`")]
    OutputExists(PathBuf),
    #[error("could not create import staging area beside `{path}`: {source}")]
    CreateStaging {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("could not write import artifact `{path}`: {source}")]
    WriteArtifact {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("could not finalize import output `{path}`: {source}")]
    Finalize {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error(
        "required resource `{resource}` for modality `{geometry}` could not be resolved: {reason}"
    )]
    RequiredResource {
        geometry: String,
        resource: String,
        reason: String,
    },
    #[error("no supported modality remained to publish")]
    NoGeometryGenerated,
    #[error("generated EFGDL for modality `{geometry}` failed seqproc validation: {reason}")]
    GeneratedGeometry { geometry: String, reason: String },
}

pub fn import_seqspec(
    config: &SeqspecImportConfig,
) -> Result<SeqspecImportReport, SeqspecImportError> {
    let source =
        fs::read_to_string(&config.source).map_err(|source| SeqspecImportError::ReadSource {
            path: config.source.clone(),
            source,
        })?;
    let options = ImportOptions {
        modalities: config.modalities.clone(),
        reads: config.reads.clone(),
        onlist_policy: config.onlist_policy,
        max_lanes: config.max_lanes,
    };
    let assessment = assess(&source, config.source.display().to_string(), &options)?;
    if config.check_only {
        return Ok(SeqspecImportReport {
            workflow_schema_version: "1.0.0",
            assessment,
            resources: Vec::new(),
            emitted_geometries: Vec::new(),
        });
    }
    let has_global_error = assessment
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == seqproc_seqspec_import::Severity::Error);
    if has_global_error {
        return Err(SeqspecImportError::Blocked);
    }
    let has_blocked_geometry = assessment
        .geometries
        .iter()
        .any(|geometry| !geometry.status.generated());
    if has_blocked_geometry && !config.allow_partial {
        return Err(SeqspecImportError::Blocked);
    }
    let output_dir = config
        .output_dir
        .as_ref()
        .ok_or(SeqspecImportError::MissingOutput)?;
    if output_dir.exists() {
        return Err(SeqspecImportError::OutputExists(output_dir.clone()));
    }
    let parent = output_dir.parent().unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent).map_err(|source| SeqspecImportError::CreateStaging {
        path: parent.to_owned(),
        source,
    })?;
    let staging = TempDir::new_in(parent).map_err(|source| SeqspecImportError::CreateStaging {
        path: parent.to_owned(),
        source,
    })?;
    let stage = staging.path();
    write_artifact(&stage.join("source.seqspec.yaml"), source.as_bytes())?;
    fs::create_dir_all(stage.join("resources")).map_err(|source| {
        SeqspecImportError::WriteArtifact {
            path: stage.join("resources"),
            source,
        }
    })?;

    let source_parent = config.source.parent().unwrap_or_else(|| Path::new("."));
    let mut resolutions = Vec::new();
    let mut emitted_geometries = Vec::new();
    let mut used_paths = BTreeSet::new();
    let mut used_geometry_filenames = BTreeSet::new();
    for geometry in &assessment.geometries {
        if !geometry.status.generated() {
            continue;
        }
        let geometry_name = sanitize_filename(&geometry.modality);
        let mut defaults = BTreeMap::new();
        for resource in &geometry.resources {
            let relative = unique_resource_path(resource, &mut used_paths);
            let destination = stage.join(&relative);
            let result =
                resolve_resource(resource, source_parent, &destination, config.resource_mode);
            match result {
                Ok(Some(digests)) => {
                    defaults.insert(resource.name.clone(), relative.clone());
                    resolutions.push(ResourceResolution {
                        geometry: geometry.modality.clone(),
                        resource: resource.name.clone(),
                        locator: resource.locator.clone(),
                        status: ResourceResolutionStatus::Resolved,
                        output_path: Some(relative),
                        stored_byte_count: Some(digests.stored_bytes),
                        content_byte_count: Some(digests.content_bytes),
                        content_md5: Some(digests.content_md5),
                        stored_blake3: Some(digests.stored_blake3),
                        content_blake3: Some(digests.content_blake3),
                        message: None,
                    });
                }
                Ok(None) => {
                    let reason =
                        "resource is unavailable under the selected resource policy".to_owned();
                    if config.resource_mode == ResourceMode::Required {
                        return Err(SeqspecImportError::RequiredResource {
                            geometry: geometry.modality.clone(),
                            resource: resource.name.clone(),
                            reason,
                        });
                    }
                    resolutions.push(ResourceResolution {
                        geometry: geometry.modality.clone(),
                        resource: resource.name.clone(),
                        locator: resource.locator.clone(),
                        status: ResourceResolutionStatus::RequiresBinding,
                        output_path: None,
                        stored_byte_count: None,
                        content_byte_count: None,
                        content_md5: None,
                        stored_blake3: None,
                        content_blake3: None,
                        message: Some(reason),
                    });
                }
                Err(reason) => {
                    // A failed copy/download is never published as a usable
                    // default binding in auto mode.
                    let _ = fs::remove_file(&destination);
                    if config.resource_mode == ResourceMode::Required {
                        return Err(SeqspecImportError::RequiredResource {
                            geometry: geometry.modality.clone(),
                            resource: resource.name.clone(),
                            reason,
                        });
                    }
                    resolutions.push(ResourceResolution {
                        geometry: geometry.modality.clone(),
                        resource: resource.name.clone(),
                        locator: resource.locator.clone(),
                        status: ResourceResolutionStatus::RequiresBinding,
                        output_path: None,
                        stored_byte_count: None,
                        content_byte_count: None,
                        content_md5: None,
                        stored_blake3: None,
                        content_blake3: None,
                        message: Some(reason),
                    });
                }
            }
        }
        let rendered = geometry.render_with_resource_defaults(&defaults);
        crate::execute::compile_geom_typed(&rendered).map_err(|error| {
            SeqspecImportError::GeneratedGeometry {
                geometry: geometry.modality.clone(),
                reason: format!("{error:?}"),
            }
        })?;
        let filename = unique_geometry_filename(&geometry_name, &mut used_geometry_filenames);
        write_artifact(&stage.join(&filename), rendered.as_bytes())?;
        emitted_geometries.push(filename);
    }

    if emitted_geometries.is_empty() {
        return Err(SeqspecImportError::NoGeometryGenerated);
    }

    let report = SeqspecImportReport {
        workflow_schema_version: "1.0.0",
        assessment,
        resources: resolutions,
        emitted_geometries,
    };
    let report_bytes = serde_json::to_vec_pretty(&report).expect("report is serializable");
    write_artifact(&stage.join("import-report.json"), &report_bytes)?;
    fs::rename(staging.path(), output_dir).map_err(|source| SeqspecImportError::Finalize {
        path: output_dir.clone(),
        source,
    })?;
    let _published_stage = staging.keep();
    Ok(report)
}

#[derive(Debug)]
struct Digests {
    stored_bytes: u64,
    content_bytes: u64,
    content_md5: String,
    stored_blake3: String,
    content_blake3: String,
}

fn resolve_resource(
    resource: &ResourceRequirement,
    source_parent: &Path,
    destination: &Path,
    mode: ResourceMode,
) -> Result<Option<Digests>, String> {
    let locator = resource.locator.trim();
    let locator_type = resource.locator_type.to_ascii_lowercase();
    let local = !locator.is_empty()
        && !locator.starts_with("http://")
        && !locator.starts_with("https://")
        && locator_type != "http"
        && locator_type != "https"
        && locator_type != "ftp";
    if local {
        let mut candidate = source_parent.join(locator);
        if candidate.is_dir() && !resource.filename.is_empty() {
            candidate = candidate.join(&resource.filename);
        }
        if candidate.is_file() {
            fs::copy(&candidate, destination)
                .map_err(|error| format!("could not copy `{}`: {error}", candidate.display()))?;
            return verify_resource(resource, destination).map(Some);
        }
    }
    let filename_candidate = source_parent.join(&resource.filename);
    if !resource.filename.is_empty() && filename_candidate.is_file() {
        fs::copy(&filename_candidate, destination).map_err(|error| {
            format!("could not copy `{}`: {error}", filename_candidate.display())
        })?;
        return verify_resource(resource, destination).map(Some);
    }
    if mode == ResourceMode::Offline || locator.is_empty() {
        return Ok(None);
    }
    if locator.starts_with("http://") || locator.starts_with("https://") {
        let remote = if locator.ends_with('/') && !resource.filename.is_empty() {
            format!("{locator}{}", resource.filename)
        } else {
            locator.to_owned()
        };
        let mut response = ureq::get(&remote)
            .call()
            .map_err(|error| format!("download failed: {error}"))?;
        let mut reader = response.body_mut().as_reader();
        let mut output = File::create(destination)
            .map_err(|error| format!("could not create download target: {error}"))?;
        io::copy(&mut reader, &mut output)
            .map_err(|error| format!("download write failed: {error}"))?;
        output
            .flush()
            .map_err(|error| format!("download flush failed: {error}"))?;
        return verify_resource(resource, destination).map(Some);
    }
    Ok(None)
}

fn verify_resource(resource: &ResourceRequirement, path: &Path) -> Result<Digests, String> {
    let mut file = File::open(path).map_err(|error| error.to_string())?;
    let mut stored_blake3 = blake3::Hasher::new();
    let mut stored_bytes = 0u64;
    let mut buffer = [0u8; 64 * 1024];
    loop {
        let count = file.read(&mut buffer).map_err(|error| error.to_string())?;
        if count == 0 {
            break;
        }
        stored_blake3.update(&buffer[..count]);
        stored_bytes += count as u64;
    }
    if resource.filesize > 0 && stored_bytes != resource.filesize as u64 {
        return Err(format!(
            "declared stored size is {}, observed {stored_bytes}",
            resource.filesize
        ));
    }

    let mut file = File::open(path).map_err(|error| error.to_string())?;
    let mut magic = [0_u8; 2];
    let magic_count = file.read(&mut magic).map_err(|error| error.to_string())?;
    use std::io::{BufReader, Seek, SeekFrom};
    file.seek(SeekFrom::Start(0))
        .map_err(|error| error.to_string())?;
    let mut content: Box<dyn Read> = if magic_count == 2 && magic == [0x1f, 0x8b] {
        Box::new(flate2::read::MultiGzDecoder::new(file))
    } else {
        Box::new(BufReader::new(file))
    };
    let mut content_md5 = Md5::new();
    let mut content_blake3 = blake3::Hasher::new();
    let mut content_bytes = 0u64;
    loop {
        let count = content
            .read(&mut buffer)
            .map_err(|error| format!("could not decode resource content: {error}"))?;
        if count == 0 {
            break;
        }
        content_md5.update(&buffer[..count]);
        content_blake3.update(&buffer[..count]);
        content_bytes += count as u64;
    }
    let observed_md5 = format!("{:x}", content_md5.finalize());
    let expected_md5 = resource.md5.trim().to_ascii_lowercase();
    if !expected_md5.is_empty() && expected_md5 != observed_md5 {
        return Err(format!(
            "declared MD5 is {expected_md5}, observed {observed_md5}"
        ));
    }
    Ok(Digests {
        stored_bytes,
        content_bytes,
        content_md5: observed_md5,
        stored_blake3: format!("blake3:{}", stored_blake3.finalize().to_hex()),
        content_blake3: format!("blake3:{}", content_blake3.finalize().to_hex()),
    })
}

fn unique_resource_path(resource: &ResourceRequirement, used: &mut BTreeSet<String>) -> String {
    let suffix = Path::new(&resource.filename)
        .extension()
        .and_then(|extension| extension.to_str())
        .map(|extension| format!(".{extension}"))
        .unwrap_or_default();
    let base = format!("resources/{}{}", sanitize_filename(&resource.name), suffix);
    if used.insert(base.clone()) {
        return base;
    }
    for index in 2usize.. {
        let candidate = format!(
            "resources/{}-{index}{suffix}",
            sanitize_filename(&resource.name)
        );
        if used.insert(candidate.clone()) {
            return candidate;
        }
    }
    unreachable!()
}

fn unique_geometry_filename(base: &str, used: &mut BTreeSet<String>) -> String {
    let filename = format!("{base}.geom");
    if used.insert(filename.clone()) {
        return filename;
    }
    for index in 2usize.. {
        let candidate = format!("{base}-{index}.geom");
        if used.insert(candidate.clone()) {
            return candidate;
        }
    }
    unreachable!()
}

fn sanitize_filename(value: &str) -> String {
    let result = value
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || character == '-' || character == '_' {
                character.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect::<String>();
    if result.is_empty() {
        "protocol".to_owned()
    } else {
        result
    }
}

fn write_artifact(path: &Path, bytes: &[u8]) -> Result<(), SeqspecImportError> {
    fs::write(path, bytes).map_err(|source| SeqspecImportError::WriteArtifact {
        path: path.to_owned(),
        source,
    })
}

/// Summarize importer diagnostics without requiring callers to understand the
/// full report schema.
pub fn diagnostics(report: &SeqspecImportReport) -> impl Iterator<Item = &ImportDiagnostic> {
    report.assessment.diagnostics.iter().chain(
        report
            .assessment
            .geometries
            .iter()
            .flat_map(|geometry| &geometry.diagnostics),
    )
}

pub fn generated_count(report: &SeqspecImportReport) -> usize {
    report
        .assessment
        .geometries
        .iter()
        .filter(|geometry| {
            matches!(
                geometry.status,
                ImportStatus::Supported
                    | ImportStatus::SupportedRequiresBinding
                    | ImportStatus::SupportedWithSelection
            )
        })
        .count()
}
