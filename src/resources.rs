//! Runtime resolution for EFGDL resource declarations and references.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
};

use serde::Serialize;
use thiserror::Error;

use crate::{
    compile::{functions::CompiledFunction, CompiledData},
    parser::ResourceRef,
    S,
};

#[derive(Debug, Clone, Default)]
pub struct ResourceBindings {
    named: BTreeMap<String, PathBuf>,
}

impl ResourceBindings {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(
        &mut self,
        name: impl Into<String>,
        path: impl Into<PathBuf>,
    ) -> Result<(), ResourceError> {
        let name = name.into();
        if !valid_identifier(&name) {
            return Err(ResourceError::MalformedBinding { name });
        }
        if self.named.insert(name.clone(), path.into()).is_some() {
            return Err(ResourceError::DuplicateBinding { name });
        }
        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &Path)> {
        self.named
            .iter()
            .map(|(name, path)| (name.as_str(), path.as_path()))
    }
}

fn valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    matches!(chars.next(), Some(first) if first == '_' || first.is_ascii_alphabetic())
        && chars.all(|character| character == '_' || character.is_ascii_alphanumeric())
}

#[derive(Debug, Error)]
pub enum ResourceError {
    #[error("resource binding `{name}` is malformed; expected NAME=PATH with an identifier name")]
    MalformedBinding { name: String },
    #[error("resource `{name}` was bound more than once")]
    DuplicateBinding { name: String },
    #[error("resource `{name}` was supplied but is not declared by this geometry")]
    UnknownBinding { name: String },
    #[error("required resource `{name}` has no --bind value or declared default")]
    MissingNamed { name: String },
    #[error("geometry references positional resource ${index}, but only {supplied} --additional values were supplied")]
    MissingPositional { index: usize, supplied: usize },
    #[error("relative resource path `{path}` requires an explicit geometry base directory")]
    MissingBase { path: PathBuf },
    #[error("could not read resource `{reference}` at `{path}`: {source}")]
    Unreadable {
        reference: String,
        path: PathBuf,
        #[source]
        source: io::Error,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct ResolvedResourceReport {
    pub reference: String,
    pub path: PathBuf,
    pub source: ResourceBindingSource,
    pub content_digest: String,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ResourceBindingSource {
    Literal,
    Positional,
    Named,
    Default,
}

#[derive(Debug, Clone, Default, Serialize, PartialEq, Eq)]
pub struct ResourceResolutionReport {
    pub resolved: Vec<ResolvedResourceReport>,
    pub unused_declarations: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct ResolvedResources {
    paths: BTreeMap<ResourceRef, PathBuf>,
    report: ResourceResolutionReport,
}

impl ResolvedResources {
    pub(crate) fn path(&self, reference: &ResourceRef) -> &Path {
        self.paths
            .get(reference)
            .expect("validated resource reference must be resolved")
    }

    pub(crate) fn report(&self) -> &ResourceResolutionReport {
        &self.report
    }
}

fn visit_function(function: &CompiledFunction, used: &mut BTreeSet<ResourceRef>) {
    let fallback = match function {
        CompiledFunction::Map(reference, fallback)
        | CompiledFunction::MapWithMismatch(reference, fallback, _)
        | CompiledFunction::MapWithEdit(reference, fallback, _) => {
            used.insert(reference.clone());
            Some(fallback)
        }
        CompiledFunction::FilterWithinDist(reference, _)
        | CompiledFunction::AnchorSet(reference) => {
            used.insert(reference.clone());
            None
        }
        CompiledFunction::PatternOrientation(_)
        | CompiledFunction::PatternProjection(_)
        | CompiledFunction::PatternBoundaryMatched => None,
        _ => None,
    };
    if let Some(fallback) = fallback {
        for S(function, _) in fallback {
            visit_function(function, used);
        }
    }
}

fn digest_file(reference: &ResourceRef, path: &Path) -> Result<String, ResourceError> {
    let mut file = File::open(path).map_err(|source| ResourceError::Unreadable {
        reference: reference.to_string(),
        path: path.to_owned(),
        source,
    })?;
    let mut hasher = blake3::Hasher::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let count = file
            .read(&mut buffer)
            .map_err(|source| ResourceError::Unreadable {
                reference: reference.to_string(),
                path: path.to_owned(),
                source,
            })?;
        if count == 0 {
            break;
        }
        hasher.update(&buffer[..count]);
    }
    Ok(format!("blake3:{}", hasher.finalize().to_hex()))
}

fn resolve_relative(
    path: &Path,
    base: Option<&Path>,
    require_base: bool,
) -> Result<PathBuf, ResourceError> {
    if path.is_absolute() {
        return Ok(path.to_owned());
    }
    match base {
        Some(base) => Ok(base.join(path)),
        None if require_base => Err(ResourceError::MissingBase {
            path: path.to_owned(),
        }),
        None => Ok(path.to_owned()),
    }
}

impl CompiledData {
    pub(crate) fn resolve_resources(
        &self,
        positional: &[String],
        bindings: &ResourceBindings,
        geometry_base: Option<&Path>,
    ) -> Result<ResolvedResources, ResourceError> {
        let declarations = self
            .resource_declarations
            .iter()
            .map(|declaration| (declaration.name.0.as_str(), declaration))
            .collect::<BTreeMap<_, _>>();
        for (name, _) in bindings.iter() {
            if !declarations.contains_key(name) {
                return Err(ResourceError::UnknownBinding {
                    name: name.to_owned(),
                });
            }
        }

        let mut used = BTreeSet::new();
        for meta in self.geometry.iter().flatten() {
            for S(function, _) in &meta.stack {
                visit_function(function, &mut used);
            }
        }

        let mut paths = BTreeMap::new();
        let mut report = ResourceResolutionReport::default();
        for reference in used {
            let (path, source) = match &reference {
                ResourceRef::Literal(path) => (
                    resolve_relative(Path::new(path), geometry_base, self.efgdl_version >= 2)?,
                    ResourceBindingSource::Literal,
                ),
                ResourceRef::Positional(index) => (
                    positional.get(*index).map(PathBuf::from).ok_or(
                        ResourceError::MissingPositional {
                            index: *index,
                            supplied: positional.len(),
                        },
                    )?,
                    ResourceBindingSource::Positional,
                ),
                ResourceRef::Named(name) => {
                    let declaration = declarations
                        .get(name.as_str())
                        .expect("named references are checked during geometry compilation");
                    if let Some((_, path)) = bindings.iter().find(|(bound, _)| *bound == name) {
                        (path.to_owned(), ResourceBindingSource::Named)
                    } else if let Some(default) = &declaration.default {
                        (
                            resolve_relative(Path::new(&default.0), geometry_base, true)?,
                            ResourceBindingSource::Default,
                        )
                    } else {
                        return Err(ResourceError::MissingNamed { name: name.clone() });
                    }
                }
            };
            let content_digest = digest_file(&reference, &path)?;
            report.resolved.push(ResolvedResourceReport {
                reference: reference.to_string(),
                path: path.clone(),
                source,
                content_digest,
            });
            paths.insert(reference, path);
        }
        let used_names = paths
            .keys()
            .filter_map(|reference| match reference {
                ResourceRef::Named(name) => Some(name.as_str()),
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        report.unused_declarations = declarations
            .keys()
            .filter(|name| !used_names.contains(**name))
            .map(|name| (*name).to_owned())
            .collect();
        Ok(ResolvedResources { paths, report })
    }
}
