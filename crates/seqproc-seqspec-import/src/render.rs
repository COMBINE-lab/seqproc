use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::{
    model::Onlist,
    projection::{
        onlist_requirement, OnlistPolicy, PatternOrientation, PatternProjection,
        ResourceRequirement,
    },
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) enum ProjectedSequenceKind {
    Fixed { literal: String },
    Random,
    Onlist,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct ProjectedRegion {
    pub source_region_id: String,
    pub region_type: String,
    pub min_len: usize,
    pub max_len: usize,
    pub source_min_len: usize,
    pub kind: ProjectedSequenceKind,
    pub onlist: Option<Onlist>,
    pub orientation: PatternOrientation,
    pub pattern_projection: Option<PatternProjection>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct ProjectedRead {
    pub lane: usize,
    pub read_id: String,
    pub min_len: usize,
    pub max_len: usize,
    pub regions: Vec<ProjectedRegion>,
}

#[derive(Debug, Clone)]
pub(crate) struct GeometryPlan {
    seqspec_version: String,
    assay_id: String,
    assay_name: String,
    modality: String,
    source_digest: String,
    reads: Vec<ProjectedRead>,
    onlist_policy: OnlistPolicy,
    resources: Vec<ResourceRequirement>,
}

impl GeometryPlan {
    pub(crate) fn new(
        seqspec_version: &str,
        assay_id: &str,
        assay_name: &str,
        modality: &str,
        source_digest: &str,
        reads: Vec<ProjectedRead>,
        onlist_policy: OnlistPolicy,
    ) -> Self {
        let mut used = BTreeSet::new();
        let mut resources = Vec::new();
        if onlist_policy == OnlistPolicy::Exact {
            for read in &reads {
                for region in &read.regions {
                    if let (ProjectedSequenceKind::Onlist, Some(onlist)) =
                        (&region.kind, &region.onlist)
                    {
                        let base = format!(
                            "read{}_{}_onlist",
                            read.lane,
                            sanitize_identifier(&region.source_region_id)
                        );
                        let name = unique_identifier(base, &mut used);
                        resources.push(onlist_requirement(name, region, onlist));
                    }
                }
            }
        }
        Self {
            seqspec_version: seqspec_version.to_owned(),
            assay_id: assay_id.to_owned(),
            assay_name: assay_name.to_owned(),
            modality: modality.to_owned(),
            source_digest: source_digest.to_owned(),
            reads,
            onlist_policy,
            resources,
        }
    }

    pub(crate) fn resources(&self) -> Vec<ResourceRequirement> {
        self.resources.clone()
    }
}

pub(crate) fn render_geometry(
    plan: &GeometryPlan,
    resource_defaults: &BTreeMap<String, String>,
) -> String {
    let mut output = String::new();
    output.push_str("header {\n");
    output.push_str("    efgdl = 2,\n");
    output.push_str("    source_format = \"seqspec\",\n");
    output.push_str(&format!(
        "    source_version = \"{}\",\n",
        escape_string(&plan.seqspec_version)
    ));
    output.push_str(&format!(
        "    source_digest = \"{}\",\n",
        escape_string(&plan.source_digest)
    ));
    if !plan.assay_id.is_empty() {
        output.push_str(&format!(
            "    source_assay = \"{}\",\n",
            escape_string(&plan.assay_id)
        ));
    }
    if !plan.assay_name.is_empty() {
        output.push_str(&format!(
            "    source_name = \"{}\",\n",
            escape_string(&plan.assay_name)
        ));
    }
    output.push_str(&format!(
        "    source_modality = \"{}\",\n",
        escape_string(&plan.modality)
    ));
    output.push_str(&format!(
        "    importer = \"seqproc-seqspec-import/{}\",\n",
        env!("CARGO_PKG_VERSION")
    ));
    output.push_str("}\n\n");

    if !plan.resources.is_empty() {
        output.push_str("resources {\n");
        for resource in &plan.resources {
            output.push_str("    ");
            output.push_str(&resource.name);
            if let Some(path) = resource_defaults.get(&resource.name) {
                output.push_str(" = \"");
                output.push_str(&escape_string(path));
                output.push('"');
            }
            output.push_str(",\n");
        }
        output.push_str("}\n\n");
    }

    let mut resource_index = 0usize;
    let mut definitions = Vec::new();
    let mut read_layouts = Vec::new();
    for read in &plan.reads {
        let mut used_labels = BTreeSet::new();
        let mut pieces = Vec::new();
        for region in &read.regions {
            let base_label = format!(
                "read{}_{}",
                read.lane,
                sanitize_identifier(&region.source_region_id)
            );
            let label = unique_identifier(base_label, &mut used_labels);
            let interval_kind = interval_kind(&region.region_type);
            let shape = interval_shape(region.min_len, region.max_len);
            match region.kind {
                ProjectedSequenceKind::Fixed { ref literal, .. } => {
                    pieces.push(format!("f<{label}>[{literal}]"));
                }
                ProjectedSequenceKind::Random => {
                    pieces.push(format!("{interval_kind}<{label}>{shape}"));
                }
                ProjectedSequenceKind::Onlist if plan.onlist_policy == OnlistPolicy::Capture => {
                    pieces.push(format!("{interval_kind}<{label}>{shape}"));
                }
                ProjectedSequenceKind::Onlist => {
                    let resource = &plan.resources[resource_index];
                    resource_index += 1;
                    // Prefix-related variable onlists can otherwise admit
                    // multiple exact candidates. Preserve conservative import
                    // semantics explicitly rather than inheriting the filter
                    // default of accepting an arbitrary equal match.
                    if region.min_len != region.max_len {
                        definitions.push("#[ambig_policy = no_match]".to_owned());
                        definitions.push("#[pattern_boundary = matched]".to_owned());
                    }
                    if resource.orientation == PatternOrientation::ReverseComplement {
                        definitions.push("#[pattern_orientation = rc]".to_owned());
                    }
                    if let Some(projection) = resource.projection {
                        match projection {
                            PatternProjection::Prefix { max_len } => definitions.push(format!(
                                "#[pattern_projection = prefix(max_len = {max_len})]"
                            )),
                            PatternProjection::Suffix { max_len } => definitions.push(format!(
                                "#[pattern_projection = suffix(max_len = {max_len})]"
                            )),
                        }
                    }
                    definitions.push(format!(
                        "{label} = filter({interval_kind}{shape}, ${})",
                        resource.name
                    ));
                    pieces.push(format!("<{label}>"));
                }
            }
        }
        read_layouts.push(format!(
            "#[read_len({}, {})]\n{}{{{}}}",
            read.min_len,
            read.max_len,
            read.lane,
            pieces.join("")
        ));
    }
    if !definitions.is_empty() {
        output.push_str(&definitions.join("\n"));
        output.push_str("\n\n");
    }
    output.push_str(&read_layouts.join("\n"));
    output.push('\n');
    output
}

fn interval_kind(region_type: &str) -> &'static str {
    match region_type.to_ascii_lowercase().as_str() {
        "barcode" => "b",
        "umi" => "u",
        "index5" | "index7" => "s",
        _ => "r",
    }
}

fn interval_shape(min_len: usize, max_len: usize) -> String {
    if min_len == max_len {
        format!("[{max_len}]")
    } else {
        format!("[{min_len}-{max_len}]")
    }
}

fn sanitize_identifier(source: &str) -> String {
    let mut result = String::with_capacity(source.len().max(1));
    for (index, character) in source.chars().enumerate() {
        let valid = character == '_' || character.is_ascii_alphanumeric();
        let character = if valid { character } else { '_' };
        if index == 0 && character.is_ascii_digit() {
            result.push_str("r_");
        }
        result.push(character.to_ascii_lowercase());
    }
    if result.is_empty() {
        result.push_str("region");
    }
    result
}

fn unique_identifier(base: String, used: &mut BTreeSet<String>) -> String {
    if used.insert(base.clone()) {
        return base;
    }
    for suffix in 2usize.. {
        let candidate = format!("{base}_{suffix}");
        if used.insert(candidate.clone()) {
            return candidate;
        }
    }
    unreachable!()
}

fn escape_string(value: &str) -> String {
    value.replace('\\', "\\\\").replace('"', "\\\"")
}
