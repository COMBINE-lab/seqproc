use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use serde_yaml::Value;

fn null_string<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Ok(Option::<String>::deserialize(deserializer)?.unwrap_or_default())
}

fn null_vec<'de, D, T>(deserializer: D) -> Result<Vec<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Ok(Option::<Vec<T>>::deserialize(deserializer)?.unwrap_or_default())
}

fn default_vec<T>() -> Vec<T> {
    Vec::new()
}

fn default_string() -> String {
    String::new()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Assay {
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub seqspec_version: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub assay_id: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub name: String,
    #[serde(default = "default_vec", deserialize_with = "null_vec")]
    pub modalities: Vec<String>,
    #[serde(default = "default_vec", deserialize_with = "null_vec")]
    pub sequence_spec: Vec<ReadSpec>,
    #[serde(default = "default_vec", deserialize_with = "null_vec")]
    pub library_spec: Vec<Region>,

    // Known metadata that does not influence import semantics.
    #[serde(default)]
    pub doi: Option<Value>,
    #[serde(default)]
    pub date: Option<Value>,
    #[serde(default)]
    pub description: Option<Value>,
    #[serde(default)]
    pub lib_struct: Option<Value>,
    #[serde(default)]
    pub sequence_protocol: Option<Value>,
    #[serde(default)]
    pub sequence_kit: Option<Value>,
    #[serde(default)]
    pub library_protocol: Option<Value>,
    #[serde(default)]
    pub library_kit: Option<Value>,
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct ReadSpec {
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub read_id: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub name: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub modality: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub primer_id: String,
    #[serde(default)]
    pub min_len: i64,
    #[serde(default)]
    pub max_len: i64,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub strand: String,
    #[serde(default = "default_vec", deserialize_with = "null_vec")]
    pub files: Vec<SeqspecFile>,
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SeqspecFile {
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub file_id: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub filename: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub filetype: String,
    #[serde(default)]
    pub filesize: i64,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub url: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub urltype: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub md5: String,
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Onlist {
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub file_id: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub filename: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub filetype: String,
    #[serde(default)]
    pub filesize: i64,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub url: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub urltype: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub md5: String,
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Region {
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub region_id: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub region_type: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub name: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub sequence_type: String,
    #[serde(default = "default_string", deserialize_with = "null_string")]
    pub sequence: String,
    #[serde(default)]
    pub min_len: i64,
    #[serde(default)]
    pub max_len: i64,
    #[serde(default)]
    pub onlist: Option<Onlist>,
    #[serde(default = "default_vec", deserialize_with = "null_vec")]
    pub regions: Vec<Region>,
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

impl Region {
    pub fn flattened_with_primer(&self, primer_id: &str, out: &mut Vec<Region>) {
        if self.region_id == primer_id || self.regions.is_empty() {
            out.push(self.clone());
        } else {
            for child in &self.regions {
                child.flattened_with_primer(primer_id, out);
            }
        }
    }

    pub fn find_descendant(&self, region_id: &str) -> Option<&Region> {
        if self.region_id == region_id {
            return Some(self);
        }
        self.regions
            .iter()
            .find_map(|region| region.find_descendant(region_id))
    }
}
