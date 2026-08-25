use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
    process::Command,
};

use clap::{Args, Parser, Subcommand};
use seqproc::execute::compile_geom_typed;
use seqproc_seqspec_import::{assess, ImportOptions, ImportStatus, IMPORTER_VERSION};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Task,
}

#[derive(Subcommand)]
enum Task {
    /// Assess pinned official seqspec corpora and write JSON/CSV/Markdown reports.
    SeqspecCompat(CompatArgs),
}

#[derive(Args)]
struct CompatArgs {
    #[arg(long, default_value = "planning/seqspec-corpora.toml")]
    manifest: PathBuf,
    #[arg(long, default_value = "target/seqspec-corpora")]
    corpus_dir: PathBuf,
    #[arg(long, default_value = "planning/generated/seqspec-compat")]
    out: PathBuf,
    /// Clone/fetch corpora. Without this flag, the command is strictly offline.
    #[arg(long)]
    fetch: bool,
}

#[derive(Deserialize)]
struct Manifest {
    report_version: String,
    corpus: Vec<Corpus>,
}

#[derive(Deserialize)]
struct Corpus {
    name: String,
    repository: String,
    commit: String,
    path: String,
    sparse: Vec<String>,
    #[serde(default)]
    local_path: Option<PathBuf>,
}

#[derive(Debug, Serialize)]
struct Row {
    corpus: String,
    corpus_commit: String,
    specification: String,
    assay_id: String,
    modality: String,
    status: ImportStatus,
    generated_efgdl: bool,
    compiles: bool,
    compile_error: Option<String>,
    lane_count: usize,
    capability_blockers: Vec<String>,
    diagnostic_codes: Vec<String>,
}

#[derive(Serialize)]
struct Report {
    report_version: String,
    importer_version: String,
    manifest_digest: String,
    rows: Vec<Row>,
    status_counts: BTreeMap<String, usize>,
    capability_impact: Vec<CapabilityImpact>,
}

#[derive(Serialize)]
struct CapabilityImpact {
    capability: String,
    blocked_modalities: usize,
    blocked_specs: usize,
}

fn main() {
    let result = match Cli::parse().command {
        Task::SeqspecCompat(args) => compatibility_report(args),
    };
    if let Err(error) = result {
        eprintln!("error: {error}");
        std::process::exit(1);
    }
}

fn compatibility_report(args: CompatArgs) -> Result<(), Box<dyn std::error::Error>> {
    let source = fs::read_to_string(&args.manifest)?;
    let manifest: Manifest = toml::from_str(&source)?;
    let mut rows = Vec::new();
    for corpus in &manifest.corpus {
        let root = materialize_corpus(corpus, &args.corpus_dir, args.fetch)?;
        let scan_root = root.join(&corpus.path);
        let entries = WalkDir::new(&scan_root)
            .follow_links(false)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|entry| {
                let name = entry.file_name().to_string_lossy();
                entry.file_type().is_file() && (name == "spec.yaml" || name.ends_with(".spec.yaml"))
            })
            .collect::<Vec<_>>();
        if entries.is_empty() {
            return Err(format!(
                "corpus `{}` contains no seqspec YAML files under {}",
                corpus.name,
                scan_root.display()
            )
            .into());
        }
        for entry in entries {
            let yaml = fs::read_to_string(entry.path())?;
            let relative = entry
                .path()
                .strip_prefix(&root)
                .unwrap_or(entry.path())
                .display()
                .to_string();
            match assess(&yaml, &relative, &ImportOptions::default()) {
                Ok(assessment) => {
                    if assessment.geometries.is_empty() {
                        rows.push(empty_row(
                            corpus,
                            relative,
                            assessment.assay_id.unwrap_or_default(),
                            assessment
                                .diagnostics
                                .iter()
                                .map(|diagnostic| diagnostic.code.clone())
                                .collect(),
                        ));
                    } else {
                        for geometry in assessment.geometries {
                            let compile_result = (!geometry.efgdl.is_empty())
                                .then(|| compile_geom_typed(&geometry.efgdl));
                            let compiles = compile_result.as_ref().is_some_and(Result::is_ok);
                            let compile_error = compile_result
                                .and_then(Result::err)
                                .map(|error| format!("{error:?}"));
                            let diagnostics = assessment
                                .diagnostics
                                .iter()
                                .chain(&geometry.diagnostics)
                                .collect::<Vec<_>>();
                            rows.push(Row {
                                corpus: corpus.name.clone(),
                                corpus_commit: corpus.commit.clone(),
                                specification: relative.clone(),
                                assay_id: assessment.assay_id.clone().unwrap_or_default(),
                                modality: geometry.modality,
                                status: geometry.status,
                                generated_efgdl: !geometry.efgdl.is_empty(),
                                compiles,
                                compile_error,
                                lane_count: geometry.inputs.len(),
                                capability_blockers: diagnostics
                                    .iter()
                                    .filter_map(|diagnostic| diagnostic.capability.clone())
                                    .collect::<BTreeSet<_>>()
                                    .into_iter()
                                    .collect(),
                                diagnostic_codes: diagnostics
                                    .iter()
                                    .map(|diagnostic| diagnostic.code.clone())
                                    .collect::<BTreeSet<_>>()
                                    .into_iter()
                                    .collect(),
                            });
                        }
                    }
                }
                Err(error) => rows.push(empty_row(
                    corpus,
                    relative,
                    String::new(),
                    vec![format!("yaml_parse:{error}")],
                )),
            }
        }
    }
    rows.sort_by(|left, right| {
        (&left.corpus, &left.specification, &left.modality).cmp(&(
            &right.corpus,
            &right.specification,
            &right.modality,
        ))
    });

    let mut status_counts = BTreeMap::new();
    let mut capability_modalities: BTreeMap<String, usize> = BTreeMap::new();
    let mut capability_specs: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for row in &rows {
        *status_counts
            .entry(format!("{:?}", row.status))
            .or_insert(0) += 1;
        if row.status != ImportStatus::BlockedSeqprocCapability {
            continue;
        }
        for capability in &row.capability_blockers {
            *capability_modalities.entry(capability.clone()).or_insert(0) += 1;
            capability_specs
                .entry(capability.clone())
                .or_default()
                .insert(format!("{}:{}", row.corpus, row.specification));
        }
    }
    let mut capability_impact = capability_modalities
        .into_iter()
        .map(|(capability, blocked_modalities)| CapabilityImpact {
            blocked_specs: capability_specs.get(&capability).map_or(0, BTreeSet::len),
            capability,
            blocked_modalities,
        })
        .collect::<Vec<_>>();
    capability_impact.sort_by(|left, right| {
        right
            .blocked_modalities
            .cmp(&left.blocked_modalities)
            .then_with(|| left.capability.cmp(&right.capability))
    });
    let report = Report {
        report_version: manifest.report_version,
        importer_version: IMPORTER_VERSION.to_owned(),
        manifest_digest: format!("blake3:{}", blake3::hash(source.as_bytes()).to_hex()),
        rows,
        status_counts,
        capability_impact,
    };
    fs::create_dir_all(&args.out)?;
    fs::write(
        args.out.join("report.json"),
        serde_json::to_vec_pretty(&report)?,
    )?;
    fs::write(args.out.join("report.csv"), render_csv(&report))?;
    fs::write(args.out.join("report.md"), render_markdown(&report))?;
    println!(
        "wrote {} assessed modality rows to {}",
        report.rows.len(),
        args.out.display()
    );
    Ok(())
}

fn empty_row(
    corpus: &Corpus,
    specification: String,
    assay_id: String,
    diagnostic_codes: Vec<String>,
) -> Row {
    Row {
        corpus: corpus.name.clone(),
        corpus_commit: corpus.commit.clone(),
        specification,
        assay_id,
        modality: String::new(),
        status: ImportStatus::BlockedSourceInvalid,
        generated_efgdl: false,
        compiles: false,
        compile_error: None,
        lane_count: 0,
        capability_blockers: Vec::new(),
        diagnostic_codes,
    }
}

fn materialize_corpus(
    corpus: &Corpus,
    corpus_dir: &Path,
    fetch: bool,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    if let Some(local) = &corpus.local_path {
        return Ok(local.clone());
    }
    let root = corpus_dir.join(&corpus.name);
    if !root.join(".git").exists() {
        if !fetch {
            return Err(format!(
                "corpus `{}` is absent at {}; rerun with --fetch",
                corpus.name,
                root.display()
            )
            .into());
        }
        fs::create_dir_all(corpus_dir)?;
        checked(
            Command::new("git")
                .args([
                    "clone",
                    "--filter=blob:none",
                    "--no-checkout",
                    &corpus.repository,
                ])
                .arg(&root),
        )?;
    }
    if fetch {
        checked(Command::new("git").arg("-C").arg(&root).args([
            "fetch",
            "origin",
            &corpus.commit,
        ]))?;
    }
    if fetch {
        checked(
            Command::new("git")
                .arg("-C")
                .arg(&root)
                .args(["sparse-checkout", "set", "--no-cone"])
                .args(&corpus.sparse),
        )?;
        checked(Command::new("git").arg("-C").arg(&root).args([
            "checkout",
            "--detach",
            &corpus.commit,
        ]))?;
    }
    let current = checked_output(
        Command::new("git")
            .arg("-C")
            .arg(&root)
            .args(["rev-parse", "HEAD"]),
    )?;
    if current.trim() != corpus.commit {
        return Err(format!(
            "corpus `{}` is at {}, expected {}; rerun with --fetch",
            corpus.name,
            current.trim(),
            corpus.commit
        )
        .into());
    }
    Ok(root)
}

fn checked(command: &mut Command) -> Result<(), Box<dyn std::error::Error>> {
    let status = command.status()?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("command failed with {status}: {command:?}").into())
    }
}

fn checked_output(command: &mut Command) -> Result<String, Box<dyn std::error::Error>> {
    let output = command.output()?;
    if !output.status.success() {
        return Err(format!("command failed with {}: {command:?}", output.status).into());
    }
    Ok(String::from_utf8(output.stdout)?)
}

fn csv_cell(value: &str) -> String {
    format!("\"{}\"", value.replace('"', "\"\""))
}

fn render_csv(report: &Report) -> String {
    let mut output = String::from("corpus,commit,specification,assay_id,modality,status,generated_efgdl,compiles,compile_error,lane_count,capability_blockers,diagnostic_codes\n");
    for row in &report.rows {
        output.push_str(&format!(
            "{},{},{},{},{},{:?},{},{},{},{},{},{}\n",
            csv_cell(&row.corpus),
            csv_cell(&row.corpus_commit),
            csv_cell(&row.specification),
            csv_cell(&row.assay_id),
            csv_cell(&row.modality),
            row.status,
            row.generated_efgdl,
            row.compiles,
            csv_cell(row.compile_error.as_deref().unwrap_or("")),
            row.lane_count,
            csv_cell(&row.capability_blockers.join(";")),
            csv_cell(&row.diagnostic_codes.join(";"))
        ));
    }
    output
}

fn render_markdown(report: &Report) -> String {
    let supported = report
        .rows
        .iter()
        .filter(|row| row.status.generated() && row.compiles)
        .count();
    let mut output = format!(
        "# seqspec compatibility report\n\nAssessed **{}** modality projections; **{}** generated EFGDL 2 that compiles. Counts are modality-level, not marketing claims about whole assays. Capability impact is a conservative lower bound: rows requiring explicit read/modality policy are not guessed or attributed to a later capability.\n\n## Status summary\n\n| Status | Modalities |\n| --- | ---: |\n",
        report.rows.len(), supported
    );
    for (status, count) in &report.status_counts {
        output.push_str(&format!("| `{status}` | {count} |\n"));
    }
    output.push_str(&format!(
        "\nManifest digest: `{}`; importer version: `{}`.\n\n## Corpus provenance\n\n| Corpus | Commit | Modality rows |\n| --- | --- | ---: |\n",
        report.manifest_digest, report.importer_version
    ));
    let mut corpora = BTreeMap::<(&str, &str), usize>::new();
    for row in &report.rows {
        *corpora
            .entry((row.corpus.as_str(), row.corpus_commit.as_str()))
            .or_default() += 1;
    }
    for ((corpus, commit), count) in corpora {
        output.push_str(&format!("| {corpus} | `{commit}` | {count} |\n"));
    }
    output.push_str("\n## Capability impact\n\n| Rank | Missing capability | Blocked modalities | Blocked specs |\n| ---: | --- | ---: | ---: |\n");
    for (index, impact) in report.capability_impact.iter().enumerate() {
        output.push_str(&format!(
            "| {} | `{}` | {} | {} |\n",
            index + 1,
            impact.capability,
            impact.blocked_modalities,
            impact.blocked_specs
        ));
    }
    output.push_str("\n## Protocol matrix\n\n| Corpus | Specification | Modality | Status | Lanes | EFGDL compiles | Required capability/action |\n| --- | --- | --- | --- | ---: | --- | --- |\n");
    for row in &report.rows {
        let action = if row.capability_blockers.is_empty() {
            row.diagnostic_codes.join(", ")
        } else {
            row.capability_blockers.join(", ")
        };
        output.push_str(&format!(
            "| {} | `{}` | {} | `{:?}` | {} | {} | {} |\n",
            row.corpus,
            row.specification,
            row.modality,
            row.status,
            row.lane_count,
            if row.compiles { "yes" } else { "no" },
            action
        ));
    }
    output
}
