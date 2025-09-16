use std::io::Cursor;

use antisequence::graph::*;
use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use seqproc::execute::compile_geom;

fn nuc(i: usize) -> u8 {
    const N: [u8; 4] = [b'A', b'C', b'G', b'T'];
    N[i & 3]
}

fn make_fastq_pair_10x(num_reads: usize) -> (Vec<u8>, Vec<u8>) {
    // 10x geometry: R1 = CB(16) + UMI(10), R2 = cDNA
    let mut r1 = Vec::with_capacity(num_reads * 100);
    let mut r2 = Vec::with_capacity(num_reads * 100);

    for i in 0..num_reads {
        // headers
        r1.extend_from_slice(format!("@r{}\n", i).as_bytes());
        r2.extend_from_slice(format!("@r{}\n", i).as_bytes());

        // R1 sequence: 16bp barcode + 10bp UMI
        for j in 0..16 {
            r1.push(nuc(i + j));
        }
        for j in 0..10 {
            r1.push(nuc(i + 16 + j + 1));
        }
        r1.extend_from_slice(b"\n+\n");
        // qualities
        for _ in 0..(16 + 10) {
            r1.push(b'I');
        }
        r1.push(b'\n');

        // R2 sequence: 60bp cDNA
        for j in 0..60 {
            r2.push(nuc(i + j * 7 + 3));
        }
        r2.extend_from_slice(b"\n+\n");
        for _ in 0..60 {
            r2.push(b'I');
        }
        r2.push(b'\n');
    }

    (r1, r2)
}

fn make_fastq_pair_sci3(num_reads: usize) -> (Vec<u8>, Vec<u8>) {
    // sci-RNA-seq3 geometry from docs:
    // anchor = f[CAGAGC]
    // brc1 = b[9-10]
    // R1: <brc1><anchor>u[8]b[10]
    // R2: r:
    let anchor = b"CAGAGC";
    let mut r1 = Vec::with_capacity(num_reads * 120);
    let mut r2 = Vec::with_capacity(num_reads * 120);

    for i in 0..num_reads {
        r1.extend_from_slice(format!("@r{}\n", i).as_bytes());
        r2.extend_from_slice(format!("@r{}\n", i).as_bytes());

        // brc1: length 9 or 10 alternating
        let bc_len = if i % 2 == 0 { 9 } else { 10 };
        for j in 0..bc_len {
            r1.push(nuc(i + j * 5));
        }
        // anchor
        r1.extend_from_slice(anchor);
        // UMI 8
        for j in 0..8 {
            r1.push(nuc(i + j * 11 + 2));
        }
        // trailing b[10]
        for j in 0..10 {
            r1.push(nuc(i + j * 13 + 1));
        }
        r1.extend_from_slice(b"\n+\n");
        for _ in 0..(bc_len + anchor.len() + 8 + 10) {
            r1.push(b'I');
        }
        r1.push(b'\n');

        // R2 cDNA 80bp
        for j in 0..80 {
            r2.push(nuc(i + j * 9 + 7));
        }
        r2.extend_from_slice(b"\n+\n");
        for _ in 0..80 {
            r2.push(b'I');
        }
        r2.push(b'\n');
    }

    (r1, r2)
}

fn bench_10x(c: &mut Criterion) {
    let geom = "1{b[16]u[10]}2{r:}".to_string();
    let compiled = compile_geom(geom).expect("compile geom");

    let mut group = c.benchmark_group("antisequence_10x_trivial");
    for &n in &[1_000usize, 10_000] {
        group.bench_function(format!("10x_N={}", n), |b| {
            b.iter_batched(
                || {
                    let (r1, r2) = make_fastq_pair_10x(n);
                    let readers = vec![Cursor::new(r1), Cursor::new(r2)];
                    readers
                },
                |readers| {
                    let mut graph = Graph::new();
                    graph.add(InputFastqOp::from_readers(readers).unwrap());
                    compiled.interpret(&mut graph, &Vec::<&str>::new());
                    let sink1 = std::io::sink();
                    let sink2 = std::io::sink();
                    graph.add(OutputFastqOp::from_writers([sink1, sink2]));
                    graph.run_with_threads(1);
                },
                BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

fn bench_sci3(c: &mut Criterion) {
    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile geom");

    let mut group = c.benchmark_group("antisequence_sci_rna_seq3");
    for &n in &[1_000usize, 10_000] {
        group.bench_function(format!("sci3_N={}", n), |b| {
            b.iter_batched(
                || {
                    let (r1, r2) = make_fastq_pair_sci3(n);
                    vec![Cursor::new(r1), Cursor::new(r2)]
                },
                |readers| {
                    let mut graph = Graph::new();
                    graph.add(InputFastqOp::from_readers(readers).unwrap());
                    compiled.interpret(&mut graph, &Vec::<&str>::new());
                    let sink1 = std::io::sink();
                    let sink2 = std::io::sink();
                    graph.add(OutputFastqOp::from_writers([sink1, sink2]));
                    graph.run_with_threads(1);
                },
                BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

fn bench_sci3_tolerant(c: &mut Criterion) {
    let geom = r#"
anchor = f[CAGAGC]
brc1  = norm(b[9-10])
1{<brc1> hamming(<anchor>, 1) u[8] b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile geom");

    let mut group = c.benchmark_group("antisequence_sci_rna_seq3_tolerant");
    for &n in &[1_000usize, 10_000] {
        group.bench_function(format!("sci3_tol_N={}", n), |b| {
            b.iter_batched(
                || {
                    let (r1, r2) = make_fastq_pair_sci3(n);
                    vec![Cursor::new(r1), Cursor::new(r2)]
                },
                |readers| {
                    let mut graph = Graph::new();
                    graph.add(InputFastqOp::from_readers(readers).unwrap());
                    compiled.interpret(&mut graph, &Vec::<&str>::new());
                    let sink1 = std::io::sink();
                    let sink2 = std::io::sink();
                    graph.add(OutputFastqOp::from_writers([sink1, sink2]));
                    graph.run_with_threads(1);
                },
                BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

fn bench_sci3_disk(c: &mut Criterion) {
    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile geom");

    let base = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let r1 = std::env::var("SCI3_R1").unwrap_or_else(|_| {
        base.join("data/sci3/SRR7827206_1_10k.fastq.gz")
            .to_string_lossy()
            .into_owned()
    });
    let r2 = std::env::var("SCI3_R2").unwrap_or_else(|_| {
        base.join("data/sci3/SRR7827206_2_10k.fastq.gz")
            .to_string_lossy()
            .into_owned()
    });

    let mut group = c.benchmark_group("antisequence_sci_rna_seq3_disk");
    group.bench_function("sci3_ENA_10k", |b| {
        b.iter(|| {
            let mut graph = Graph::new();
            graph
                .add(InputFastqOp::from_files([r1.as_str(), r2.as_str()]).unwrap());
            compiled.interpret(&mut graph, &Vec::<&str>::new());
            let sink1 = std::io::sink();
            let sink2 = std::io::sink();
            graph.add(OutputFastqOp::from_writers([sink1, sink2]));
            graph.run_with_threads(1);
        })
    });
    group.finish();
}

fn bench_sci3_disk_multi(c: &mut Criterion) {
    // Same geometry as bench_sci3_disk
    let geom = r#"
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
"#
    .to_string();
    let compiled = compile_geom(geom).expect("compile geom");

    let base_dir = std::env::var("SCI3_DIR").ok().map(std::path::PathBuf::from).unwrap_or_else(|| {
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("data/sci3")
    });

    // Parse runs from env or default to SRR7827206..=SRR7827215
    let runs: Vec<String> = if let Ok(run_list) = std::env::var("SCI3_RUNS") {
        run_list
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
    } else {
        (7827206..=7827215).map(|n| format!("SRR{}", n)).collect()
    };

    // For each run, try to find subset files with common suffixes, then full files as fallback
    let mut pairs: Vec<(String, String, String)> = Vec::new(); // (acc, r1, r2)
    for acc in runs {
        let r1_candidates = [
            base_dir.join(format!("{}_1_10k.fastq.gz", &acc)),
            base_dir.join(format!("{}_1_10000k.fastq.gz", &acc)),
            base_dir.join(format!("{}_1.fastq.gz", &acc)),
        ];
        let r2_candidates = [
            base_dir.join(format!("{}_2_10k.fastq.gz", &acc)),
            base_dir.join(format!("{}_2_10000k.fastq.gz", &acc)),
            base_dir.join(format!("{}_2.fastq.gz", &acc)),
        ];

        let r1 = r1_candidates.iter().find(|p| p.exists()).cloned();
        let r2 = r2_candidates.iter().find(|p| p.exists()).cloned();
        if let (Some(r1), Some(r2)) = (r1, r2) {
            pairs.push((
                acc,
                r1.to_string_lossy().into_owned(),
                r2.to_string_lossy().into_owned(),
            ));
        }
    }

    if pairs.is_empty() {
        // Nothing to benchmark
        return;
    }

    let mut group = c.benchmark_group("antisequence_sci_rna_seq3_disk");

    // Per-run benches
    for (acc, r1, r2) in &pairs {
        let name = format!("sci3_ENA_{}", acc);
        let r1 = r1.clone();
        let r2 = r2.clone();
        group.bench_function(name, |b| {
            b.iter(|| {
                let mut graph = Graph::new();
                graph.add(InputFastqOp::from_files([r1.as_str(), r2.as_str()]).unwrap());
                compiled.interpret(&mut graph, &Vec::<&str>::new());
                let sink1 = std::io::sink();
                let sink2 = std::io::sink();
                graph.add(OutputFastqOp::from_writers([sink1, sink2]));
                graph.run_with_threads(1);
            })
        });
    }

    // Aggregated bench over all present runs
    if pairs.len() > 1 {
        group.bench_function("sci3_ENA_multi", |b| {
            b.iter(|| {
                for (_, r1, r2) in &pairs {
                    let mut graph = Graph::new();
                    graph.add(InputFastqOp::from_files([r1.as_str(), r2.as_str()]).unwrap());
                    compiled.interpret(&mut graph, &Vec::<&str>::new());
                    let sink1 = std::io::sink();
                    let sink2 = std::io::sink();
                    graph.add(OutputFastqOp::from_writers([sink1, sink2]));
                    graph.run_with_threads(1);
                }
            })
        });
    }
    group.finish();
}

criterion_group!(benches, bench_10x, bench_sci3, bench_sci3_tolerant, bench_sci3_disk, bench_sci3_disk_multi);
criterion_main!(benches);
