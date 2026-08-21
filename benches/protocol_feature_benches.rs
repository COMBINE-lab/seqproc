use std::{io::Cursor, time::Duration};

use antisequence::graph::{Graph, InputFastqOp, NullOutputOp};
use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use seqproc::{compile::CompiledData, execute::compile_geom};

const V2: &str = "header { efgdl = 2 }\n";

fn reads() -> usize {
    std::env::var("SEQPROC_FEATURE_BENCH_READS")
        .ok()
        .and_then(|value| value.parse().ok())
        .filter(|value| *value > 0)
        .unwrap_or(20_000)
}

fn fastq_with_prefix(count: usize, prefix: &[u8]) -> Vec<u8> {
    let mut fastq = Vec::with_capacity(count * (prefix.len() * 2 + 24));
    for index in 0..count {
        fastq.extend_from_slice(format!("@r{index}\n").as_bytes());
        fastq.extend_from_slice(prefix);
        fastq.extend_from_slice(b"ACGTACGT\n+\n");
        fastq.extend(std::iter::repeat_n(b'I', prefix.len() + 8));
        fastq.push(b'\n');
    }
    fastq
}

fn bench_compiled(
    group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    compiled: &CompiledData,
    fastq: &[u8],
) {
    group.bench_function(name, |benchmark| {
        benchmark.iter_batched(
            || Cursor::new(fastq.to_vec()),
            |reader| {
                let mut graph = Graph::<antisequence::trace::NoTrace>::new();
                graph.add(InputFastqOp::from_reader(reader).unwrap());
                compiled.interpret(&mut graph, &[]);
                graph.add(NullOutputOp::new());
                graph.try_run_with_threads(1).unwrap();
            },
            BatchSize::LargeInput,
        );
    });
}

fn bench_layout_choice_paths(c: &mut Criterion) {
    let n = reads();
    let compiled = compile_geom(format!("{V2}1{{(f[AAA] | f[CCC] | f[GGG])r:}}")).unwrap();
    let mut group = c.benchmark_group("layout_choice_paths");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(5));
    bench_compiled(
        &mut group,
        "successful_first",
        &compiled,
        &fastq_with_prefix(n, b"AAA"),
    );
    bench_compiled(
        &mut group,
        "late_fallback",
        &compiled,
        &fastq_with_prefix(n, b"GGG"),
    );
    bench_compiled(
        &mut group,
        "all_rejected",
        &compiled,
        &fastq_with_prefix(n, b"TTT"),
    );
    group.finish();
}

fn bench_legacy_neutrality(c: &mut Criterion) {
    let n = reads();
    let legacy = compile_geom("1{b<bc>[8]u<umi>[6]r:}".to_string()).unwrap();
    let v2 = compile_geom(format!("{V2}1{{b<bc>[8]u<umi>[6]r:}}")).unwrap();
    let fastq = fastq_with_prefix(n, b"ACGTACGTACGTAC");
    let mut group = c.benchmark_group("legacy_feature_neutrality");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(5));
    bench_compiled(&mut group, "headerless_efgdl1", &legacy, &fastq);
    bench_compiled(&mut group, "v2_no_new_features", &v2, &fastq);
    group.finish();
}

fn bench_indexed_capture_lowering(c: &mut Criterion) {
    let n = reads();
    let manual = compile_geom(format!(
        "{V2}1{{b<round1>[8]b<round2>[8]r:}} -> 1{{<round1><round2>}}"
    ))
    .unwrap();
    let indexed = compile_geom(format!(
        "{V2}1{{(b<round>[8])*2r:}} -> 1{{<round[1]><round[2]>}}"
    ))
    .unwrap();
    let fastq = fastq_with_prefix(n, b"ACGTACGTTGCATGCA");
    let mut group = c.benchmark_group("indexed_capture_lowering");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(5));
    bench_compiled(&mut group, "manually_expanded", &manual, &fastq);
    bench_compiled(&mut group, "indexed_repeat", &indexed, &fastq);
    group.finish();
}

fn write_anchor_set(count: usize) -> tempfile::NamedTempFile {
    use std::io::Write;

    let mut file = tempfile::NamedTempFile::new().unwrap();
    for mut value in 0..count {
        let mut anchor = [b'A'; 8];
        for base in anchor.iter_mut().rev() {
            *base = b"ACGT"[value & 3];
            value >>= 2;
        }
        writeln!(file, "{}", std::str::from_utf8(&anchor).unwrap()).unwrap();
    }
    file
}

fn bench_anchor_set_scale(c: &mut Criterion) {
    let n = reads();
    let geometry = format!(
        "{V2}#[search(relative)]\n#[anchor_set($0)]\nanchor = f[AAAAAAAA]\n1{{<anchor>r:}}"
    );
    let compiled = compile_geom(geometry).unwrap();
    let fastq = fastq_with_prefix(n, b"AAAAAAAA");
    let mut group = c.benchmark_group("anchor_set_scale");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(5));
    for count in [8, 1024] {
        let anchors = write_anchor_set(count);
        let path = anchors.path().to_string_lossy().into_owned();
        group.bench_function(format!("anchors_{count}"), |benchmark| {
            benchmark.iter_batched(
                || {
                    let mut graph = Graph::new();
                    graph.add(InputFastqOp::from_reader(Cursor::new(fastq.clone())).unwrap());
                    compiled.interpret(&mut graph, &[path.as_str()]);
                    graph.add(NullOutputOp::new());
                    graph
                },
                |graph| graph.try_run_with_threads(1).unwrap(),
                BatchSize::LargeInput,
            );
        });
    }
    group.finish();
}

fn paired_fastq(count: usize) -> (Vec<u8>, Vec<u8>, Vec<u8>) {
    let mut read1 = Vec::with_capacity(count * 48);
    let mut read2 = Vec::with_capacity(count * 48);
    let mut interleaved = Vec::with_capacity(count * 96);
    for index in 0..count {
        let first = format!("@r{index}/1\nACGTACGT\n+\nIIIIIIII\n");
        let second = format!("@r{index}/2\nTGCATGCA\n+\nJJJJJJJJ\n");
        read1.extend_from_slice(first.as_bytes());
        read2.extend_from_slice(second.as_bytes());
        interleaved.extend_from_slice(first.as_bytes());
        interleaved.extend_from_slice(second.as_bytes());
    }
    (read1, read2, interleaved)
}

fn bench_interleaved_input(c: &mut Criterion) {
    let (read1, read2, interleaved) = paired_fastq(reads());
    let mut group = c.benchmark_group("interleaved_input");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(5));
    group.bench_function("separate_two_lane", |benchmark| {
        benchmark.iter_batched(
            || {
                let mut graph = Graph::<antisequence::trace::NoTrace>::new();
                graph.add(
                    InputFastqOp::from_readers([
                        Cursor::new(read1.clone()),
                        Cursor::new(read2.clone()),
                    ])
                    .unwrap(),
                );
                graph
            },
            |mut graph| {
                graph.add(NullOutputOp::new());
                graph.try_run_with_threads(1).unwrap();
            },
            BatchSize::LargeInput,
        );
    });
    group.bench_function("interleaved_two_lane", |benchmark| {
        benchmark.iter_batched(
            || {
                let mut graph = Graph::<antisequence::trace::NoTrace>::new();
                graph.add(
                    InputFastqOp::from_interleaved_reader(Cursor::new(interleaved.clone()), 2)
                        .unwrap(),
                );
                graph
            },
            |mut graph| {
                graph.add(NullOutputOp::new());
                graph.try_run_with_threads(1).unwrap();
            },
            BatchSize::LargeInput,
        );
    });
    group.finish();
}

fn bench_bounded_input_arity(c: &mut Criterion) {
    let n = reads();
    let lanes = (0..3)
        .map(|lane| fastq_with_prefix(n, b"ACGTACGT".get(..lane + 4).unwrap()))
        .collect::<Vec<_>>();
    let mut group = c.benchmark_group("bounded_input_arity");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(5));
    for arity in 1..=3 {
        group.bench_function(format!("lanes_{arity}"), |benchmark| {
            benchmark.iter_batched(
                || {
                    let readers = lanes[..arity]
                        .iter()
                        .cloned()
                        .map(Cursor::new)
                        .collect::<Vec<_>>();
                    let mut graph = Graph::<antisequence::trace::NoTrace>::new();
                    graph.add(InputFastqOp::from_readers(readers).unwrap());
                    graph.add(NullOutputOp::new());
                    graph
                },
                |graph| graph.try_run_with_threads(1).unwrap(),
                BatchSize::LargeInput,
            );
        });
    }
    group.finish();
}

criterion_group!(
    feature_benches,
    bench_layout_choice_paths,
    bench_legacy_neutrality,
    bench_indexed_capture_lowering,
    bench_anchor_set_scale,
    bench_interleaved_input,
    bench_bounded_input_arity,
);
criterion_main!(feature_benches);
