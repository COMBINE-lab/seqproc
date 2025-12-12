# Seqproc: Optimization and Design Notes

This document provides detailed writeups on seqproc performance optimizations, language refinements, demuxing, statistics collection, and user-space tooling. It is designed so a graduate student can pick up where this work left off.

---

## Table of Contents

1. [Suggestions on General Refinements to the Seqproc Language](#1-suggestions-on-general-refinements-to-the-seqproc-language)
2. [Demuxing State: Correctness, Ease of Use, Future Testing](#2-demuxing-state-correctness-ease-of-use-future-testing)
3. [Statistics Collection / Aggregation System](#3-statistics-collection--aggregation-system)
4. [User-Space Tools](#4-user-space-tools)
5. [Detailed Optimization Writeups](#5-detailed-optimization-writeups)
6. [Pending Items for Future Work](#6-pending-items-for-future-work)

---

## 1. Suggestions on General Refinements to the Seqproc Language

The seqproc language (EFGDL - Extended Fragment Geometry Description Language) is a domain-specific language for describing sequencing read geometries and transformations.

### Current Language Features

The language supports:

- **Interval types**: `b` (barcode), `u` (UMI), `f` (fixed sequence), `r` (read sequence), `x` (discard)
- **Length specifiers**: `[N]` (fixed), `[N-M]` (range), `:` (unbounded)
- **Labels**: `<label>` for referencing defined intervals
- **Transformations**: `rev()`, `revcomp()`, `trunc()`, `pad()`, `norm()`, `map()`, `hamming()`, etc.

### Suggested Refinements

#### 1.1 Improved Error Messages with Source Location

**Current issue**: Parser errors can be cryptic, especially for complex nested transformations.

**Suggestion**: Enhance the `chumsky` parser error recovery to provide:
- Line and column numbers in error messages
- Contextual hints about what was expected
- "Did you mean?" suggestions for common typos

**Implementation path**:
```rust
// In src/geometry/parser.rs, enhance error handling:
.map_err_with_span(|t, span| {
    throw(t, Simple::custom(span, format!(
        "Expected '{}' at line {}:{}. Did you mean '{}'?",
        expected, line, col, suggestion
    )))
})
```

#### 1.2 Optional/Conditional Intervals

**Current limitation**: All defined intervals must be present in every read.

**Suggestion**: Add optional interval syntax, e.g., `b?[10]` or `optional(<label>)`:
```
# Proposed syntax
bc = b?[10-12]  # Barcode may or may not be present
1{<bc>u[8]r:}2{r:}
```

**Use case**: Protocols where some reads have variable structure.

#### 1.3 Inline Comments

**Current limitation**: No support for inline comments in geometry files.

**Suggestion**: Add `#` or `//` style comments:
```
# This is the cell barcode
bc = b[16]
1{<bc>u[10]}2{r:}  # UMI is 10bp
```

**Implementation**: Add `Token::Comment` to lexer and skip in parser.

#### 1.4 Named Transformation Pipelines

**Current limitation**: Complex transformation chains are repeated.

**Suggestion**: Allow defining reusable transformation pipelines:
```
pipeline normalize_bc = norm(pad_to(self, 16, A))

bc = b[14-16]
1{<bc>}2{r:}
-> 1{normalize_bc(<bc>)}2{<r>}
```

#### 1.5 Validation Mode / Dry-Run

**Current limitation**: Geometry errors only appear at runtime.

**Suggestion**: Add `--validate` flag that checks:
- All labels are defined before use
- Interval lengths are consistent
- Transformation chains are valid
- Whitelist/map files exist and are readable

#### 1.6 Better `self` Semantics in `map()`

**Current issue**: The `self` keyword inside `map()` expressions can be confusing.

**Suggestion**: Document more clearly or allow explicit naming:
```
map(<bc>, whitelist.tsv, pad_to(input, 16, A))  # 'input' instead of 'self'
```

---

## 2. Demuxing State: Correctness, Ease of Use, Future Testing

### Current Implementation Status

Demultiplexing is implemented in `src/demux.rs` using ANTISEQUENCE's `LookupOp`:

```rust
pub struct DemuxConfig {
    pub sample_map_path: PathBuf,    // TSV: barcode → sample name
    pub barcode_label: String,        // e.g., "seq2.bc1"
    pub sample_attr: String,          // default: "sample"
    pub output_dir: PathBuf,
    pub unassigned_name: String,
}
```

**Workflow**:
1. Load barcode→sample TSV mapping
2. Add `LookupOp` to graph after geometry interpretation
3. Use `OutputFastqFileOp` with expression-based routing: `{output_dir}/{sample}_R1.fastq`

### Correctness Concerns

#### 2.1 Attribute Type Mismatches

**Issue discovered during optimization**: `MatchAnyOp` and downstream operations (`SetOp`, `SelectOp`) have conflicting expectations for attribute types (`Bool` vs `Bytes`).

**Symptoms**:
- Panics like `"Expected bytes, but found [bool false]"`
- Panics like `"Expected bool, but found [bytes ...]"`

**Root cause**: When a pattern doesn't match in `MatchAnyOp`:
- `SetOp` expects `sub` to be `Bool` (for conditional logic)
- `SetOp` expects `mapped` to be `Bytes` (for the replacement value)
- Different code paths set these inconsistently

**Current workaround**: Modified `expect_bool` in `expr_node.rs` to handle `Bytes` data by checking `!v.is_empty()`.

**Recommended fix**:
1. Audit all `MatchAnyOp` attribute settings
2. Ensure `sub` is always `Bool`
3. Ensure `mapped`/`pattern_name` is always `Bytes`
4. Add type assertions in debug builds

#### 2.2 Unassigned Read Handling

**Current behavior**: Reads that don't match any barcode get `sample = "unassigned"`.

**Potential issues**:
- No count tracking for unassigned reads
- Large unassigned files if whitelist is incomplete
- No option to discard unassigned reads

**Suggestions**:
- Add `--discard-unassigned` flag
- Add unassigned count to `DemuxStats`
- Warn if unassigned rate > threshold

### Ease of Use Improvements

#### 2.3 Simplified CLI

**Current**:
```bash
seqproc --geom geo.fgdl -1 R1.fq -2 R2.fq \
  --demux-map barcodes.tsv --demux-label seq2.bc1 \
  --demux-out-dir demux_out
```

**Suggested improvements**:
- Auto-detect barcode label from geometry if only one barcode defined
- Support gzipped barcode maps
- Add `--demux-format` for CSV/TSV/JSON input

#### 2.4 Demux Statistics

Add comprehensive demux stats output:
```json
{
  "total_reads": 1000000,
  "assigned_reads": 950000,
  "unassigned_reads": 50000,
  "samples": {
    "sample_A": 480000,
    "sample_B": 470000
  },
  "barcodes_not_in_map": ["NNNNNNNN", ...]
}
```

### Future Testing

#### 2.5 Test Cases to Add

1. **Exact match demux**: All barcodes in whitelist
2. **Partial match demux**: Some barcodes missing from whitelist
3. **Hamming distance demux**: Barcodes with 1-2 mismatches
4. **Large-scale demux**: 10M+ reads, 1000+ samples
5. **Edge cases**: Empty barcodes, N-containing barcodes, duplicate sample names

#### 2.6 Regression Test Template

```rust
#[test]
fn test_demux_correctness() {
    // 1. Create synthetic FASTQs with known barcodes
    // 2. Create barcode→sample map
    // 3. Run seqproc with demux
    // 4. Verify each output file contains correct barcodes
    // 5. Verify read counts match expected
}
```

### Why Demux May Be Slower Than Other Tools

Potential performance bottlenecks compared to dedicated demuxers:

1. **Per-read file path evaluation**: Even with constant path optimization, demux requires dynamic path construction
2. **Hash map lookups**: `LookupOp` uses `FxHashMap` but still has per-read lookup cost
3. **File handle management**: Many output files = many open handles + buffer management
4. **No SIMD barcode matching**: Unlike specialized tools that use SIMD for Hamming distance

**Optimization opportunities**:
- Batch writes per sample before flushing
- Pre-sort reads by sample, then write in bulk
- Use memory-mapped I/O for output files

---

## 3. Statistics Collection / Aggregation System

### Current State

Basic stats collection exists via `SeqprocStats` when `--summary` is provided:

```rust
pub struct SeqprocStats {
    pub call: Option<String>,
    // ... additional fields
}
```

### Proposed System (Mirroring Matchbox)

#### 3.1 Statistics Categories

**Per-read statistics**:
- Read count (total, passed, failed)
- Base quality distribution
- Read length distribution
- N-content

**Per-barcode statistics**:
- Barcode match rate (exact, 1-mismatch, 2-mismatch)
- Barcode quality scores
- Collision rate (ambiguous matches)

**Per-sample statistics** (for demux):
- Read count per sample
- Base composition per sample
- Duplication rate estimates

**Transformation statistics**:
- Padding/truncation counts
- Filter pass/fail rates
- Map hit/miss rates

#### 3.2 Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Graph Execution                       │
│  ┌──────────┐   ┌──────────┐   ┌──────────┐            │
│  │ InputOp  │ → │ MatchOp  │ → │ OutputOp │            │
│  └────┬─────┘   └────┬─────┘   └────┬─────┘            │
│       │              │              │                   │
│       ▼              ▼              ▼                   │
│  ┌─────────────────────────────────────────────────┐   │
│  │              StatsCollector (TLS)                │   │
│  │  - per-thread counters                          │   │
│  │  - histograms                                   │   │
│  │  - sample maps                                  │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
                           │
                           ▼
                    ┌──────────────┐
                    │ StatsAggregator │
                    │  - merge TLS   │
                    │  - compute     │
                    └──────────────┘
                           │
                           ▼
                    ┌──────────────┐
                    │  JSON/TSV    │
                    │  Output      │
                    └──────────────┘
```

#### 3.3 Implementation Sketch

```rust
// Thread-local stats accumulator
thread_local! {
    static STATS_TLS: RefCell<ThreadStats> = RefCell::new(ThreadStats::new());
}

pub struct ThreadStats {
    pub read_count: u64,
    pub base_count: u64,
    pub quality_histogram: [u64; 256],
    pub length_histogram: HashMap<usize, u64>,
    pub barcode_matches: HashMap<Vec<u8>, u64>,
}

impl ThreadStats {
    pub fn record_read(&mut self, read: &Read) {
        self.read_count += 1;
        // ... accumulate stats
    }
}

// Global aggregator
pub struct StatsAggregator {
    pub merged: ThreadStats,
}

impl StatsAggregator {
    pub fn merge_thread_stats(&mut self, ts: ThreadStats) {
        self.merged.read_count += ts.read_count;
        // ... merge histograms, maps
    }
    
    pub fn finalize(&self) -> SeqprocStats {
        // Compute derived statistics
    }
}
```

#### 3.4 Output Formats

**JSON** (default):
```json
{
  "version": "1.0",
  "timestamp": "2024-01-15T10:30:00Z",
  "input": {
    "file1": "R1.fastq.gz",
    "file2": "R2.fastq.gz",
    "total_reads": 10000000
  },
  "quality": {
    "mean_q30_rate": 0.95,
    "q30_histogram": [...]
  },
  "barcodes": {
    "exact_match_rate": 0.92,
    "corrected_rate": 0.05,
    "unmatched_rate": 0.03
  }
}
```

**TSV** (for pipeline integration):
```
metric	value
total_reads	10000000
q30_rate	0.95
barcode_match_rate	0.97
```

---

## 4. User-Space Tools

### 4.1 Seqproc Geometry Visualizer (Web App)

**Purpose**: Help users create and validate geometry files interactively.

**Features**:
- Visual representation of read structure
- Drag-and-drop interval editor
- Real-time validation
- Preview with sample reads
- Export to .fgdl format

**Architecture**:
```
┌─────────────────────────────────────────┐
│           React Frontend                 │
│  ┌─────────────┐  ┌─────────────┐       │
│  │ GeomEditor  │  │ ReadPreview │       │
│  └─────────────┘  └─────────────┘       │
│           │              │               │
│           ▼              ▼               │
│  ┌─────────────────────────────────┐    │
│  │     seqproc-wasm (Rust→WASM)    │    │
│  │  - parse geometry               │    │
│  │  - validate                     │    │
│  │  - simulate read processing     │    │
│  └─────────────────────────────────┘    │
└─────────────────────────────────────────┘
```

**Implementation steps**:
1. Compile seqproc parser to WASM using `wasm-pack`
2. Create React UI with interval visualization
3. Add sample FASTQ preview
4. Deploy as static site (GitHub Pages / Netlify)

### 4.2 Head-of-Reads Visualizer

**Purpose**: Show first N reads with geometry overlaid.

**Features**:
- Color-coded intervals
- Quality score heatmap
- Barcode match highlighting
- Export as SVG/PNG

**CLI**:
```bash
seqproc-viz --geom 10x.fgdl -1 R1.fq -2 R2.fq --head 100 --output reads.html
```

**Output**: Interactive HTML with:
```
Read 1:
  R1: [ATCGATCG][NNNNNNNN][ATCG...
      ├──BC──┤ ├──UMI──┤ ├─cDNA...
  R2: [ATCGATCGATCGATCG...
      ├────────cDNA────────...
```

### 4.3 Online Repository of Seqproc Scripts

**Purpose**: Community-maintained collection of geometry files for common protocols.

**Structure**:
```
seqproc-protocols/
├── 10x-genomics/
│   ├── 3prime-v3.fgdl
│   ├── 5prime-v2.fgdl
│   └── README.md
├── illumina/
│   ├── nextera.fgdl
│   └── truseq.fgdl
├── smart-seq/
│   └── smart-seq3.fgdl
├── sci-rna-seq/
│   ├── sci-rna-seq3.fgdl
│   └── sci-rna-seq3-tolerant.fgdl
└── index.json  # Machine-readable index
```

**Features**:
- GitHub repository with PR-based submissions
- Validation CI (geometry must parse + pass tests)
- CLI integration: `seqproc --protocol 10x-3prime-v3`
- Version tracking per protocol

**CLI integration**:
```rust
// In bin.rs, add protocol lookup:
if let Some(protocol) = args.protocol {
    let url = format!(
        "https://raw.githubusercontent.com/combine-lab/seqproc-protocols/main/{}.fgdl",
        protocol
    );
    // Fetch and cache geometry
}
```

### 4.4 Seqproc Playground (Interactive REPL)

**Purpose**: Test geometry transformations interactively.

**Features**:
```
seqproc-repl> load 10x.geom
Loaded geometry: 1{b[16]u[10]}2{r:}

seqproc-repl> test "ATCGATCGATCGATCG" "NNNNNNNNNN" "ATCGATCG..."
Read 1:
  bc: ATCGATCGATCGATCG (16bp)
  umi: NNNNNNNNNN (10bp)
Read 2:
  r: ATCGATCG... (150bp)

seqproc-repl> transform
After transformation:
  R1: ATCGATCGATCGATCG (bc, padded)
  R2: ATCGATCG... (unchanged)
```

---

## 5. Detailed Optimization Writeups

This section provides end-to-end explanations of all performance optimizations made to seqproc/ANTISEQUENCE, designed for someone unfamiliar with the codebase.

### 5.1 Output I/O Optimization

#### Background: The Output Bottleneck

When seqproc processes reads, the final step writes transformed reads to FASTQ files. The original implementation had several inefficiencies:

1. **Per-read file path evaluation**: Every read evaluated a file path expression
2. **Global writer lock**: All threads contended on a single mutex to access file writers
3. **Unbatched writes**: Each read triggered a separate write call

#### Optimization 1: Stubbed Output (`ANTISEQ_STUB_OUTPUT`)

**Purpose**: Isolate processing cost from I/O cost for benchmarking.

**Location**: `ANTISEQUENCE/src/graph/ops/output_fastq_op.rs`

**Implementation**:
```rust
#[inline(always)]
fn stub_output() -> bool {
    static STUB: OnceLock<bool> = OnceLock::new();
    *STUB.get_or_init(|| {
        std::env::var("ANTISEQ_STUB_OUTPUT")
            .ok()
            .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
            .unwrap_or(false)
    })
}

impl<T: Trace> GraphNode<T> for OutputFastqFileOp {
    fn run_inner(&self, read: Read) -> Result<(Option<Read>, bool)> {
        if stub_output() { return Ok((Some(read), false)); }
        // ... actual output logic
    }
}
```

**How it works**:
1. `OnceLock` ensures env var is read exactly once (thread-safe, zero-cost after init)
2. When enabled, `run_inner` returns immediately without writing
3. Useful for profiling to see "pure processing" time

**Benchmark result**: Stubbing output showed ~2-5% speedup, indicating output is NOT the dominant bottleneck for in-memory benchmarks.

#### Optimization 2: File Path Expression Optimization

**Purpose**: Avoid re-evaluating constant file paths on every read.

**Location**: `OutputFastqFileOp` constructor

**Implementation**:
```rust
pub struct OutputFastqFileOp {
    file_exprs: Vec<Expr>,
    // Pre-evaluated constant paths: Some(bytes) if constant, None if dynamic
    file_consts: Vec<Option<Vec<u8>>>,
    // ...
}

// In constructor:
let mut file_expr: Expr = file_expr.into();
let _ = file_expr.optimize();  // Fold constants

// Try to evaluate at construction time
let const_path = match file_expr.eval_bytes_static() {
    Ok(bytes) => Some(bytes),
    Err(_) => None,  // Expression depends on read data
};
file_consts.push(const_path);

// In run_inner:
let file_name: Cow<[u8]> = if let Some(c) = self.file_consts.get(i).and_then(|o| o.as_ref()) {
    Cow::Borrowed(&c[..])  // Fast path: use pre-computed
} else {
    file_expr.eval_bytes(&read, false)?  // Slow path: evaluate
};
```

**How it works**:
1. At construction, try to evaluate file path expression without a read
2. If it succeeds (pure constant like `/dev/null`), store the result
3. At runtime, use `Cow::Borrowed` to avoid allocation for constants
4. For dynamic paths (demux), fall back to per-read evaluation

#### Optimization 3: Thread-Local Writer Cache (`WRITER_TLS`)

**Purpose**: Reduce global lock contention when multiple threads write.

**Problem**: Original code had a single `RwLock<HashMap<path, writer>>`. Every read:
1. Acquired read lock to check if writer exists
2. If not, acquired write lock to create writer
3. Acquired writer's mutex to write

**Solution**: Thread-local cache of writer handles.

**Implementation**:
```rust
thread_local! {
    static WRITER_TLS: RefCell<FxHashMap<Vec<u8>, Arc<Mutex<dyn Write + Send>>>> = 
        RefCell::new(FxHashMap::default());
}

fn get_cached_writer(&self, file_name: &[u8]) -> io::Result<Arc<Mutex<dyn Write + Send>>> {
    if writer_tls_disabled() {
        return self.get_writer(file_name);  // Fallback to global
    }
    
    // Check TLS cache first
    if let Some(w) = WRITER_TLS.with(|m| m.borrow().get(file_name).map(Arc::clone)) {
        return Ok(w);
    }
    
    // Not in TLS, get from global and cache
    let w = self.get_writer(file_name)?;
    WRITER_TLS.with(|m| {
        m.borrow_mut().insert(file_name.to_vec(), Arc::clone(&w));
    });
    Ok(w)
}
```

**How it works**:
1. Each thread has its own `FxHashMap` of path → writer Arc
2. First access: get writer from global map, clone Arc into TLS
3. Subsequent accesses: direct TLS lookup, no global lock
4. Writers themselves still have mutex (for actual write serialization)

**Benchmark result**: TLS cache gives **2-5% speedup** on file-output benchmarks.

#### Optimization 4: Output Batching (FAILED EXPERIMENT)

**Purpose**: Reduce write syscalls by buffering multiple records.

**Implementation attempted**:
```rust
struct TlsOutputState {
    writers: FxHashMap<Vec<u8>, Arc<Mutex<dyn Write + Send>>>,
    bufs: FxHashMap<Vec<u8>, BufState>,
}

struct BufState { buf: Vec<u8>, count: usize }

// Per record:
// 1. Serialize FASTQ to temp Vec
// 2. Append to TLS buffer
// 3. When count >= batch_size, flush buffer to writer
```

**Why it failed** (20-30% REGRESSION):
1. **Extra allocation**: Temp Vec for each record before appending
2. **Defeating BufWriter**: Explicit flush after each batch bypassed BufWriter's buffering
3. **Single-threaded benchmark**: No lock contention to amortize
4. **Already buffered**: `/dev/null` + BufWriter already effective

**Lesson learned**: Not all batching helps. Profile before and after!

### 5.2 Threading Optimization

#### Background: Graph Execution Model

ANTISEQUENCE processes reads through a graph of operations:
```
InputFastqOp → MatchOp → SetOp → ... → OutputFastqOp
```

`Graph::run_with_threads(N)` spawns N worker threads, each:
1. Gets a chunk of reads from `InputFastqOp`
2. Processes through all graph nodes
3. Writes to output
4. Repeats until EOF

#### The Threading Bottleneck

**Problem**: For "easy" geometries (like 10x: `1{b[16]u[10]}2{r:}`):
- Per-read processing is very fast (~microseconds)
- Threads spend most time waiting on:
  - Input lock (all threads read from same source)
  - Condition variables (waiting for work)

**Profile evidence**: Flame graphs showed 48-49% of samples in `__psynch_cvwait` (macOS condition variable wait).

#### Optimization: Tunable Chunk Size (`ANTISEQ_CHUNK_SIZE`)

**Purpose**: Balance parallelism overhead vs. work granularity.

**Location**: `ANTISEQUENCE/src/graph/ops/input_fastq_op.rs`

**Implementation**:
```rust
fn chunk_size() -> usize {
    static CHUNK: OnceLock<usize> = OnceLock::new();
    *CHUNK.get_or_init(|| {
        std::env::var("ANTISEQ_CHUNK_SIZE")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|&v| v > 0)
            .unwrap_or(1024)  // Default: 1024 reads per chunk
    })
}
```

**How it affects performance**:
- **Small chunks (256)**: More parallelism granularity, but more lock acquisitions
- **Large chunks (4096+)**: Fewer locks, but threads may starve, worse cache locality
- **Sweet spot (1024)**: Best for 1M+ read workloads with 4 threads

**Benchmark findings**:

| Chunk Size | Threads | 1M 10x Time | Speedup vs 1T |
|------------|---------|-------------|---------------|
| 512        | 4       | ~750ms      | 1.3x          |
| 1024       | 2       | ~640ms      | 1.56x         |
| 1024       | 4       | ~650ms      | 1.53x         |
| 1024       | 8       | ~680ms      | 1.47x         |
| 256        | 4       | ~800ms      | 1.25x         |

**Key insight**: 512 chunk size + >2 threads was consistently BAD. 1024 chunk size is the empirically optimal default.

#### Thread Count Recommendations

Based on profiling:

- **Simple geometry (10x)**: 2-4 threads optimal. More threads hit diminishing returns due to light per-read work.
- **Complex geometry (sci3 tolerant)**: 4 threads helpful. Hamming distance + normalization is CPU-bound.

**Default configuration** (set in code):
```rust
// In antisequence_benches.rs
fn threads() -> usize {
    std::env::var("ANTISEQ_THREADS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|&t| t >= 1)
        .unwrap_or(4)  // Default: 4 threads
}
```

### 5.3 Batched Graph Execution (Read Recycling)

#### Background: Allocation Overhead

Original per-read model:
```rust
loop {
    let read = input_op.run(None)?;  // Allocates new Read
    for node in graph.nodes() {
        node.run(read)?;
    }
    // read dropped, memory freed
}
```

**Problem**: Each `Read` struct contains:
- `StrMappings` with `SmallVec` of mappings
- `SmallAttrMap` for attributes
- Multiple `Vec<u8>` for sequences

Creating/destroying millions of these = significant allocator pressure.

#### Optimization: Vec<Read> Batching with Recycling

**Key insight**: Reuse the same `Read` objects across loop iterations.

**Graph-level change** (`ANTISEQUENCE/src/graph.rs`):
```rust
fn run_trace_inner(&self, trace: &T) -> Result<()> {
    let mut next_input: Option<Vec<Read>> = None;
    loop {
        // Pass previous output as input (for recycling)
        let (out, done) = self.run_one(next_input, trace)?;
        
        // Recycle output vector for next input
        // DO NOT clear - let InputFastqOp handle recycling
        next_input = out;
        
        if done { break; }
    }
    Ok(())
}
```

**InputFastqOp change** (`input_fastq_op.rs`):
```rust
fn run(&self, reads: Option<Vec<Read>>, trace: &T) -> Result<(Option<Vec<Read>>, bool)> {
    let cs = chunk_size();
    let mut b = reads.unwrap_or_else(|| Vec::with_capacity(cs));
    // b already contains Read objects from previous iteration!
    
    let mut i = 0;
    'outer: for _ in 0..cs {
        // Reuse existing Read if available
        if i >= b.len() {
            b.push(Read::new());
        }
        let curr_read = &mut b[i];
        curr_read.clear();  // Reset mappings, keep allocated buffers
        
        // Fill curr_read with new data using recycled buffers
        curr_read.add_fastq_parts_recycled(...);
        
        i += 1;
    }
    
    b.truncate(i);  // Remove unused reads at end
    Ok((Some(b), i == 0))
}
```

**Read recycling** (`read.rs`):
```rust
impl Read {
    pub fn clear(&mut self) {
        for m in &mut self.str_mappings {
            m.recycle();  // Clear mappings but keep Vec capacity
        }
    }
}

impl StrMappings {
    pub fn recycle(&mut self) {
        self.seq.clear();  // Clear but keep capacity
        self.qual.clear();
        self.mappings.clear();
        // Re-add root mapping
        self.mappings.push(StrMapping { ... });
    }
}
```

**Performance impact**:
- Reduced allocator calls by ~90%
- **~30% speedup** over baseline for 1M read benchmarks
- Throughput: ~1.5M reads/sec at 4 threads, 1024 chunk size

### 5.4 Input Optimization

#### set_fastq_entry Optimization

**Original code** used named lookups:
```rust
let mapping = read.mapping_mut("seq1", "root").unwrap();
mapping.set_seq(...);
```

**Optimized code** uses direct index:
```rust
// seq_idx is precomputed (0 for seq1, 1 for seq2)
let str_mappings = &mut read.str_mappings[seq_idx];
str_mappings.set_fastq_entry(name, seq, qual, origin, idx);
```

**Additional optimization**: `set_fastq_entry` now:
1. Extends existing buffers instead of reallocating
2. Uses direct index access to root mapping
3. Avoids bounds checks where safe

### 5.5 Profiling Methodology

#### Tools Used

1. **Criterion benchmarks** (`seqproc/benches/antisequence_benches.rs`):
   - Statistical benchmarking with confidence intervals
   - Baseline comparison for regression detection
   - Multiple sample sizes: 1k, 10k, 1M, 10M reads

2. **Samply profiler**:
   - CPU sampling profiler for macOS
   - Generates flame graphs viewable in Firefox Profiler
   - Command: `samply record -- target/release/seqproc ...`

3. **Environment variables** for A/B testing:
   - `ANTISEQ_STUB_OUTPUT=1`: Disable output writes
   - `ANTISEQ_DISABLE_OUTPUT_TLS=1`: Disable TLS writer cache
   - `ANTISEQ_CHUNK_SIZE=N`: Set input chunk size
   - `ANTISEQ_THREADS=N`: Set worker thread count

#### Benchmark Workflow

```bash
# 1. Create baseline
cargo bench --bench antisequence_benches -- --save-baseline baseline_name

# 2. Make changes

# 3. Compare against baseline
cargo bench --bench antisequence_benches -- --baseline baseline_name | tee results.txt

# 4. Parse results to CSV
python3 bench_change_to_csv.py < results.txt
```

---

## 6. Pending Items for Future Work

### 6.1 Profiling Tasks

**Run samply profiles** for seqproc on 10x and sci3 datasets:
```bash
# 10x 1M
samply record --output profiles/seqproc_10x_1M.samply -- \
  target/release/seqproc --geom 10x.geom \
  --file1 data/bench/10x_N1000000_R1.fastq \
  --file2 data/bench/10x_N1000000_R2.fastq \
  --threads 4 --out1 /dev/null --out2 /dev/null

# 10x 10M
samply record --output profiles/seqproc_10x_10M.samply -- \
  target/release/seqproc --geom 10x.geom \
  --file1 data/bench/10x_N10000000_R1.fastq \
  --file2 data/bench/10x_N10000000_R2.fastq \
  --threads 4 --out1 /dev/null --out2 /dev/null

# sci3 1M
samply record --output profiles/seqproc_sci3_1M.samply -- \
  target/release/seqproc --geom sci3.geom \
  --file1 data/bench/sci3_N1000000_R1.fastq \
  --file2 data/bench/sci3_N1000000_R2.fastq \
  --threads 4 --out1 /dev/null --out2 /dev/null

# sci3 10M
samply record --output profiles/seqproc_sci3_10M.samply -- \
  target/release/seqproc --geom sci3.geom \
  --file1 data/bench/sci3_N10000000_R1.fastq \
  --file2 data/bench/sci3_N10000000_R2.fastq \
  --threads 4 --out1 /dev/null --out2 /dev/null
```

**Analyze hotspots** to understand:
- Why seqproc is faster than Splitcode
- Where further optimization effort should focus
- Whether I/O or compute dominates for each geometry

### 6.2 Splitcode Comparison

Profile Splitcode using similar methodology:
```bash
samply record --output profiles/splitcode_10x_10M.samply -- \
  splitcode -c splitcode_10x.txt --nFastqs=2 \
  data/bench/10x_N10000000_R1.fastq \
  data/bench/10x_N10000000_R2.fastq
```

Compare flame graphs to identify:
- Different algorithmic approaches
- I/O strategies
- Memory allocation patterns

### 6.3 Dynamic Tuning

Implement an auto-tuning system:

1. **Offline advisor mode**:
   ```bash
   seqproc --auto-tune --geom X --file1 ... --file2 ... --max-reads 1000000
   ```
   - Grid search over threads × chunk_size
   - Cache recommendations per (geom_hash, cpu_model)

2. **Runtime heuristic**:
   - Analyze geometry complexity (has `hamming`? `map_with_mismatch`?)
   - Choose threads/chunk based on complexity score

### 6.4 Further Optimizations to Explore

1. **SIMD barcode matching**: Use SIMD for Hamming distance calculation
2. **Memory-mapped input**: Avoid read syscalls for large files
3. **Async I/O**: Overlap reading and processing
4. **Arena allocator**: Pool Read objects instead of per-read allocation

### 6.5 Testing Infrastructure

1. Add comprehensive demux tests (see Section 2.5)
2. Add performance regression tests in CI
3. Create benchmark dashboard for tracking performance over time

---

## Summary

This document covers:

1. **Language refinements**: Error messages, optional intervals, comments, pipelines, validation
2. **Demux state**: Attribute type issues, correctness concerns, testing needs, performance gaps
3. **Statistics system**: Architecture for thread-local collection and aggregation
4. **User tools**: Geometry visualizer, read viewer, protocol repository, REPL
5. **Optimizations**: Detailed explanations of all implemented optimizations with code examples
6. **Pending work**: Profiling tasks, comparison analysis, dynamic tuning, further optimizations

A graduate student picking up this work should:
1. Run the pending profiling tasks (Section 6.1)
2. Analyze hotspots to guide next optimization
3. Consider implementing the auto-tuning system (Section 6.3)
4. Add comprehensive demux tests (Section 2.5)
5. Explore the user tools (Section 4) for broader impact
