# Production panic audit

This audit covers the `dev` source tree after the typed-error milestone. Test
assertions and test-only `unwrap`, `expect`, and `panic` calls are excluded.
Malformed supported user input must return `SeqprocError`; it must not reach
one of the invariants below.

## Public error boundary

- `compile_geom_typed` distinguishes lexing, parsing, and semantic compilation
  and retains owned source spans.
- `run(RunConfig, CompiledData)` returns `Result<RunReport, SeqprocError>`.
- Resource and mapping/whitelist/anchor-set loading is fallible. Conflicting
  mappings, empty anchor sets, invalid anchor lengths, malformed demultiplexing
  labels, and unreadable inputs do not unwind.
- ANTISEQUENCE error sources are `Send + Sync`, allowing typed failures to cross
  worker and compatibility FIFO thread boundaries.
- Unit-returning `interpret`, `interpret_with_unassigned`, and
  `interpret_with_demux` are explicitly deprecated compatibility wrappers. They
  log and return on failure; their former FASTQ-constructor panics were removed.

## Remaining intentional invariants

| Location | Invariant | Established by |
| --- | --- | --- |
| `resources.rs` resolved-resource lookup | Every compiled resource reference was collected and resolved before graph construction. | Semantic resource-reference validation plus `resolve_resources`. |
| `processors/mod.rs` expression lowering | `normalize` has a ranged interval, and graph-only matching/mapping modifiers never enter scalar expression lowering. | Geometry composition validation and `execute_stack` dispatch. |
| `geometry/parser.rs` layout collapse | A one-element concat/choice contains exactly one value after the cardinality check. | Parser combinator minimum cardinality. |
| `geometry/interpret.rs` labels and alternatives | Compiled reads have a root label, at least one normalized alternative, valid output labels, and read indices fitting `u8`. | Semantic compilation and the public three-lane bound. |
| `geometry/compile/functions.rs` nested function lowering | A successful nested-function result is only extracted after the companion error collection has proved it successful. | `compile_function` partitions successful values and returns accumulated errors before extraction. |
| `geometry/compile/utils.rs` interval-size updates | An update arm is reached only after the interval was proven to carry a bounded size. | The preceding `None` branch returns an unsupported-transformation diagnostic. |
| `geometry/compile/reads.rs` named interval lookup | Every named interval encountered while compiling a read was entered in the definition map. | Definition/reference validation precedes read lowering. |
| `geometry/compile/mod.rs` definition and literal lowering | Definition keys exist after dependency ordering, successful validation results are extracted only after error checks, literal nucleotides are ASCII, and formatting into `String` is infallible. | Semantic dependency validation, lexer alphabet constraints, and `std::fmt::Write for String`. |
| compatibility `compile_geom` conversion | `compile_geom_typed` can only fail with the geometry variant. | The function performs no I/O or graph construction. |

The audit command is:

```console
rg -n 'panic!|unimplemented!|unreachable!|\.unwrap\(|\.expect\(' src --glob '*.rs'
```

Any newly introduced production occurrence must either be removed or added to
this table with its proof boundary and a regression test showing malformed
input cannot reach it.
