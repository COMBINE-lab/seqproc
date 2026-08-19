---
title: Citation and support
description: Cite seqproc, report problems, and contribute changes.
---

## Cite seqproc

Until a journal version of record is available, please cite:

> Noah Cape, Elan Fisher, Daniel Liu, and Rob Patro. “seqproc: An efficient,
> flexible, and concise tool for sequence geometry description and
> transformation.” bioRxiv (2026).

[Read the preprint](https://www.biorxiv.org/content/10.64898/2026.07.28.741211v1)
and record the exact seqproc commit or release used in the methods section.

If a result depends on a paper benchmark configuration, also archive or cite
the corresponding revision of the
[analysis repository](https://github.com/COMBINE-lab/seqproc-paper-analysis).

## Support and contributions

- Use [GitHub Issues](https://github.com/COMBINE-lab/seqproc/issues) for bugs,
  feature requests, and documentation corrections.
- Use [GitHub Discussions](https://github.com/COMBINE-lab/seqproc/discussions)
  for broader protocol-design questions when discussions are enabled.
- Submit focused pull requests with tests for changed language or execution
  behavior.

Changes to matching, ambiguity, ordering, or compression semantics should add
both unit coverage and byte-level output comparisons. Performance changes
should report the workload, build, machine, replicate values, and correctness
checks.

## License

`seqproc` is open source under the
[BSD 3-Clause license](https://github.com/COMBINE-lab/seqproc/blob/main/LICENSE).
