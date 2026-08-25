# seqspec compatibility report

Assessed **139** modality projections; **25** generated EFGDL 2 that compiles. Counts are modality-level, not marketing claims about whole assays. Capability impact is a conservative lower bound: rows requiring explicit read/modality policy are not guessed or attributed to a later capability.

## Status summary

| Status | Modalities |
| --- | ---: |
| `BlockedSeqprocCapability` | 23 |
| `BlockedSourceInvalid` | 20 |
| `NeedsUserPolicy` | 71 |
| `Supported` | 1 |
| `SupportedRequiresBinding` | 24 |

Manifest digest: `blake3:88c2d51d51e1d33ec2de5c58f29fbc17fe16bfc8b8cb8cebeaad03e21647c1e5`; importer version: `0.1.0`.

## Corpus provenance

| Corpus | Commit | Modality rows |
| --- | --- | ---: |
| igvf-seqspec | `7f7c7d6264a39910c69902e056621b1d22895636` | 68 |
| pachterlab-seqspec | `0771046233fdf93542253c28ecda75054b730080` | 71 |

## Capability impact

| Rank | Missing capability | Blocked modalities | Blocked specs |
| ---: | --- | ---: | ---: |
| 1 | `variable_read_window_layout` | 23 | 22 |
| 2 | `general_variable_boundary_matching` | 21 | 20 |
| 3 | `partial_fixed_prefix_matching` | 18 | 17 |
| 4 | `partial_onlist_window_matching` | 6 | 6 |

## Protocol matrix

| Corpus | Specification | Modality | Status | Lanes | EFGDL compiles | Required capability/action |
| --- | --- | --- | --- | ---: | --- | --- |
| igvf-seqspec | `specs/10xCRISPR/spec.yaml` | crispr | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/10xCRISPR/spec.yaml` | rna | `BlockedSourceInvalid` | 6 | no | primer_not_found |
| igvf-seqspec | `specs/10x_atac/spec.yaml` | atac | `SupportedRequiresBinding` | 4 | yes |  |
| igvf-seqspec | `specs/10x_rna_5prime/spec.yaml` | rna | `BlockedSeqprocCapability` | 3 | no | variable_read_window_layout |
| igvf-seqspec | `specs/10x_rna_atac/spec.yaml` | atac | `SupportedRequiresBinding` | 4 | yes |  |
| igvf-seqspec | `specs/10x_rna_atac/spec.yaml` | rna | `SupportedRequiresBinding` | 3 | yes |  |
| igvf-seqspec | `specs/10x_rna_atac_multi/spec.yaml` | atac | `SupportedRequiresBinding` | 4 | yes |  |
| igvf-seqspec | `specs/10x_rna_atac_multi/spec.yaml` | rna | `SupportedRequiresBinding` | 3 | yes |  |
| igvf-seqspec | `specs/10x_rna_atac_multi/spec.yaml` | tag | `SupportedRequiresBinding` | 3 | yes |  |
| igvf-seqspec | `specs/10x_rna_v1/spec.yaml` | rna | `Supported` | 2 | yes |  |
| igvf-seqspec | `specs/10x_rna_v2/spec.yaml` | rna | `SupportedRequiresBinding` | 2 | yes |  |
| igvf-seqspec | `specs/10x_rna_v3/spec.yaml` | rna | `SupportedRequiresBinding` | 3 | yes |  |
| igvf-seqspec | `specs/10xfb_3prime/spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/10xfb_3prime/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/10xfb_5prime/spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/10xfb_5prime/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/10xfb_vdj_5prime/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/10xfb_vdj_5prime/spec.yaml` | vdj | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/PIPseqV2/spec.yaml` | RNA | `SupportedRequiresBinding` | 2 | yes |  |
| igvf-seqspec | `specs/PIPseqV3/spec.yaml` | RNA | `SupportedRequiresBinding` | 2 | yes |  |
| igvf-seqspec | `specs/SureCell/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/Tang2009/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/bd_rhapsody_eb/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/bd_rhapsody_v1/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/cel_seq/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/cel_seq2/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/dogmaseq_dig/spec.yaml` | atac | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/dogmaseq_dig/spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/dogmaseq_dig/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/dogmaseq_dig/spec.yaml` | tag | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/dogmaseq_lll/spec.yaml` | atac | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/dogmaseq_lll/spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/dogmaseq_lll/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/drop_seq/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/inDropv2/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/issaac_seq/spec.yaml` | ATAC | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/issaac_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/mars_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/mcscrb_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/microwell_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/parse_wt_v2/spec.yaml` | rna | `BlockedSourceInvalid` | 0 | no | modality_library_missing, modality_reads_missing |
| igvf-seqspec | `specs/pi_atac_seq/spec.yaml` | ATAC | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/quartz_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/quartz_seq2/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/scCRISPRa/spec.yaml` | crispr | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/scCRISPRa/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/sci_rna_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/sci_rna_seq3/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/scifi_rna_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/seq_well/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/seq_well_s3/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/share_seq/spec.yaml` | atac | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/share_seq/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/smart_seq2/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/smart_seq3/spec.yaml` | RNA_end | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/smart_seq3/spec.yaml` | RNA_internal | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/sn_m3c_seq/spec.yaml` | hic | `BlockedSourceInvalid` | 2 | no | fixed_sequence_length_mismatch |
| igvf-seqspec | `specs/sn_m3c_seq/spec.yaml` | methyl | `BlockedSourceInvalid` | 2 | no | fixed_sequence_length_mismatch |
| igvf-seqspec | `specs/snmCTseq/spec.yaml` | methyl | `BlockedSourceInvalid` | 2 | no | fixed_sequence_length_mismatch |
| igvf-seqspec | `specs/snmCTseq/spec.yaml` | rna | `BlockedSourceInvalid` | 2 | no | fixed_sequence_length_mismatch |
| igvf-seqspec | `specs/split_seq/spec.yaml` | RNA | `SupportedRequiresBinding` | 2 | yes |  |
| igvf-seqspec | `specs/strt_seq/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/strt_seq_2i/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/strt_seq_c1/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/sugarseq/spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/sugarseq/spec.yaml` | tag | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/vasa_seq_drop/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| igvf-seqspec | `specs/vasa_seq_plate/spec.yaml` | RNA | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10x_atac.spec.yaml` | atac | `SupportedRequiresBinding` | 4 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_5prime.spec.yaml` | rna | `BlockedSeqprocCapability` | 3 | no | variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_atac.spec.yaml` | atac | `SupportedRequiresBinding` | 4 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_atac.spec.yaml` | rna | `SupportedRequiresBinding` | 3 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_atac_multi.spec.yaml` | atac | `SupportedRequiresBinding` | 4 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_atac_multi.spec.yaml` | rna | `SupportedRequiresBinding` | 3 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_atac_multi.spec.yaml` | tag | `SupportedRequiresBinding` | 3 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_v1.spec.yaml` | rna | `SupportedRequiresBinding` | 4 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_v2.spec.yaml` | rna | `SupportedRequiresBinding` | 3 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10x_rna_v3.spec.yaml` | rna | `SupportedRequiresBinding` | 3 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10xcrispr.spec.yaml` | crispr | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10xcrispr.spec.yaml` | rna | `SupportedRequiresBinding` | 6 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10xfb_3prime.spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10xfb_3prime.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10xfb_5prime.spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10xfb_5prime.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10xfb_vdj_5prime.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10xfb_vdj_5prime.spec.yaml` | vdj | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/10xv3_scrnaseq_element_adept_truseq_dual.spec.yaml` | rna | `SupportedRequiresBinding` | 4 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/10xv3_scrnaseq_illumina_truseq_dual.spec.yaml` | rna | `SupportedRequiresBinding` | 4 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/bd_rhapsody_eb.spec.yaml` | rna | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/bd_rhapsody_v1.spec.yaml` | rna | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_onlist_window_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/cel_seq.spec.yaml` | rna | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, partial_onlist_window_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/cel_seq2.spec.yaml` | rna | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, partial_onlist_window_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/dogmaseq_dig.spec.yaml` | atac | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/dogmaseq_dig.spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/dogmaseq_dig.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/dogmaseq_dig.spec.yaml` | tag | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/dogmaseq_lll.spec.yaml` | atac | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/dogmaseq_lll.spec.yaml` | protein | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/dogmaseq_lll.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/drop_seq.spec.yaml` | rna | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/indropv2.spec.yaml` | rna | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, partial_onlist_window_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/issaac_seq.spec.yaml` | ATAC | `BlockedSeqprocCapability` | 4 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/issaac_seq.spec.yaml` | RNA | `BlockedSeqprocCapability` | 4 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/mars_seq.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/mcscrb_seq.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/microwell_seq.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/parse_wt_v2.spec.yaml` | rna | `SupportedRequiresBinding` | 2 | yes |  |
| pachterlab-seqspec | `docs/examples/assays/pi_atac_seq.spec.yaml` | ATAC | `BlockedSourceInvalid` | 2 | no | fixed_sequence_length_mismatch, primer_not_unique |
| pachterlab-seqspec | `docs/examples/assays/pipseqv2.spec.yaml` | RNA | `BlockedSeqprocCapability` | 3 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/pipseqv3.spec.yaml` | RNA | `BlockedSeqprocCapability` | 4 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/quartz_seq.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | read_window_exceeds_library |
| pachterlab-seqspec | `docs/examples/assays/quartz_seq2.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | general_variable_boundary_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/sccrispra.spec.yaml` | crispr | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/sccrispra.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/sci_rna_seq.spec.yaml` | RNA | `BlockedSeqprocCapability` | 4 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/sci_rna_seq3.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, partial_onlist_window_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/scifi_rna_seq.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | general_variable_boundary_matching, partial_onlist_window_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/seq_well.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/seq_well_s3.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/share_seq.spec.yaml` | atac | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/share_seq.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/smart_seq2.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | read_window_exceeds_library |
| pachterlab-seqspec | `docs/examples/assays/smart_seq3.spec.yaml` | RNA_end | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/smart_seq3.spec.yaml` | RNA_internal | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/sn_m3c_seq.spec.yaml` | hic | `BlockedSourceInvalid` | 4 | no | fixed_sequence_length_mismatch |
| pachterlab-seqspec | `docs/examples/assays/sn_m3c_seq.spec.yaml` | methyl | `BlockedSourceInvalid` | 4 | no | fixed_sequence_length_mismatch |
| pachterlab-seqspec | `docs/examples/assays/snmctseq.spec.yaml` | methyl | `BlockedSourceInvalid` | 4 | no | fixed_sequence_length_mismatch |
| pachterlab-seqspec | `docs/examples/assays/snmctseq.spec.yaml` | rna | `BlockedSourceInvalid` | 4 | no | fixed_sequence_length_mismatch |
| pachterlab-seqspec | `docs/examples/assays/split_seq.spec.yaml` | RNA | `BlockedSeqprocCapability` | 4 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/strt_seq.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | primer_not_found |
| pachterlab-seqspec | `docs/examples/assays/strt_seq_2i.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | primer_not_found |
| pachterlab-seqspec | `docs/examples/assays/strt_seq_c1.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | primer_not_found |
| pachterlab-seqspec | `docs/examples/assays/sugarseq.spec.yaml` | rna | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/sugarseq.spec.yaml` | tag | `NeedsUserPolicy` | 0 | no | modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/surecell.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/tang2009.spec.yaml` | RNA | `BlockedSourceInvalid` | 2 | no | primer_not_found |
| pachterlab-seqspec | `docs/examples/assays/template.spec.yaml` | mode | `BlockedSourceInvalid` | 0 | no | missing_sequence_spec, modality_reads_missing |
| pachterlab-seqspec | `docs/examples/assays/vasa_seq_drop.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, variable_read_window_layout |
| pachterlab-seqspec | `docs/examples/assays/vasa_seq_plate.spec.yaml` | RNA | `BlockedSeqprocCapability` | 2 | no | general_variable_boundary_matching, partial_fixed_prefix_matching, partial_onlist_window_matching, variable_read_window_layout |
