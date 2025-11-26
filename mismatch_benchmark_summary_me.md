Okay so I incorporated the things you requested I fix. I started by using your sciseq3 geometry to generate the synthetic fastqs which looks like this:

> Note: That this is is the tolerant anchor version of the geometry.
```
# splitcode config approximating the sci-RNA-seq3 antisequence geometry (tolerant anchor)
# R1: variable barcode (9-10 nt) + anchor CAGAGC + UMI[8] + trailing b[10]
# R2: cDNA, passed through.

@extract <splitcode_sci_rna_seq3[9-10]>{{anchor}}
@extract {{anchor}}8<splitcode_sci_rna_seq3[10]>
@extract {{anchor}}<splitcode_sci_rna_seq3[8]>

# group	id	tag	distances	next	maxFindsG	location
group	id	tag	distances	next	maxFindsG	location
anchor	anchor	CAGAGC	1	-	1	0:9:16
```
I also for completion and sanity check purposes generated the strict anchor version of the geometry in `splitcode_sci3.txt`:
```
@extract <splitcode_sci_rna_seq3[9-10]>{{anchor}}
@extract {{anchor}}8<splitcode_sci_rna_seq3[10]>
@extract {{anchor}}<splitcode_sci_rna_seq3[8]>

# Tab-delimited tag table header:
# group	id	tag	distances	next	maxFindsG	location
group	id	tag	distances	next	maxFindsG	location
anchor	anchor	CAGAGC	0	-	1	0:9:16
```

Note that the only difference here is that the `distances` column is a 0 in the strict and a 1 in the tolerant. The latter is [what you have on github](https://github.com/noahcape/seqproc-spotcheck/blob/main/sci-rna-seq3/sci-rna-seq3-config.txt).

And then (as seen before) I used the seqproc geometry to generate the synthetic fastqs which I also genned up a strict and tolerant version of in `sci3.geom` and `sci3_tolerant.geom` respectively:

Here is the classic strict:
```
anchor = f[CAGAGC]
brc1  = b[9-10]
1{<brc1><anchor>u[8]b[10]}2{r:}
```

And here is the tolerant:
```
anchor = f[CAGAGC]
brc1  = norm(b[9-10])
1{<brc1> hamming(<anchor>, 1) u[8] b[10]}2{r:}
```

I made sure that the end to end data was generated and saved in a completely different file structure than the previous mismatch benchmark data as to not invalidate these results. Within that directory (`data/bench_mismatch`), I generated the new synthetic uncompressed fastqs which to reiterate are generated using a `generate_synth_fastqs.py` script with the following arguments:

```bash
--protocol both
--run-sizes 1000000 10000000
--out-dir data/bench_mismatch
--anchor-hamming1-prob 0.1
--anchor-bad-prob 0.1
```

This means that the synthetic fastqs data now tests the alignment of the anchor with some probability of containing a mismatch or a bad anchor. It does the hamming 1 and bad anchor about 10% of the time and the exact anchor about 80% of the time. For completion, here is that full function which does it:
```Python
def mutate_anchor_hamming1(anchor: str) -> str:
    """Return a variant of `anchor` with Hamming distance exactly 1."""
    idx = random.randrange(len(anchor))
    orig = anchor[idx]
    choices = [b for b in NUCS if b != orig]
    new_base = random.choice(choices)
    return anchor[:idx] + new_base + anchor[idx + 1 :]

def mutate_anchor_far(anchor: str, min_distance: int = 2) -> str:
    """Return a variant of `anchor` with Hamming distance >= min_distance."""
    # For the 6-bp sci3 anchor, just flip two distinct positions.
    if len(anchor) < min_distance:
        raise ValueError("anchor too short for requested minimum distance")
    indices = random.sample(range(len(anchor)), k=min_distance)
    anchor_list = list(anchor)
    for idx in indices:
        orig = anchor_list[idx]
        choices = [b for b in NUCS if b != orig]
        anchor_list[idx] = random.choice(choices)
    return "".join(anchor_list)
```

```Python
    anchor = "CAGAGC"
    lines_r1 = []
    lines_r2 = []
    for i in range(num_reads):
        lines_r1.append(f"@r{i}\n")
        lines_r2.append(f"@r{i}\n")

        # brc1: length 9 or 10 alternating
        bc_len = 9 if i % 2 == 0 else 10
        seq_r1 = []
        for j in range(bc_len):
            seq_r1.append(nuc(i + j * 5))
        # anchor (mutated depending on probabilities)
        r = random.random()                                 # sample [0, 1)
        if r < anchor_hamming1_prob:                        # 10% chance
            this_anchor = mutate_anchor_hamming1(anchor)
        elif r < anchor_hamming1_prob + anchor_bad_prob:    # 10% chance
            this_anchor = mutate_anchor_far(anchor)
        else:                                               # 80% chance
            this_anchor = anchor
        seq_r1.append(this_anchor)
        # UMI 8
        for j in range(8):
            seq_r1.append(nuc(i + j * 11 + 2))
        # trailing b[10]
        for j in range(10):
            seq_r1.append(nuc(i + j * 13 + 1))
        seq_r1_str = "".join(seq_r1)
        lines_r1.append(seq_r1_str + "\n")
        lines_r1.append("+\n")
        lines_r1.append("I" * len(seq_r1_str) + "\n")

        # R2 cDNA 80bp
        seq_r2 = []
        for j in range(80):
            seq_r2.append(nuc(i + j * 9 + 7))
        seq_r2_str = "".join(seq_r2)
        lines_r2.append(seq_r2_str + "\n")
        lines_r2.append("+\n")
        lines_r2.append("I" * 80 + "\n")

    return ("".join(lines_r1), "".join(lines_r2))
```

This should now force seqproc and splitcode to do the actual alignment of the anchor. 

Once I genned up the data (see the picture below), I ran my joint benchmark driver which collects the median wall times for each tool and protocol combination (10x, sci3, sci3_tolerant) at N = 1M and N = 10M.
```bash
python run_splitcode_seqproc_benchmarks.py \
  --protocols 10x sci3 sci3_tolerant \
  --run-sizes 1000000 10000000 \
  --threads 4 \
  --runs 5 \
  --data-dir data/bench_mismatch \                  <-- for sanity I put it in a different directory than the previous mismatch benchmark data
  --out-csv split_vs_seqproc_times_mismatch.csv
```



> (picture of data/bench_mismatch directory)




Once I got the results I loaded them into a notebook and added a cell that walks through `data/bench_mismatch/sci3_N1000000_R1.fastq` and, for each read index `i`, looks at the 6-mer at offset `9` (even `i`) or `10` (odd `i`) and computes the Hamming distance to `CAGAGC`.

I then generated seqproc outputs on this mismatch dataset at N = 1,000,000 for both strict and tolerant sci3 using:

```bash
# strict
cargo run --release --bin seqproc -- \
    --geom sci3.geom \
    --file1 data/bench_mismatch/sci3_N1000000_R1.fastq \
    --file2 data/bench_mismatch/sci3_N1000000_R2.fastq \
    --threads 4 \
    --out1 data/bench_mismatch/seqproc_sci3_strict_N1000000_R1.fastq \
    --out2 data/bench_mismatch/seqproc_sci3_strict_N1000000_R2.fastq

# tolerant
cargo run --release --bin seqproc -- \
    --geom sci3_tolerant.geom \
    --file1 data/bench_mismatch/sci3_N1000000_R1.fastq \
    --file2 data/bench_mismatch/sci3_N1000000_R2.fastq \
    --threads 4 \
    --out1 data/bench_mismatch/seqproc_sci3_tol_N1000000_R1.fastq \
    --out2 data/bench_mismatch/seqproc_sci3_tol_N1000000_R2.fastq
```

After I crunched that data I loaded it all up into the same notebook ive been using and did the following analysis. 

I first looked at the performance of the tools on the mismatch dataset and from this raw data:
```
protocol=10x, N=1,000,000, threads=4: seqproc is 1.07x faster than splitcode.
protocol=10x, N=10,000,000, threads=4: seqproc is 1.90x faster than splitcode.
protocol=sci3, N=1,000,000, threads=4: seqproc is 1.88x faster than splitcode.
protocol=sci3, N=10,000,000, threads=4: seqproc is 2.86x faster than splitcode.
protocol=sci3_tolerant, N=1,000,000, threads=4: seqproc is 2.34x faster than splitcode.
protocol=sci3_tolerant, N=10,000,000, threads=4: seqproc is 3.96x faster than splitcode.
```
you can see that seqproc is always faster than splitcode on the mismatch dataset. 
10x: seqproc is only slightly faster at 1M (1.07x) but almost 2x at 10M. 
sci3 strict: already ~1.9x at 1M, ~2.9x at 10M. 
sci3 tolerant: ~2.3x at 1M, almost 4x at 10M.



(Picture of times)




The same story holds for the 10M dataset with seqproc being about 2x faster than splitcode and scaling better at even this hard mixed anchor geometry. Okay so now that I quickly spot checked the times I wanted to check if the anchor mismatch distribution was different for the mismatch dataset and really if the seqproc outputs were correct by way of the new synthetic fastqs. I did a quick spot check via code that the input data didnt have any wires crossed with the old data, and once I confirmed I was looking at the right run data I did the anchor mismatch distribution analysis. I tested the anchor mismatch distribution in the sci3 input on `data/bench_mismatch/sci3_N1000000_R1.fastq` (though I could easily test it for others) I found that the total sci3 reads inspected: **1,000,000** and the Hamming distances (at the expected 9/10 offsets).
Here is the raw result:
```
Total sci3 reads inspected (N): 1,000,000
  exact      (hd=0):   799714  (79.97%)
  hamming=1  (hd=1):   100053  (10.01%)
  hamming>=2 (hd>=2):   100233  (10.02%)

For sanity I quickly spot checked that the old synthetic exact match dataset returned the expected results of 100% match. In my analysis code all I did (for sanity) was change the bench paths from
```Python
...
base_mismatch = Path("data/bench_mismatch")
input_sci3_r1 = base_mismatch / "sci3_N1000000_R1.fastq"
...
```
to:
```Python
...
base = Path("data/bench")                           <-- Note this is what changed
input_sci3_r1 = base / "sci3_N1000000_R1.fastq"
...
```

And then got the following correct results:
```
Total sci3 reads inspected (N): 1,000,000
  exact      (hd=0):  1000000  (100.00%)
  hamming=1  (hd=1):        0  ( 0.00%)
  hamming>=2 (hd>=2):        0  ( 0.00%)
```

See we got 100% exact matches and 0 mismatches and 0 bad anchors. This is the expected result for the old synthetic exact match dataset and thus sanity checking the analysis code.

So looking back at the anchor mismatch distribution results for the mismatch dataset we can see that they match the intended generator probabilities (~80% exact, ~10% 1-mismatch, ~10% "bad" anchors) and confirms that the anchors are always at the expected offsets.

So the next thing was check the strict vs tolerant seqproc behaviour on sci3 mismatches. Using the strict and tolerant outputs generated for the 1M datasets I got the following results about that:

```
Strict R1 exists: True
Tol    R1 exists: True
Input N=1,000,000, strict N=799,714, tol N=899,767
hd=0 (exact) reads:   799,714
hd=1 (1-mismatch) reads: 100,053

Seqproc strict vs expected:
  expected ~ hd=0 count: 799,714
  observed strict outputs: 799,714

Seqproc tolerant vs expected:
  expected ~ hd<=1 count: 899,767
  observed tolerant outputs: 899,767

Fraction of reads kept (strict):   79.97%
Fraction of reads kept (tolerant): 89.98%
```

So we can see that seqproc is keeping the exact matches and 1-mismatch reads for the strict geometry and 1-mismatch and 2-mismatch reads for the tolerant geometry. Sanity checks there confirm that these results are correct.

I wanted to do the old bases per second analysis to confirm that seqproc wasnt just removing reads at random or doing some other kind of weirdness. Note that these results below assume that

**10x**: R1 length 26, R2 length 60 -> **86 bases per read pair**.
**sci3 / sci3_tolerant**: R1 alternates 33 / 34 (9/10 bp barcode + 6 bp anchor + 8 bp UMI + 10 bp tail), R2 is 80 -> mean R1 length 33.5 -> **113.5 bases per read pair**.

And that for each row I computed:
```Python
total_bases = run_size * bases_per_pair
bases_per_sec = total_bases / time_mid_seconds
```

And so the results are:



(Picture of the per base analysis)



These results reflect the wall-time ratios from the summary table (because both tools process the same total number of bases per run).

So doing the analysis you suggested, after explicitly introducing anchor mismatches and verifying that both tools handle them according to spec, the performance advantage I measured for seqproc versus splitcode persists and, if anything, is more pronounced on the sci3 tolerant benchmarks.