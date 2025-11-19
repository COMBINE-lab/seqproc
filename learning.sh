# ============================
# STEP 0: Build the binary
# ============================
set -euo pipefail
echo "STEP 0: Building seqproc binary"
cargo build --bin seqproc

# ============================
# STEP 1: Create a tiny geometry (EFGDL) file
# ============================
echo "STEP 1: Writing tiny.efgdl"
cat > tiny.efgdl << 'EFGDL'
brc = b[2]
1{<brc>}2{r<read>:}
 -> 1{<brc>}2{<read>}
EFGDL

echo "STEP 1: Geometry file contents:"
nl -ba tiny.efgdl

# ============================
# STEP 2: Create minimal FASTQ inputs
# R1 has >=2 bases for the barcode; R2 is arbitrary cDNA
# ============================
echo "STEP 2: Writing r1.fastq and r2.fastq"
cat > r1.fastq << 'R1'
@r1
ACGT
+
IIII
R1

cat > r2.fastq << 'R2'
@r1
GGGGGG
+
IIIIII
R2

echo "STEP 2: Input R1:"
nl -ba r1.fastq
echo "STEP 2: Input R2:"
nl -ba r2.fastq

# ============================
# STEP 3: Run seqproc with the tiny geometry
# -o, -w specify output fastq files
# RUST_LOG=info shows CLI logs (defaults to INFO if unset)
# ============================
echo "STEP 3: Running seqproc"
RUST_LOG=info \
target/debug/seqproc \
  --geom tiny.efgdl \
  -1 r1.fastq \
  -2 r2.fastq \
  -o out1.fastq \
  -w out2.fastq

# ============================
# STEP 4: Inspect outputs
# Expectation:
#   - out1.fastq contains the first 2 bases of R1 (the barcode) => "AC"
#   - out2.fastq contains the entire R2 read                       => "GGGGGG"
# ============================
echo "STEP 4: Output R1 (out1.fastq):"
nl -ba out1.fastq

echo "STEP 4: Output R2 (out2.fastq):"
nl -ba out2.fastq