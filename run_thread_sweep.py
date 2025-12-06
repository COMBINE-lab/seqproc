#!/usr/bin/env python3
import subprocess
import sys
import shlex

THREADS = [1, 2, 4, 8, 12]
RUNS = 3
N = 10_000_000
PROTOCOL = "split_seq_v1"
OUT_CSV = "thread_sweep_10M.csv"

SPLITCODE_CMD_TEMPLATE = (
    "../splitcode/build/src/splitcode -c splitcode_configs/splitcode_split_seq_v1.txt "
    "--nFastqs=2 data/bench/split_seq_v1_N10000000_R1.fastq data/bench/split_seq_v1_N10000000_R2.fastq "
    "-o /dev/null,/dev/null -t {t}"
)

SEQPROC_CMD_TEMPLATE = (
    "cargo run --release --bin seqproc -- --geom split_seq_v1.geom "
    "--file1 data/bench/split_seq_v1_N10000000_R1.fastq --file2 data/bench/split_seq_v1_N10000000_R2.fastq "
    "--threads {t} --out1 /dev/null --out2 /dev/null"
)

def run_bench(tool, cmd, t):
    print(f"Running {tool} with {t} threads...")
    bench_cmd = [
        sys.executable, "bench_command_to_csv.py",
        "--tool", tool,
        "--protocol", PROTOCOL,
        "--run-size", str(N),
        "--threads", str(t),
        "--runs", str(RUNS),
        "--out-csv", OUT_CSV,
        "--",
    ] + shlex.split(cmd)
    
    subprocess.check_call(bench_cmd)

def main():
    print(f"Starting thread sweep for N={N}")
    
    # Clear output CSV if exists? No, bench_command_to_csv appends. 
    # But maybe we want a fresh start.
    # subprocess.call(["rm", OUT_CSV]) 

    for t in THREADS:
        # Run Splitcode
        sc_cmd = SPLITCODE_CMD_TEMPLATE.format(t=t)
        run_bench("splitcode", sc_cmd, t)
        
        # Run Seqproc
        sp_cmd = SEQPROC_CMD_TEMPLATE.format(t=t)
        run_bench("seqproc", sp_cmd, t)

if __name__ == "__main__":
    main()
