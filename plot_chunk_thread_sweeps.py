#!/usr/bin/env python3
import csv
import glob
import os
import re
from collections import defaultdict

import numpy as np
import matplotlib.pyplot as plt

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
SWEEPS_DIR = os.path.join(SCRIPT_DIR, "sweeps")

FILE_RE = re.compile(r"bench_c(\d+)_t(\d+)\.csv$")

# benchmark -> list of (chunk, threads, time_mid_seconds)
data = defaultdict(list)

for path in glob.glob(os.path.join(SWEEPS_DIR, "bench_c*_t*.csv")):
    m = FILE_RE.search(os.path.basename(path))
    if not m:
        continue
    chunk = int(m.group(1))
    threads = int(m.group(2))
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            bench = row["benchmark"]
            try:
                tsec = float(row["time_mid_seconds"])
            except (KeyError, ValueError):
                continue
            data[bench].append((chunk, threads, tsec))

os.makedirs(SWEEPS_DIR, exist_ok=True)

def sanitize(name: str) -> str:
    return re.sub(r"[^A-Za-z0-9_.-]+", "_", name)

arrays = {}
agg = defaultdict(list)  # (chunk, threads) -> list of median times

for bench, records in data.items():
    # Deduplicate (chunk,threads) pairs
    uniq = {}
    for chunk, threads, change in records:
        uniq[(chunk, threads)] = change
    records = [(c, t, ch) for (c, t), ch in uniq.items()]

    chunks = sorted({c for c, _, _ in records})
    threads_list = sorted({t for _, t, _ in records})

    if not chunks or not threads_list:
        continue

    # Build matrix rows=chunks, cols=threads
    mat = [[np.nan for _ in threads_list] for _ in chunks]
    for c, t, ch in records:
        i = chunks.index(c)
        j = threads_list.index(t)
        mat[i][j] = ch
    arr = np.array(mat, dtype=float)

    safe_name = sanitize(bench)

    # Store for combined plots and aggregate stats
    arrays[bench] = (chunks, threads_list, arr)
    for i, c in enumerate(chunks):
        for j, t in enumerate(threads_list):
            val = arr[i, j]
            if not np.isnan(val):
                agg[(c, t)].append(val)

    # Line plot: median time vs threads, one line per chunk size
    plt.figure()
    for i, c in enumerate(chunks):
        ys = arr[i, :]
        xs = []
        ys_valid = []
        for x, y in zip(threads_list, ys):
            if not np.isnan(y):
                xs.append(x)
                ys_valid.append(y)
        if not xs:
            continue
        plt.plot(xs, ys_valid, marker="o", label=f"chunk={c}")
    plt.xlabel("Threads")
    plt.ylabel("Median time (s)")
    plt.title(bench)
    plt.legend()
    plt.grid(True, alpha=0.3)
    out_line = os.path.join(SWEEPS_DIR, f"{safe_name}_lines.png")
    plt.tight_layout()
    plt.savefig(out_line, dpi=150)
    plt.close()

    # Heatmap: chunks x threads
    plt.figure()
    im = plt.imshow(
        arr,
        aspect="auto",
        origin="lower",
        extent=[min(threads_list) - 0.5, max(threads_list) + 0.5,
                min(chunks) - 0.5, max(chunks) + 0.5],
    )
    plt.colorbar(im, label="Median time (s)")
    plt.xlabel("Threads")
    plt.ylabel("Chunk size")
    plt.yticks(chunks)
    plt.xticks(threads_list)
    plt.title(bench)
    out_heat = os.path.join(SWEEPS_DIR, f"{safe_name}_heatmap.png")
    plt.tight_layout()
    plt.savefig(out_heat, dpi=150)
    plt.close()

# Combined multi-subplot figures for all benchmarks
benches = sorted(arrays.keys())
if benches:
    # Determine grid size
    n = len(benches)
    ncols = 2
    nrows = (n + ncols - 1) // ncols

    # Combined line plots
    fig, axs = plt.subplots(nrows, ncols, figsize=(5 * ncols, 3.5 * nrows), sharex=True, sharey=True)
    if not isinstance(axs, np.ndarray):
        axs = np.array([[axs]])
    axs_flat = axs.ravel()

    for ax, bench in zip(axs_flat, benches):
        chunks, threads_list, arr = arrays[bench]
        for i, c in enumerate(chunks):
            ys = arr[i, :]
            xs = []
            ys_valid = []
            for x, y in zip(threads_list, ys):
                if not np.isnan(y):
                    xs.append(x)
                    ys_valid.append(y)
            if xs:
                ax.plot(xs, ys_valid, marker="o", label=f"chunk={c}")
        ax.set_title(bench, fontsize=8)
        ax.grid(True, alpha=0.3)

    # Turn off any unused axes
    for ax in axs_flat[len(benches) :]:
        ax.axis("off")

    fig.text(0.5, 0.04, "Threads", ha="center")
    fig.text(0.04, 0.5, "Median time (s)", va="center", rotation="vertical")

    # Global legend from first non-empty axis
    handles, labels = [], []
    for ax in axs_flat:
        h, l = ax.get_legend_handles_labels()
        if h:
            handles, labels = h, l
            break
    if handles:
        fig.legend(handles, labels, loc="upper center", ncol=len(handles))

    out_all_lines = os.path.join(SWEEPS_DIR, "all_benchmarks_lines.png")
    fig.tight_layout(rect=[0.03, 0.06, 0.97, 0.9])
    fig.savefig(out_all_lines, dpi=150)
    plt.close(fig)

    # Combined heatmaps
    fig, axs = plt.subplots(nrows, ncols, figsize=(5 * ncols, 3.5 * nrows), sharex=True, sharey=True)
    if not isinstance(axs, np.ndarray):
        axs = np.array([[axs]])
    axs_flat = axs.ravel()

    # Use common extent across all benchmarks
    # Assume all have the same chunks/threads grid (from the sweep script)
    ref_chunks, ref_threads, _ = arrays[benches[0]]

    for ax, bench in zip(axs_flat, benches):
        chunks, threads_list, arr = arrays[bench]
        im = ax.imshow(
            arr,
            aspect="auto",
            origin="lower",
            extent=[min(threads_list) - 0.5, max(threads_list) + 0.5,
                    min(chunks) - 0.5, max(chunks) + 0.5],
        )
        ax.set_title(bench, fontsize=8)
        ax.set_xticks(ref_threads)
        ax.set_yticks(ref_chunks)

    for ax in axs_flat[len(benches) :]:
        ax.axis("off")

    cbar = fig.colorbar(im, ax=axs_flat[: len(benches)], shrink=0.8)
    cbar.set_label("Median time (s)")
    fig.text(0.5, 0.04, "Threads", ha="center")
    fig.text(0.04, 0.5, "Chunk size", va="center", rotation="vertical")

    out_all_heat = os.path.join(SWEEPS_DIR, "all_benchmarks_heatmaps.png")
    fig.tight_layout(rect=[0.03, 0.06, 0.97, 0.9])
    fig.savefig(out_all_heat, dpi=150)
    plt.close(fig)

    # Aggregated metrics across benchmarks: mean median time per (chunk, threads)
    agg_chunks = sorted({c for (c, _) in agg.keys()})
    agg_threads = sorted({t for (_, t) in agg.keys()})

    if agg_chunks and agg_threads:
        mat = [[np.nan for _ in agg_threads] for _ in agg_chunks]
        for (c, t), vals in agg.items():
            i = agg_chunks.index(c)
            j = agg_threads.index(t)
            mat[i][j] = float(np.mean(vals))
        arr_agg = np.array(mat, dtype=float)

        # Aggregated heatmap
        fig, ax = plt.subplots(figsize=(6, 4))
        im = ax.imshow(
            arr_agg,
            aspect="auto",
            origin="lower",
            extent=[min(agg_threads) - 0.5, max(agg_threads) + 0.5,
                    min(agg_chunks) - 0.5, max(agg_chunks) + 0.5],
        )
        ax.set_xticks(agg_threads)
        ax.set_yticks(agg_chunks)
        ax.set_xlabel("Threads")
        ax.set_ylabel("Chunk size")
        ax.set_title("Mean median time across all benchmarks")
        cbar = fig.colorbar(im)
        cbar.set_label("Mean median time (s)")
        out_agg_heat = os.path.join(SWEEPS_DIR, "aggregate_heatmap.png")
        fig.tight_layout()
        fig.savefig(out_agg_heat, dpi=150)
        plt.close(fig)

        # Aggregated line plot (mean over benchmarks)
        fig, ax = plt.subplots(figsize=(6, 4))
        for i, c in enumerate(agg_chunks):
            ys = arr_agg[i, :]
            xs = []
            ys_valid = []
            for x, y in zip(agg_threads, ys):
                if not np.isnan(y):
                    xs.append(x)
                    ys_valid.append(y)
            if xs:
                ax.plot(xs, ys_valid, marker="o", label=f"chunk={c}")
        ax.set_xlabel("Threads")
        ax.set_ylabel("Mean median time (s)")
        ax.set_title("Mean median time across all benchmarks")
        ax.grid(True, alpha=0.3)
        ax.legend()
        out_agg_lines = os.path.join(SWEEPS_DIR, "aggregate_lines.png")
        fig.tight_layout()
        fig.savefig(out_agg_lines, dpi=150)
        plt.close(fig)

print(f"Wrote plots to {SWEEPS_DIR}")
