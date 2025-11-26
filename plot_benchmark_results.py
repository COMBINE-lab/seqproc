#!/usr/bin/env python3
import csv
import os
import sys
from collections import defaultdict
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <benchmark_csv>")
        sys.exit(1)
    
    csv_path = sys.argv[1]
    if not os.path.exists(csv_path):
        print(f"Error: {csv_path} not found")
        sys.exit(1)

    df = pd.read_csv(csv_path)
    
    # Filter out unrelated columns if necessary, but pandas handles mostly.
    # Ensure types
    df['time_mid_seconds'] = pd.to_numeric(df['time_mid_seconds'], errors='coerce')
    df['run_size'] = pd.to_numeric(df['run_size'], errors='coerce')
    df['threads'] = pd.to_numeric(df['threads'], errors='coerce')
    
    # Drop rows with NaN
    df.dropna(subset=['time_mid_seconds', 'run_size', 'threads'], inplace=True)

    out_dir = Path("plots")
    out_dir.mkdir(exist_ok=True)

    protocols = df['protocol'].unique()
    
    # Plot 1: Time vs Threads (for largest N)
    for protocol in protocols:
        subset = df[df['protocol'] == protocol]
        max_n = subset['run_size'].max()
        data = subset[subset['run_size'] == max_n]
        
        if data.empty:
            continue

        plt.figure(figsize=(8, 6))
        tools = data['tool'].unique()
        for tool in tools:
            tool_data = data[data['tool'] == tool].sort_values('threads')
            plt.plot(tool_data['threads'], tool_data['time_mid_seconds'], marker='o', label=tool)
        
        plt.title(f"{protocol} Performance (N={max_n})")
        plt.xlabel("Threads")
        plt.ylabel("Time (s)")
        plt.legend()
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(out_dir / f"{protocol}_time_vs_threads_N{max_n}.png")
        plt.close()

    # Plot 2: Time vs Run Size (for fixed threads, e.g. 4 or max)
    for protocol in protocols:
        subset = df[df['protocol'] == protocol]
        # Find most common thread count or max
        # Default to 4 if present, else max
        if 4 in subset['threads'].unique():
            fixed_t = 4
        else:
            fixed_t = subset['threads'].max()
            
        data = subset[subset['threads'] == fixed_t]
        
        if data.empty:
            continue

        plt.figure(figsize=(8, 6))
        tools = data['tool'].unique()
        for tool in tools:
            tool_data = data[data['tool'] == tool].sort_values('run_size')
            plt.plot(tool_data['run_size'], tool_data['time_mid_seconds'], marker='o', label=tool)
            
        plt.title(f"{protocol} Performance (Threads={fixed_t})")
        plt.xlabel("Run Size (N)")
        plt.ylabel("Time (s)")
        plt.xscale('log')
        plt.yscale('log')
        plt.legend()
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(out_dir / f"{protocol}_time_vs_size_T{fixed_t}.png")
        plt.close()

    print(f"Plots saved to {out_dir}")

if __name__ == "__main__":
    main()
