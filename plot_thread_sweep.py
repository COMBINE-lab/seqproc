#!/usr/bin/env python3
import csv
import matplotlib.pyplot as plt
from collections import defaultdict

CSV_FILE = "thread_sweep_10M.csv"
OUT_PLOT = "thread_sweep_10M.png"

def main():
    data = defaultdict(list) # tool -> [(threads, time)]
    
    try:
        with open(CSV_FILE, "r") as f:
            reader = csv.DictReader(f)
            for row in reader:
                tool = row["tool"]
                threads = int(row["threads"])
                time_sec = float(row["time_mid_seconds"])
                data[tool].append((threads, time_sec))
    except FileNotFoundError:
        print(f"Error: {CSV_FILE} not found. Run run_thread_sweep.py first.")
        return

    plt.figure(figsize=(10, 6))
    
    colors = {"splitcode": "red", "seqproc": "blue"}
    markers = {"splitcode": "o", "seqproc": "s"}
    
    for tool, points in data.items():
        points.sort() # Sort by threads
        xs = [p[0] for p in points]
        ys = [p[1] for p in points]
        
        plt.plot(xs, ys, label=tool, color=colors.get(tool, "black"), marker=markers.get(tool, "x"), linewidth=2)
        
        # Annotate points
        for x, y in zip(xs, ys):
            plt.annotate(f"{y:.2f}s", (x, y), textcoords="offset points", xytext=(0, 10), ha='center')

    plt.title("Performance Comparison: Splitcode vs Seqproc (10M reads, SPLiT-seq v1)")
    plt.xlabel("Threads")
    plt.ylabel("Time (seconds)")
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.legend()
    plt.xticks([1, 2, 4, 8, 12])
    
    plt.savefig(OUT_PLOT)
    print(f"Plot saved to {OUT_PLOT}")

if __name__ == "__main__":
    main()
