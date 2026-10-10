"""Summarise slab_cache_bench.sh runs: median/p95 per brick, server-read share, MB/s.

    python3 slab_cache_summary.py <results-dir> A B C

MB/s is aggregate throughput (total bytes / run span), where the run span is the sum of wall times at
concurrency 1 and is reported by the bench script otherwise; per-request MB/s is bytes / wall.
"""
import csv
import statistics
import sys
from pathlib import Path


def pct(xs, p):
    xs = sorted(xs)
    k = (len(xs) - 1) * p / 100
    lo, hi = int(k), min(int(k) + 1, len(xs) - 1)
    return xs[lo] + (xs[hi] - xs[lo]) * (k - lo)


def main():
    d = Path(sys.argv[1])
    for label in sys.argv[2:]:
        rows = list(csv.reader(open(d / f"{label}.tsv"), delimiter="\t"))
        bad = [r for r in rows if r[3] != "200"]
        ok = [r for r in rows if r[3] == "200"]
        wall = [float(r[4]) for r in ok]
        read = [float(r[6]) for r in ok if r[6]]
        mb = sum(int(r[5]) for r in ok) / 1e6
        per_req = [int(r[5]) / 1e6 / (float(r[4]) / 1000) for r in ok]
        print(f"{label} (conc {rows[0][1]}): n={len(ok)} errors={len(bad)}  "
              f"wall median {statistics.median(wall):.1f} ms p95 {pct(wall, 95):.1f} ms | "
              f"server-read median {statistics.median(read):.1f} ms p95 {pct(read, 95):.1f} ms | "
              f"per-request {statistics.median(per_req):.1f} MB/s | {mb:.0f} MB total")


if __name__ == "__main__":
    main()
