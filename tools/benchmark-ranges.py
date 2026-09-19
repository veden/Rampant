#!/usr/bin/env python3
"""Aggregate Factorio --benchmark-verbose per-tick CSV into tick ranges.

Factorio's benchmark prints a single aggregate (avg/min/max) per run. To see how
cost evolves across a run, run with per-tick timings:

    factorio --benchmark SAVE --benchmark-ticks N \
        --benchmark-verbose "wholeUpdate,gameUpdate,entityUpdate,particleUpdate,mapGenerator,scriptUpdate"

which writes a CSV: a header line `tick,wholeUpdate,...` then one line per tick,
values in nanoseconds. This script buckets the ticks into ranges and reports
avg/min/max per column per bucket, so a 50,000-tick run yields e.g. ten 5,000-tick
ranges with a number each instead of one number for the whole run.

Usage:
    benchmark-ranges.py RUN.out [--bucket 5000] [--skip-first N] [--columns a,b]
"""

import argparse
import sys


def parse(path, skip_first):
    header = None
    rows = []
    with open(path, "r", errors="replace") as f:
        for line in f:
            line = line.strip()
            if not line.startswith("t") or "," not in line:
                continue
            parts = line.split(",")
            if not parts[0][1:].isdigit():
                # header line: tick,wholeUpdate,...
                header = [p.strip() for p in parts if p.strip()]
                continue
            if header is None:
                continue
            try:
                values = [float(p) for p in parts[1 : 1 + len(header) - 1]]
            except ValueError:
                continue
            if len(values) != len(header) - 1:
                continue
            tick = int(parts[0][1:])
            if tick < skip_first:
                continue
            rows.append((tick, values))
    return header, rows


def bucketize(rows, size):
    buckets = {}
    for tick, values in rows:
        b = tick // size
        buckets.setdefault(b, []).append((tick, values))
    return buckets


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("path")
    ap.add_argument("--bucket", type=int, default=5000)
    ap.add_argument("--skip-first", type=int, default=1,
                    help="skip the load/init tick (default 1)")
    ap.add_argument("--columns", default="",
                    help="comma-separated column names to report (default: all)")
    args = ap.parse_args()

    header, rows = parse(args.path, args.skip_first)
    if not header or not rows:
        print("no per-tick data found (did the run complete with --benchmark-verbose?)")
        return 1

    cols = header[1:]
    wanted = [c for c in args.columns.split(",") if c] or cols
    idx = {c: i for i, c in enumerate(cols)}

    print(f"# {len(rows)} ticks, {len(cols)} columns: {','.join(cols)}")
    print(f"# bucket={args.bucket} ticks, values in milliseconds")
    print()
    print("range".ljust(20) + "".join(c.rjust(12) for c in wanted))
    for b in sorted(bucketize(rows, args.bucket)):
        entries = bucketize(rows, args.bucket)[b]
        lo = b * args.bucket
        hi = lo + args.bucket - 1
        out = []
        for c in wanted:
            vals = [v[idx[c]] / 1e6 for _, v in entries]
            out.append(f"{sum(vals) / len(vals):12.3f}")
        print(f"{lo}-{hi}".ljust(20) + "".join(out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
