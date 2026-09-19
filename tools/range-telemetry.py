#!/usr/bin/env python3
"""Aggregate Rampant per-phase tick telemetry from factorio-current.log.

The mod logs (gated by rampant--telemetryEnabled) one line per RANGE_TICKS
ticks per phase:

    [RANGE] tick=12880000 phase=pick pick=0 Elapsed: 0.494942ms
    [RANGE] tick=12880000 phase=dirty Elapsed: 0.430092ms
    [RANGE] tick=12880000 phase=ai Elapsed: 0.389042ms

A completed --benchmark run therefore yields per-range, per-phase numbers, which
--benchmark-verbose cannot provide (it SIGSEGVs at teardown for long runs).

Usage: range-telemetry.py LOG [--bucket 5000]
"""

import argparse
import re
import sys
from collections import defaultdict

LINE = re.compile(r"\[RANGE\] tick=(\d+) phase=(\w+).*?Elapsed:\s*([\d.]+)ms")
COUNTER = re.compile(
    r"\[RANGE\] tick=(\d+) phase=dirty .*?"
    r"sweepMarks=(\d+) propagationMarks=(\d+) chunksProcessed=(\d+) epsilonSkips=(\d+)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("log")
    ap.add_argument("--bucket", type=int, default=5000)
    args = ap.parse_args()

    samples = defaultdict(lambda: defaultdict(list))
    counters = {}
    with open(args.log, "r", errors="replace") as f:
        for line in f:
            m = LINE.search(line)
            if m:
                tick, phase, ms = int(m.group(1)), m.group(2), float(m.group(3))
                samples[tick // args.bucket][phase].append(ms)
            c = COUNTER.search(line)
            if c:
                counters[int(c.group(1)) // args.bucket] = tuple(
                    int(c.group(i)) for i in (2, 3, 4, 5))

    if not samples:
        print("no [RANGE] telemetry found (is rampant--telemetryEnabled on?)")
        return 1

    phases = sorted({p for b in samples.values() for p in b})
    print(f"# bucket={args.bucket} ticks, per-phase script ms")
    print("range".ljust(20) + "".join(p.rjust(10) for p in phases) + "total".rjust(10))
    prev = None
    for b in sorted(samples):
        row = samples[b]
        cells = []
        total = 0.0
        for p in phases:
            vals = row.get(p)
            v = sum(vals) / len(vals) if vals else 0.0
            total += v
            cells.append(f"{v:10.3f}")
        lo = b * args.bucket
        extra = ""
        if b in counters:
            cur = counters[b]
            if prev is not None:
                d = [cur[i] - prev[i] for i in range(4)]
                extra = ("  sweep=%d prop=%d chunks=%d skip=%d" % tuple(d))
            prev = cur
        print(f"{lo}-{lo + args.bucket - 1}".ljust(20) + "".join(cells) + f"{total:10.3f}" + extra)
    return 0


if __name__ == "__main__":
    sys.exit(main())
