#!/usr/bin/env python3
"""Measure the chunk pipeline: the scan queue (pendingChunks) and the surface release.

Both are per-chunk engine queries over a surface, both used to be unbounded in one tick,
and both are sliced by a COUNT because the control stage has no readable clock (profilers
are opaque, os is not exposed). This measures whether the slices hold:

  * per-chunk scan cost ([SCAN] lines: chunk coords + cost)
  * load flush: per-tick scan cost (sum of [SCAN] in the tick) and how long it lasts
  * surface release: [REL] lines, chunk totals, dropped-scan totals, tick cost nearby
  * steady state after the flush: [TICK] ranges

No diagnose window: per-tick logging roughly doubles the tick it measures, so a diagnose
window is for attributing a phase, not for gameplay numbers.

Usage: tools/measure-chunk-pipeline.py     (copies a save; see tools/save_drive.py)
"""
import collections
import re
import time

import os
import sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import save_drive as d

run = d.Run()
print("work=%s" % run.work, flush=True)
if not run.wait_ready(300):
    print(open(run.log).read()[-2000:], flush=True)
    run.close()
    raise SystemExit(1)
print("ready tick=%s" % d.tick(), flush=True)
print("queue at ready: %s" % d.call("chunkQueueStatus"), flush=True)
for i in range(8):
    time.sleep(10)
    print("  t+%02ds %s" % ((i + 1) * 10, d.call("chunkQueueStatus")), flush=True)
print("fields: resource=%s" % d.call("verifyField", "resource", 0.10), flush=True)
print("fields: enemy=%s" % d.call("verifyField", "enemy", 0.10), flush=True)
run.close()

lines = open(run.log, errors="replace").read().split("\n")
tick_pat = re.compile(r"\[TICK\] tick=(\d+) Elapsed: ([\d.]+)ms")
scan_pat = re.compile(r"\[SCAN\] tick=(\d+) chunk=(-?\d+),(-?\d+) Elapsed: ([\d.]+)ms")
rel_pat = re.compile(r"\[REL\] surface=(\S+) chunks=(\d+) queuedScans=(\d+) tick=(\d+)")
sub_pat = re.compile(r"\[SUB\] tick=(\d+) name=(\S+).*?Elapsed: ([\d.]+)ms")

ticks = [(int(m.group(1)), float(m.group(2))) for m in (tick_pat.search(l) for l in lines) if m]
scan_sum, scan_n = collections.defaultdict(float), collections.Counter()
per_chunk = []
for l in lines:
    m = scan_pat.search(l)
    if m:
        t = int(m.group(1))
        scan_sum[t] += float(m.group(4))
        scan_n[t] += 1
        per_chunk.append(float(m.group(4)))
rels = [(m.group(1), int(m.group(2)), int(m.group(4))) for m in (rel_pat.search(l) for l in lines) if m]
subs = collections.defaultdict(list)
for l in lines:
    m = sub_pat.search(l)
    if m:
        subs[m.group(2)].append((int(m.group(1)), float(m.group(3))))

print("\n--- per-chunk scan cost ---", flush=True)
if per_chunk:
    v = sorted(per_chunk)
    n = len(v)
    q = lambda p: v[min(int(n * p), n - 1)]
    print("scanned=%d p50=%.3f p90=%.3f p99=%.3f max=%.3f over1ms=%d" % (
        n, q(.5), q(.9), q(.99), v[-1], sum(1 for x in v if x > 1)))

print("\n--- load flush per tick ---", flush=True)
if scan_sum:
    ft = sorted(scan_sum)
    per = [scan_sum[t] for t in ft]
    v = sorted(per)
    n = len(v)
    q = lambda p: v[min(int(n * p), n - 1)]
    print("flush ticks=%d  per-tick scan cost: p50=%.2f p90=%.2f p99=%.2f max=%.2f over2ms=%d over4ms=%d over8ms=%d" % (
        len(ft), q(.5), q(.9), q(.99), v[-1],
        sum(1 for x in per if x > 2), sum(1 for x in per if x > 4), sum(1 for x in per if x > 8)))
    print("chunks per flush tick: p50=%d max=%d" % (
        sorted(scan_n[t] for t in ft)[len(ft) // 2], max(scan_n[t] for t in ft)))
    print("flush window: ticks %d..%d (= %.1f s of game time at 60 UPS)" % (
        ft[0], ft[-1], (ft[-1] - ft[0]) / 60.0))

print("\n--- [REL] surface releases ---", flush=True)
for name, chunks, t in rels:
    print("  surface=%s chunks=%d tick=%d" % (name, chunks, t))

print("\n--- [TICK] around the release ticks ---", flush=True)
if rels and ticks:
    around = set()
    for _, _, t in rels:
        around.update(range(t - 2, t + 3))
    for t, ms in sorted(ticks):
        if t in around:
            print("  tick=%d %.2fms scans=%d scan_ms=%.2f" % (t, ms, scan_n.get(t, 0), scan_sum.get(t, 0.0)))

print("\n--- [TICK] ranges after the flush, in equal buckets ---", flush=True)
# The first minutes after a load are still the load aftermath (the map sheds records, the
# fields reseed), and they dominate the tail: reporting one number for "after the flush"
# mixes that with steady state, and bucketing shows the decay in a single run.
if ticks and scan_sum:
    start = max(scan_sum)
    later = [(t, ms) for t, ms in ticks if t > start]
    if later:
        lo, hi = later[0][0], later[-1][0]
        span = max(1, (hi - lo + 1) // 4)
        for b in range(4):
            blo, bhi = lo + b * span, lo + (b + 1) * span
            v = sorted(ms for t, ms in later if blo <= t < bhi)
            if not v:
                continue
            n = len(v)
            q = lambda p: v[min(int(n * p), n - 1)]
            print("bucket %d (tick %d..%d, %4.1f s in) n=%3d p50=%.2f p90=%.2f p99=%.2f "
                  "max=%.2f over5ms=%d" % (
                      b + 1, blo, bhi, (blo - start) / 60.0, n, q(.5), q(.9), q(.99), v[-1],
                      sum(1 for x in v if x > 5)))

print("\n--- [SUB] surfaceRelease / surfaceSweep / pendingChunks ---", flush=True)
for name in ("surfaceRelease", "surfaceSweep", "pendingChunks"):
    v = sorted(x for _, x in subs.get(name, []))
    if v:
        n = len(v)
        print("  %-16s n=%4d p50=%.3f p90=%.3f max=%.3f" % (name, n, v[n // 2], v[int(n * .9)], v[-1]))
    else:
        print("  %-16s (no samples - telemetry off for that subsystem)" % name)
