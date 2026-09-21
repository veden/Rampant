#!/usr/bin/env python3
"""Run a Factorio benchmark and report per-bucket highs/lows.

This is the whole-system measurement the H8 work needs:

  * the engine's per-tick CSV columns give wholeUpdate / gameUpdate /
    scriptUpdate / engineEstimate per tick;
  * the mod's in-game [SUB] samples give the mod subsystem breakdown;
  * the mod's [RANGE] samples give the coarse pick/dirty/ai phase timing.

The script runs Factorio itself, captures stdout, buckets the run into
1000-tick (configurable) ranges, and prints min/avg/max/p95 per bucket so a
single aggregate cannot hide a spike or a phase shift.

Usage:
    tools/benchmark-breakdown.py \
        --save /path/to/save.zip \
        --mod-directory /path/to/mods \
        --config /path/to/config.ini \
        --ticks 12000 --bucket 1000

    tools/benchmark-breakdown.py --save save.zip --label H8 --mod-directory mods

If --output is given, the raw Factorio output is kept there.
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
from collections import defaultdict

DEFAULT_FACTORIO = "/mnt/gallery/gameFiles/factorio/bin/x64/factorio"
DEFAULT_COLUMNS = ("wholeUpdate,engineEstimate,gameUpdate,scriptUpdate,"
                   "entityUpdate,particleUpdate,mapGenerator")
SUB = re.compile(r"\[SUB\] tick=(\d+) name=([\w.]+) Elapsed:\s*([\d.]+)ms")
RANGE = re.compile(r"\[RANGE\] tick=(\d+) phase=(\w+).*?Elapsed:\s*([\d.]+)ms")


def percentile(values, pct):
    if not values:
        return 0.0
    ordered = sorted(values)
    if len(ordered) == 1:
        return ordered[0]
    k = (len(ordered) - 1) * (pct / 100.0)
    lo = int(k)
    hi = min(lo + 1, len(ordered) - 1)
    frac = k - lo
    return ordered[lo] * (1.0 - frac) + ordered[hi] * frac


def parse_cli_rows(path):
    """Return (header, rows) where rows is [(tick, [values...])] in ms."""
    header = None
    rows = []
    with open(path, "r", errors="replace") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            parts = [p.strip() for p in line.split(",")]
            if len(parts) < 2:
                continue
            if parts[0] == "tick":
                header = [p for p in parts[1:] if p != ""]
                continue
            if header is None or not parts[0].startswith("t"):
                continue
            if not parts[0][1:].isdigit():
                continue
            values = []
            ok = True
            for raw in parts[1 : 1 + len(header)]:
                if raw == "":
                    values.append(0.0)
                    continue
                try:
                    values.append(float(raw))
                except ValueError:
                    ok = False
                    break
            if not ok or len(values) != len(header):
                continue
            rows.append((int(parts[0][1:]), [v / 1e6 for v in values]))
    return header, rows


def parse_samples(path):
    subs = []
    ranges = []
    with open(path, "r", errors="replace") as f:
        for line in f:
            m = SUB.search(line)
            if m:
                subs.append((int(m.group(1)), m.group(2), float(m.group(3))))
                continue
            m = RANGE.search(line)
            if m:
                ranges.append((int(m.group(1)), m.group(2), float(m.group(3))))
    return subs, ranges


def bucketize(rows, size):
    buckets = defaultdict(list)
    for tick, values in rows:
        buckets[tick // size].append((tick, values))
    return buckets


def bucketize_named(rows, size):
    buckets = defaultdict(list)
    for tick, name, ms in rows:
        buckets[tick // size].append((name, ms))
    return buckets


def print_cli_table(header, rows, bucket, label, wanted):
    if not wanted:
        wanted = header
    print()
    print(f"# {label}: per-tick CLI columns (ms)")
    print("range".ljust(22) + "".join(c.rjust(18) for c in wanted))
    for b in sorted(bucketize(rows, bucket)):
        entries = bucketize(rows, bucket)[b]
        lo = b * bucket
        hi = lo + bucket - 1
        cells = []
        for name in wanted:
            if name not in header:
                cells.append("".rjust(18))
                continue
            i = header.index(name)
            vals = [v[i] for _, v in entries]
            p95 = percentile(vals, 95)
            cells.append(f"{min(vals):5.2f}/{sum(vals)/len(vals):5.2f}/{max(vals):5.2f}/{p95:5.2f}")
        print(f"{lo}-{hi}".ljust(22) + "".join(c.rjust(18) for c in cells))
    print("# cells are min/avg/max/p95 within the range")


def print_named_table(rows, bucket, label):
    by_bucket = bucketize_named(rows, bucket)
    names = sorted({name for entries in by_bucket.values() for name, _ in entries})
    if not names:
        return
    print()
    print(f"# {label}: sampled subsystem values (ms)")
    print("range".ljust(22) + "".join(n.rjust(13) for n in names))
    for b in sorted(by_bucket):
        entries = by_bucket[b]
        lo = b * bucket
        hi = lo + bucket - 1
        cells = []
        for name in names:
            vals = [ms for n, ms in entries if n == name]
            if not vals:
                cells.append("".rjust(13))
            else:
                cells.append(f"{min(vals):6.3f}/{sum(vals)/len(vals):6.3f}/{max(vals):6.3f}")
        print(f"{lo}-{hi}".ljust(22) + "".join(c.rjust(13) for c in cells))
    print("# cells are min/avg/max within the range")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--save", required=True)
    ap.add_argument("--factorio", default=DEFAULT_FACTORIO)
    ap.add_argument("--mod-directory", default="")
    ap.add_argument("--config", default="")
    ap.add_argument("--ticks", type=int, default=12000)
    ap.add_argument("--bucket", type=int, default=1000)
    ap.add_argument("--label", default="run")
    ap.add_argument("--output", default="")
    ap.add_argument("--columns", default=DEFAULT_COLUMNS)
    ap.add_argument("--verbose", default="all",
                    help='value for --benchmark-verbose (default: all)')
    ap.add_argument("--subsystem-telemetry", action="store_true",
                    help="temporarily enable exact per-call [SUB] logging in the mod copy")
    args = ap.parse_args()

    if not os.path.isfile(args.factorio):
        print(f"factorio not found: {args.factorio}", file=sys.stderr)
        return 2
    if not os.path.isfile(args.save):
        print(f"save not found: {args.save}", file=sys.stderr)
        return 2

    output = args.output
    tmp_output = None
    if not output:
        fd, output = tempfile.mkstemp(prefix="rampant-breakdown-", suffix=".out")
        os.close(fd)
        tmp_output = output

    cmd = [
        args.factorio,
        "--benchmark", args.save,
        "--benchmark-ticks", str(args.ticks),
        "--benchmark-verbose", args.verbose,
    ]
    if args.config:
        cmd[1:1] = ["-c", args.config]
    if args.mod_directory:
        cmd[1:1] = ["--mod-directory", args.mod_directory]

    settings_path = None
    settings_backup = None
    settings_file = os.path.join(args.mod_directory, "Rampant", "settings.lua")
    if args.subsystem_telemetry and os.path.isfile(settings_file):
        settings_path = settings_file
        with open(settings_path, "r", errors="replace") as f:
            settings_backup = f.read()
        patched = settings_backup
        patched = patched.replace(
            'name = "rampant--subsystemTelemetry",\n            description = "Log every timed subsystem call for benchmark breakdowns (high overhead).",\n            setting_type = "startup",\n            default_value = false,',
            'name = "rampant--subsystemTelemetry",\n            description = "Log every timed subsystem call for benchmark breakdowns (high overhead).",\n            setting_type = "startup",\n            default_value = true,')
        if patched == settings_backup:
            print("# warning: subsystem telemetry setting not found in mod copy", file=sys.stderr)
        else:
            with open(settings_path, "w", errors="replace") as f:
                f.write(patched)
            print("# exact subsystem telemetry enabled in temp mod copy")
    if args.mod_directory:
        for stale in ("mod-settings.dat", os.path.join("mods", "mod-settings.dat")):
            try:
                os.unlink(os.path.join(args.mod_directory, stale))
            except OSError:
                pass

    print(f"# running: {' '.join(cmd)}")
    print(f"# raw output: {output}")
    try:
        with open(output, "w", errors="replace") as f:
            proc = subprocess.run(cmd, stdout=f, stderr=subprocess.STDOUT)
        print(f"# factorio exit: {proc.returncode} (SIGSEGV at teardown is expected for long verbose runs)")
    finally:
        if settings_path and settings_backup is not None:
            with open(settings_path, "w", errors="replace") as f:
                f.write(settings_backup)
            print("# temp mod copy settings.lua restored")

    header, rows = parse_cli_rows(output)
    subs, ranges = parse_samples(output)
    if not header or not rows:
        print("no per-tick CSV found; check --benchmark-verbose and the save", file=sys.stderr)
        return 1

    wanted = [c for c in args.columns.split(",") if c]
    if "engineEstimate" in wanted and "wholeUpdate" in header and "scriptUpdate" in header:
        wi = header.index("wholeUpdate")
        si = header.index("scriptUpdate")
        for _, values in rows:
            values.append(values[wi] - values[si])
        header = header + ["engineEstimate"]

    print()
    print(f"# label={args.label} ticks={len(rows)} bucket={args.bucket}")
    print_cli_table(header, rows, args.bucket, args.label, wanted)
    print_named_table([(t, n, v) for t, n, v in subs], args.bucket, "SUB")
    print_named_table([(t, n, v) for t, n, v in ranges], args.bucket, "RANGE")

    if tmp_output and not args.output:
        try:
            os.unlink(tmp_output)
        except OSError:
            pass
    return 0


if __name__ == "__main__":
    sys.exit(main())
