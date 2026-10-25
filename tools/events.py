#!/usr/bin/env python3
"""Summarise a Rampant event-telemetry stream.

The mod writes one line per event to script-output/rampant/events.jsonl when the
startup setting `rampant--eventTelemetry` is on:

    t=331|cmd.CREATE_BASE|chunk=147|name=neutral-biter-spawner-v1-t1-rampant|id=6

This reads that file and answers the questions a log is usually read for: what
kinds of events happened, how many per game-minute, which base/field/entity was
involved, and what a specific window looked like.

usage:
  events.py FILE                     summary by kind and per 5-minute bucket
  events.py FILE --kind cmd.DISPATCH_SQUAD
  events.py FILE --tick 1000 5000    raw events in a tick window
  events.py FILE --grep base=4
"""

import argparse
import collections
import sys


def parse(path):
    for line in open(path, errors="replace"):
        line = line.strip()
        if not line or "|" not in line:
            continue
        parts = line.split("|")
        tick = 0
        if parts[0].startswith("t="):
            try:
                tick = int(parts[0][2:])
            except ValueError:
                tick = 0
            kind = parts[1]
        else:
            kind = parts[0]
        fields = {}
        for p in parts[2:] if parts[0].startswith("t=") else parts[1:]:
            if "=" in p:
                k, v = p.split("=", 1)
                fields[k] = v
        yield tick, kind, fields


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("file")
    ap.add_argument("--kind", help="only this event kind (exact)")
    ap.add_argument("--grep", help="only lines containing this literal text")
    ap.add_argument("--tick", nargs=2, type=int, metavar=("FROM", "TO"))
    ap.add_argument("--bucket", type=int, default=18000,
                    help="bucket size in ticks for the per-bucket table (default 5 game-minutes)")
    args = ap.parse_args()

    counts = collections.Counter()
    buckets = collections.defaultdict(collections.Counter)
    first_tick = None
    last_tick = 0
    printed = 0

    for tick, kind, fields in parse(args.file):
        if args.kind and kind != args.kind:
            continue
        if args.tick and not (args.tick[0] <= tick <= args.tick[1]):
            continue
        if args.grep and args.grep not in (kind + "|" + "|".join(f"{k}={v}" for k, v in fields.items())):
            continue
        counts[kind] += 1
        buckets[tick // args.bucket][kind] += 1
        if first_tick is None:
            first_tick = tick
        last_tick = max(last_tick, tick)
        if args.kind or args.grep or args.tick:
            printed += 1
            if printed <= 200:
                kv = " ".join(f"{k}={v}" for k, v in sorted(fields.items()))
                print(f"{tick:>9}  {kind:<28} {kv}")

    if args.kind or args.grep or args.tick:
        print(f"\n{printed} matching events"
              + (f" (showing first 200)" if printed > 200 else ""))
        return

    if not counts:
        print("no events", file=sys.stderr)
        return 1

    # 60 ticks per second, 60 seconds per minute: ticks/3600 is game-MINUTES.
    span_minutes = (last_tick - first_tick) / 60 / 60
    print(f"events {first_tick}..{last_tick} ticks ({span_minutes:.1f} game-minutes)\n")
    print(f"{'kind':<30}{'count':>8}")
    for kind, n in counts.most_common():
        print(f"{kind:<30}{n:>8}")

    print("\nper bucket (game-minutes):")
    kinds = [k for k, _ in counts.most_common(8)]
    header = "bucket   " + "".join(f"{k.split('.')[-1][:11]:>13}" for k in kinds)
    print(header)
    for b in sorted(buckets):
        row = f"{(b * args.bucket) // 3600:>4}m  "
        row += "".join(f"{buckets[b].get(k, 0):>13}" for k in kinds)
        print(row)
    return 0


if __name__ == "__main__":
    sys.exit(main())
