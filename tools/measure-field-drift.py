#!/usr/bin/env python3
"""Measure field correctness against the oracle over time, through play and a rebuild.

The field is compared with a fresh exact solve every 15 s. A single comparison cannot tell
a field that is wrong from one that is healing, and a rebuild that reads 0 mismatches says
nothing about the next minute - which is how the drift that mattered stayed hidden:

  play, sampled              does the live field track an exact solve as the map changes
  rebuild, then sampled      a rebuild is exact by construction; the samples after it say
                             whether the event path keeps it exact

Usage: tools/measure-field-drift.py      (copies a save; see tools/save_drive.py)
"""
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import save_drive as d


def brief(name, field=""):
    if field:
        v = d.call("verifyField", field, 0.10)
        if isinstance(v, dict):
            return "%s mism=%s liveOnly=%s maxDelta=%s frontier=%s" % (
                name, v.get("mismatches"), v.get("liveOnly"), v.get("maxDelta"),
                v.get("frontier"))
        return "%s %s" % (name, v)
    return name


run = d.Run()
print("work=%s" % run.work, flush=True)
if not run.wait_ready(300):
    print(open(run.log).read()[-3000:], flush=True)
    run.close()
    raise SystemExit(1)
print("ready at tick %s" % d.tick(), flush=True)

print("\n--- unmodified play, sampled ---", flush=True)
for i in range(8):
    time.sleep(15)
    print("t+%03ds tick=%s | %s | %s" % ((i + 1) * 15, d.tick(),
        brief("resource", "resource"), brief("enemy", "enemy")), flush=True)

print("\n--- rebuild resource, then sampled play ---", flush=True)
print("rebuild sources=%s" % d.call("rebuildField", "resource"), flush=True)
for i in range(8):
    time.sleep(15)
    print("t+%03ds tick=%s | %s | %s | %s" % ((i + 1) * 15, d.tick(),
        brief("resource", "resource"), brief("enemy", "enemy"), brief("base", "base")), flush=True)
print("load %s" % d.call("fieldLoad"), flush=True)
print("support resource %s" % d.call("fieldSupport", "resource", 3), flush=True)

run.close()
