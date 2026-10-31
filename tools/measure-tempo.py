"""Group-formation tempo on the reference save: dispatches per 10 game-minutes.

This is the A/B harness for the AI tempo work: it reads only committed behaviour
(squad.created), the attempts that produced it, deaths, and concurrency, so a
change either moves the number or it does not.

No code change: reads existing counters through the Rampant remote.
  squad.created        groups actually formed (the dispatch event)
  squad.formAttempt    visits that tried
  squad.destroyed      groups that died
  Universe.squadCount  groups alive right now (concurrency)
Reported per game-minute bucket so the number is "dispatches per 10 game-minutes".
"""
import os, sys, time
sys.path.insert(0, "os.path.dirname(os.path.abspath(__file__))")
import save_drive as d

KEYS = ("squad.created", "squad.formAttempt", "squad.destroyed", "squad.formEmpty",
        "squad.formCooldownArmed", "squad.deploy.centroid", "squad.deploy.fallback")
PROBE = ('local m = remote.call("Rampant","getMetrics") '
         'local c = m.counters '
         'local u = storage.universe '
         'local n = 0 for _ in pairs(u.bases or {}) do n = n + 1 end '
         'return string.format("%d|%d|%d|%d|%d|%d|%d|%d|%s|%s", '
         'game.tick, c["squad.created"] or 0, c["squad.formAttempt"] or 0, '
         'c["squad.destroyed"] or 0, c["squad.formEmpty"] or 0, '
         'c["squad.formCooldownArmed"] or 0, c["squad.deploy.centroid"] or 0, '
         'c["squad.deploy.fallback"] or 0, tostring(u.squadCount or 0), tostring(n))')

run = d.Run()
print("work=%s" % run.work, flush=True)
if not run.wait_ready(300):
    print(open(run.log).read()[-1500:], flush=True); run.close(); raise SystemExit(1)
time.sleep(5)
rows = []
for i in range(40):
    raw = d.lua(PROBE, 30)
    try:
        rows.append([int(v) for v in str(raw).strip().split("|")])
    except Exception:
        print("probe failed: %r" % (raw,), flush=True); break
    time.sleep(15)
run.close()

if rows:
    t0 = rows[0][0]
    print("\n%-8s %-8s %-8s %-6s %-6s %-6s" % ("gameMin", "created", "attempt", "died", "live", "bases"))
    buckets = {}
    for r in rows:
        gm = int((r[0] - t0) / 3600.0)
        buckets.setdefault(gm, []).append(r)
    for gm in sorted(buckets):
        b = buckets[gm]
        first, last = b[0], b[-1]
        print("%-8d %-8d %-8d %-6d %-6s %-6s" % (
            gm, last[1]-first[1], last[2]-first[2], last[3]-first[3], last[8], last[9]))
    created = rows[-1][1] - rows[0][1]
    attempt = rows[-1][2] - rows[0][2]
    lived = [r[8] for r in rows]
    duration = (rows[-1][0] - rows[0][0]) / 3600.0
    print("\nwindow %.1f game-minutes: created=%d (%.2f/min) attempts=%d (accept %.1f%%) live p50=%d max=%d" % (
        duration, created, created / max(duration, 0.01), attempt,
        100.0 * created / max(attempt, 1), sorted(lived)[len(lived)//2], max(lived)))
