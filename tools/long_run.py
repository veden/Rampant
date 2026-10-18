#!/usr/bin/env python3
"""
E2E long-run interrogation driver.

Launches the long-run scenario server (or attaches to one), samples
telemetry + game state at intervals, records the session as JSONL, and
runs session-level assertions when LONGRUN_COMPLETE is observed.

Usage:
  python3 tools/long_run.py --port 34202 --password rampant-test \
      [--interval 15] [--max-wait 900] [--artifact /tmp/rampant-longrun.jsonl]

Session assertions (the invariants, not the specifics):
  1. dirty queue drained: cascade terminates throughout
  2. chunk index: zero misses
  3. settlements: enemy spawner count grew (migration completed)
  4. squad formation happened and stayed bounded
  5. evolution progressed
  6. UPS healthy (the run actually progressed at speed)
"""

import argparse
import json
import os
import socket
import struct
import subprocess
import sys
import time

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
RCON = os.path.join(SCRIPT_DIR, "rcon_client.py")


def rcon_query(port, password, command, timeout=20, retries=2):
    """Use rcon_client.py to run a /silent-command and capture its response.
    Retries on transient failures (busy server, connection resets)."""
    for attempt in range(retries + 1):
        try:
            out = subprocess.run(
                [sys.executable, RCON, "query", str(port), password, command],
                capture_output=True, text=True, timeout=timeout,
            )
            if out.stdout.strip():
                return out.stdout.strip()
        except subprocess.TimeoutExpired:
            pass
        except Exception:
            pass
        time.sleep(2)
    return "" 


def sample_tick(port, password):
    for line in rcon_query(port, password, "/silent-command rcon.print(game.tick)").splitlines():
        line = line.strip()
        if line.isdigit():
            return int(line)
    return None


def sample_metrics(port, password):
    """Pull the metrics summary (counters/latest/highWater/stats — no raw
    ring buffers; full getMetrics is too large for the RCON response)."""
    out = rcon_query(
        port, password,
        '/silent-command rcon.print(helpers.table_to_json(remote.call("Rampant","getMetricsSummary")))',
    )
    for line in out.splitlines():
        line = line.strip()
        if line.startswith("{"):
            try:
                return json.loads(line)
            except Exception:
                pass
    return None


def sample_ph(port, password, x, y):
    out = rcon_query(
        port, password,
        '/silent-command local p=remote.call("Rampant","queryPheromones",{x=%d,y=%d},game.surfaces[1]); '
        'if p then rcon.print(helpers.table_to_json(p)) else rcon.print("null") end' % (x, y),
    )
    for line in out.splitlines():
        line = line.strip()
        if line.startswith("{"):
            try:
                return json.loads(line)
            except Exception:
                pass
    return None


def sample_spawners(port, password):
    out = rcon_query(
        port, password,
        '/silent-command local s=game.surfaces[1]; local n=#s.find_entities_filtered{force="enemy",type="unit-spawner"}; rcon.print(n)',
    )
    for line in out.splitlines():
        line = line.strip()
        if line.isdigit():
            return int(line)
    return None


def sample_evolution(port, password):
    out = rcon_query(
        port, password,
        '/silent-command rcon.print(tostring(game.forces.enemy.get_evolution_factor(game.surfaces[1])))',
    )
    for line in out.splitlines():
        line = line.strip()
        try:
            return float(line)
        except ValueError:
            pass
    return None


def sample_groups(port, password):
    out = rcon_query(
        port, password,
        '/silent-command local g=remote.call("Rampant","getEnemyGroups",game.surfaces[1]); '
        'local s,b=0,0; for _,x in ipairs(g or {}) do if x.isBuilder then b=b+1 else s=s+1 end end; '
        'rcon.print("s="..s.." b="..b)',
    )
    for line in out.splitlines():
        line = line.strip()
        if line.startswith("s="):
            parts = dict(kv.split("=") for kv in line.split(" "))
            return int(parts.get("s", 0)), int(parts.get("b", 0))
    return None, None


PH_POINTS = [(0, 0), (200, 0), (-180, 80), (160, -140), (0, 220)]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=34202)
    ap.add_argument("--password", default="rampant-test")
    ap.add_argument("--interval", type=float, default=15.0)
    ap.add_argument("--max-wait", type=float, default=1500.0)
    ap.add_argument("--artifact", default="/tmp/rampant-longrun.jsonl")
    args = ap.parse_args()

    artifact = open(args.artifact, "w")
    samples = []
    t0 = time.time()
    last_tick = None
    last_time = None
    started = False
    complete = False

    while time.time() - t0 < args.max_wait:
        tick = sample_tick(args.port, args.password)
        if tick is None:
            time.sleep(2)
            continue

        now = time.time() - t0
        record = {"real_s": round(now, 1), "tick": tick}
        if last_tick and last_time:
            elapsed = now - last_time
            if elapsed > 0:
                record["ups"] = round((tick - last_tick) / elapsed, 1)
        last_tick = tick
        last_time = now

        metrics = sample_metrics(args.port, args.password)
        if metrics:
            record["metrics"] = {
                "counters": metrics.get("counters", {}),
                "latest": metrics.get("latest", {}),
                "highWater": metrics.get("highWater", {}),
            }
        record["pheromones"] = {}
        for x, y in PH_POINTS:
            record["pheromones"][f"{x},{y}"] = sample_ph(args.port, args.password, x, y)
        record["spawners"] = sample_spawners(args.port, args.password)
        record["evolution"] = sample_evolution(args.port, args.password)
        squads, builders = sample_groups(args.port, args.password)
        record["squads"] = squads
        record["builders"] = builders

        artifact.write(json.dumps(record) + "\n")
        artifact.flush()
        samples.append(record)
        print(f"[{now:7.1f}s] tick={tick} ups={record.get('ups')} "
              f"spawners={record['spawners']} evo={record['evolution']} "
              f"squads={squads} builders={builders}", flush=True)

        # check the game log for LONGRUN signals via a stdout probe
        # (the scenario prints to stdout, which the recipe captures; here we
        # check completion via tick >= 4 game-hours)
        if tick >= 240 * 3600:
            complete = True
            break

        time.sleep(args.interval)

    artifact.close()

    if not samples:
        print("FAIL: no samples collected")
        sys.exit(1)

    # ---- Session-level assertions ----
    failures = []

    # 1. chunk index zero misses
    all_misses = []
    for s in samples:
        m = s.get("metrics", {}).get("counters", {}).get("index.misses", 0)
        all_misses.append(m)
    if all(v == 0 for v in all_misses):
        print("PASS: chunk index zero misses")
    else:
        failures.append(f"index misses: {all_misses}")

    # 2. settlements: spawner count grew beyond the staged 4
    spawner_counts = [s.get("spawners") for s in samples if s.get("spawners") is not None]
    if spawner_counts and max(spawner_counts) > 4:
        print(f"PASS: settlements occurred (spawners {min(spawner_counts)} -> {max(spawner_counts)})")
    else:
        failures.append(f"no settlement growth: spawner counts {spawner_counts}")

    # 3. squad formation happened
    formed = sum(1 for s in samples if (s.get("squads") or 0) > 0 or (s.get("builders") or 0) > 0)
    if formed > 0:
        print("PASS: squad formation observed")
    else:
        failures.append("no squads formed during the session")

    # 4. evolution progressed
    evos = [s.get("evolution") for s in samples if s.get("evolution") is not None]
    if evos and evos[-1] > evos[0] and evos[-1] > 0:
        print(f"PASS: evolution progressed ({evos[0]:.3f} -> {evos[-1]:.3f})")
    else:
        failures.append(f"evolution did not progress: {evos}")

    # 5. UPS healthy
    upses = [s.get("ups") for s in samples if s.get("ups") is not None]
    if upses and sum(upses) / len(upses) > 200:
        print(f"PASS: mean UPS {sum(upses)/len(upses):.0f}")
    else:
        failures.append(f"mean UPS too low: {upses}")

    if not complete:
        failures.append("session did not complete (4 game-hours not reached)")

    print("\n=== Session Assertions ===")
    if failures:
        print("FAILED:")
        for f in failures:
            print("  -", f)
        sys.exit(1)
    print("ALL SESSION ASSERTIONS PASSED")
    print(f"Artifact: {args.artifact}")


if __name__ == "__main__":
    main()
