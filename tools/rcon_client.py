#!/usr/bin/env python3
"""
RCON client for driving the Factorio headless test server.

Two subcommands:
  query <port> <password> '<silent-command>'   Send a command, print responses
  ups   <port> <password> [duration] [interval] Sample game.tick and print a
                                                UPS curve (ticks/sec)

Notes:
- Factorio RCON is the Source RCON binary protocol; plain `nc` text does not work.
- For data back from the game, use `rcon.print(...)` inside the command —
  `print()` goes to game stdout, NOT the RCON response.
- `/silent-command` evaluates in the SCENARIO context: `storage` there is the
  scenario's storage, not the mod's (see docs/09-Engine-Testing.md).

Example:
  python3 tools/rcon_client.py query 27018 test '/silent-command rcon.print(game.tick)'
  python3 tools/rcon_client.py ups 27018 test 20 2
"""

import json as jsonlib
import socket
import struct
import sys
import time


def rcon(port, command, password, timeout=4):
    s = socket.create_connection(("127.0.0.1", port), timeout=timeout)

    def send(pkt_type, body):
        payload = struct.pack("<ii", 1, pkt_type) + body + b"\x00\x00"
        s.sendall(struct.pack("<i", len(payload)) + payload)

    def drain():
        s.settimeout(5.0)
        out = b""
        while True:
            try:
                hdr = s.recv(4)
            except socket.timeout:
                break
            if len(hdr) < 4:
                break
            ln = struct.unpack("<i", hdr)[0]
            data = b""
            while len(data) < ln:
                chunk = s.recv(ln - len(data))
                if not chunk:
                    break
                data += chunk
            out += data
        return out

    send(3, password.encode())  # auth
    drain()
    send(2, command.encode())   # exec
    resp = drain()
    s.close()

    bodies = []
    off = 0
    while off + 12 <= len(resp):
        rest = resp[off + 8:]
        if rest.endswith(b"\x00\x00"):
            bodies.append(rest[:-2].decode(errors="replace"))
        else:
            bodies.append(rest.decode(errors="replace"))
        off += 8 + len(rest)
    return bodies


def query_tick(port, password):
    for body in rcon(port, "/silent-command rcon.print(game.tick)", password):
        t = body.strip()
        if t.isdigit():
            return int(t)
    return None


def cmd_query(port, password, command):
    for body in rcon(port, command, password):
        print(body)


def cmd_metrics(port, password):
    """Pull Rampant telemetry (counters, latest, high-water, sample stats)."""
    cmd = '/silent-command rcon.print(helpers.table_to_json(remote.call("Rampant","getMetrics")))'
    for body in rcon(port, cmd, password):
        body = body.strip()
        if not body:
            continue
        try:
            data = jsonlib.loads(body)
        except Exception:
            print(body)
            continue
        print("enabled:", data.get("enabled"))
        if not data.get("enabled"):
            return
        for section in ("counters", "latest", "highWater"):
            print(f"-- {section} --")
            for k in sorted(data.get(section, {})):
                print(f"  {k}: {data[section][k]}")
        print("-- sample stats (count/mean/max) --")
        for k in sorted(data.get("samples", {})):
            s = data["samples"][k]
            if isinstance(s, list) and len(s) > 0:
                mean = sum(s) / len(s)
                print(f"  {k}: n={len(s)} mean={mean:.3f} max={max(s):.3f}")


def cmd_ups(port, password, duration, interval):
    t0 = time.time()
    last = None
    while time.time() - t0 < duration:
        t = query_tick(port, password)
        now = time.time() - t0
        if t is not None and last:
            dt = now - last[0]
            if dt > 0.5:
                ups = (t - last[1]) / dt
                print(f"t={now:6.1f}s tick={t:8d} ups={ups:8.3f}", flush=True)
        if t is not None:
            last = (now, t)
        time.sleep(interval)


def main():
    args = sys.argv[1:]
    if len(args) < 3:
        print(__doc__)
        sys.exit(1)
    cmd, port, password = args[0], int(args[1]), args[2]
    if cmd == "query":
        if len(args) < 4:
            print(__doc__)
            sys.exit(1)
        cmd_query(port, password, args[3])
    elif cmd == "ups":
        duration = float(args[3]) if len(args) > 3 else 60
        interval = float(args[4]) if len(args) > 4 else 2.0
        cmd_ups(port, password, duration, interval)
    elif cmd == "metrics":
        cmd_metrics(port, password)
    else:
        print(__doc__)
        sys.exit(1)


if __name__ == "__main__":
    main()
