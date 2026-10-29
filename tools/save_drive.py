#!/usr/bin/env python3
"""Run a headless Factorio against a COPY of a save, and talk to the mod over UDP exec.

Library, not a script: the measurement scripts in this directory import it.

  import save_drive as d
  run = d.Run()                 # copies the save, launches the server, waits for ports
  run.wait_ready(300)
  print(d.call("fieldLoad"))    # remote.call("Rampant", ...), result decoded
  print(d.lua("return game.tick"))
  run.close()                   # SIGKILL, then verify the original save is untouched

Safety, enforced here rather than by convention:
  * the save is COPIED and only the copy is loaded, so the original cannot be written;
  * the server is always killed with SIGKILL - SIGTERM makes Factorio save the map in
    place, which is how a test run once overwrote a real save;
  * close() prints and asserts the original's md5 and mtime, and refuses to claim
    success if they moved.

Environment: SAVE (save to copy), FACTORIO_BIN, DRIVE_UDP (default 5200), DRIVE_RCON
(default 27018). Factorio 2.1.23 needs read-data in the config explicitly; the generated
config sets it from the binary path.
"""
import hashlib
import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SAVE = os.environ.get("SAVE", "/mnt/gallery/gameFiles/factorio/saves/Starting2.zip")
FAC = os.environ.get("FACTORIO_BIN", "/mnt/gallery/gameFiles/factorio/bin/x64/factorio")
UDP_PORT = int(os.environ.get("DRIVE_UDP", "5200"))
RCON_PORT = int(os.environ.get("DRIVE_RCON", "27018"))


def wait_ports_free(seconds=120):
    """Wait until our UDP and RCON ports are unbound, so the next server owns them."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        busy = False
        for port, kind in ((UDP_PORT, socket.SOCK_DGRAM), (RCON_PORT, socket.SOCK_STREAM)):
            s = socket.socket(socket.AF_INET, kind)
            try:
                s.bind(("127.0.0.1", port))
            except OSError:
                busy = True
            finally:
                s.close()
        if not busy:
            return True
        time.sleep(1.0)
    return False


def md5(path):
    h = hashlib.md5()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1 << 20), b""):
            h.update(block)
    return h.hexdigest()


def lua(code, timeout=60.0):
    """exec: one line of Lua in the mod's context. Returns the result, or an
    ("error", message) string."""
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.settimeout(timeout)
    try:
        s.sendto(("exec:" + code).encode(), ("127.0.0.1", UDP_PORT))
        data, _ = s.recvfrom(200000)
    except socket.timeout:
        return "<timeout>"
    finally:
        s.close()
    try:
        pkt = json.loads(data.decode())
    except ValueError:
        return data.decode("utf-8", "replace")
    if "error" in pkt:
        return "<error: %s>" % pkt["error"]
    return pkt.get("result")


def call(method, *args):
    a = ",".join(json.dumps(x) if isinstance(x, str) else str(x) for x in args)
    return lua('return remote.call("Rampant","%s"%s)' % (method, ("," + a) if a else ""))


def jdump(v):
    return json.dumps(v, sort_keys=True) if isinstance(v, (dict, list)) else str(v)


class Run:
    def __init__(self):
        self.work = tempfile.mkdtemp(prefix="rampant-drive.")
        self.orig_md5 = md5(SAVE)
        self.orig_mtime = os.stat(SAVE).st_mtime
        mods = os.path.join(self.work, "mods", "Rampant")
        os.makedirs(mods)
        os.makedirs(os.path.join(self.work, "ud"))
        for name in os.listdir(REPO):
            if name.startswith(".") and name not in (".luacheckrc",):
                continue
            src = os.path.join(REPO, name)
            if os.path.isfile(src) or name in ("sounds", "locale", "libs", "graphics",
                                               "prototypes", "migrations"):
                dst = os.path.join(mods, name)
                if os.path.isdir(src):
                    shutil.copytree(src, dst)
                else:
                    shutil.copy(src, dst)
        shutil.copy(SAVE, os.path.join(self.work, "world.zip"))
        # read-data must be explicit: 2.1.23 no longer derives it from the executable
        # dir, and without it the server dies at t=0 with "no package core".
        with open(os.path.join(self.work, "config.ini"), "w") as f:
            f.write("[path]\nread-data=%s/data\nwrite-data=%s/ud\n"
                    % (os.path.dirname(os.path.dirname(os.path.dirname(FAC))), self.work))
        with open(os.path.join(self.work, "server.json"), "w") as f:
            f.write(json.dumps({"name": "drive", "description": "", "visibility":
                                {"public": False, "lan": False},
                                "require_user_verification": False, "max_players": 5,
                                "auto_pause": False, "autosave_interval": 0}))
        # A previous headless run can still hold the UDP/RCON ports while it shuts down,
        # and the launch then dies with "Host address is already in use" while a stale
        # server answers the ready probe - which silently measures the wrong process.
        wait_ports_free()
        self.log = os.path.join(self.work, "run.log")
        self.p = subprocess.Popen(
            [FAC, "--start-server", os.path.join(self.work, "world.zip"),
             "--mod-directory", os.path.join(self.work, "mods"),
             "-c", os.path.join(self.work, "config.ini"),
             "--server-settings", os.path.join(self.work, "server.json"),
             "--enable-lua-udp", str(UDP_PORT),
             "--rcon-port", str(RCON_PORT), "--rcon-password", "test"],
            stdout=open(self.log, "w"), stderr=subprocess.STDOUT)

    def wait_ready(self, seconds=300):
        deadline = time.time() + seconds
        while time.time() < deadline:
            log = open(self.log).read() if os.path.exists(self.log) else ""
            if "already in use" in log:
                print("launch failed: port in use", flush=True)
                return False
            if lua('return remote.call("Rampant","isInitialized")', 3.0) is True:
                return True
            time.sleep(1.0)
        return False

    def close(self):
        self.p.send_signal(9)
        try:
            self.p.wait(timeout=60)
        except subprocess.TimeoutExpired:
            pass
        new_md5 = md5(SAVE)
        new_mtime = os.stat(SAVE).st_mtime
        ok = new_md5 == self.orig_md5 and new_mtime == self.orig_mtime
        print("\nsave untouched=%s md5=%s mtime=%s work=%s" %
              (ok, new_md5, new_mtime, self.work), flush=True)
        if not ok:
            print("WARNING: the original save CHANGED", flush=True)
        return ok


def tick():
    return lua("return game.tick")


def main():
    run = Run()
    print("work=%s" % run.work, flush=True)
    if not run.wait_ready():
        print("never initialized; tail of log:", flush=True)
        print(open(run.log).read()[-3000:])
        run.close()
        return
    print("ready at tick %s" % tick(), flush=True)

    # Backlog trend first: an unmodified run says whether the fields keep up with the
    # load-time source reports at all, before any diagnostic disturbs them.
    for i in range(6):
        time.sleep(10)
        print("t+%02ds tick=%s %s" % ((i + 1) * 10, tick(), jdump(call("fieldLoad"))), flush=True)

    t0 = tick()
    for name in ("resource", "base", "enemy"):
        print("\n=== %s @ tick %s ===" % (name, tick()), flush=True)
        print("  load      %s" % jdump(call("fieldLoad")), flush=True)
        print("  support   %s" % jdump(call("fieldSupport", name, 6)), flush=True)
        print("  rebuild   sources=%s" % jdump(call("rebuildField", name)), flush=True)
        print("  support+  %s" % jdump(call("fieldSupport", name, 6)), flush=True)
        print("  load+     %s" % jdump(call("fieldLoad")), flush=True)

    print("\n=== 30 s of play ===", flush=True)
    before = tick()
    time.sleep(30)
    after = tick()
    print("ticks simulated: %s -> %s (%s)" % (before, after, None if None in (before, after) else after - before), flush=True)

    for name in ("resource", "base", "enemy"):
        print("\n=== %s after play @ tick %s ===" % (name, tick()), flush=True)
        print("  load      %s" % jdump(call("fieldLoad")), flush=True)
        sup = call("fieldSupport", name, 6)
        print("  support   %s" % jdump(sup), flush=True)
        if isinstance(sup, str) and "id=" in sup:
            worst = sup.split("id=")[1].split()[0]
            print("  diag %s  %s" % (worst, jdump(call("fieldDiag", name, int(worst)))), flush=True)
        print("  verify    %s" % jdump(call("verifyField", name, 0.10)), flush=True)

    print("\n=== settle and sweep ===", flush=True)
    for i in range(3):
        time.sleep(20)
        print("t+%02ds tick=%s %s" % ((i + 1) * 20, tick(), jdump(call("fieldLoad"))), flush=True)
    print("ranges    %s" % jdump(call("fieldRanges")), flush=True)

    run.close()


if __name__ == "__main__":
    main()
