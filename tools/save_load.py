#!/usr/bin/env python3
"""
Save/load continuity driver.

Flow:
  1. launch the save-load scenario server
  2. wait for SAVE_READY in the scenario stdout
  3. RCON /save the game
  4. kill the server
  5. relaunch loading the SAVE (--start-server)
  6. wait for CONTINUITY_OK / TESTS_FAILED
  7. report

The scenario holds all state in storage, so the reload resumes at the
continuity phase automatically (on_load path).
"""

import argparse
import os
import subprocess
import sys
import time

FACTORIO = "/mnt/gallery/gameFiles/factorio/bin/x64/factorio"
SAVES_DIR = "/mnt/gallery/gameFiles/factorio/saves"
SCENARIOS_DIR = "/mnt/gallery/gameFiles/factorio/scenarios"
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
RCON = os.path.join(SCRIPT_DIR, "rcon_client.py")
SETTINGS = "/tmp/rampant-saveload-server-settings.json"
OUT = "/tmp/rampant-saveload.out"


def launch(load_save, port, rcon_port):
    args = [FACTORIO]
    if load_save:
        args += ["--start-server", os.path.join(SAVES_DIR, "rampant-save-load-test.zip")]
    else:
        args += ["--start-server-load-scenario", "rampant-behavior-save-load"]
    args += ["--server-settings", SETTINGS, "--port", str(port),
             "--rcon-port", str(rcon_port), "--rcon-password", "rampant-test"]
    out = open(OUT, "w")
    proc = subprocess.Popen(args, stdout=out, stderr=subprocess.STDOUT)
    return proc, out


def wait_for(out_path, needles, timeout):
    """Wait for any needle in the output file; return the matched needle."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            text = open(out_path).read()
        except FileNotFoundError:
            time.sleep(2)
            continue
        for needle in needles:
            if needle in text:
                return needle
        time.sleep(1)
    return None


def rcon(port, command):
    subprocess.run(
        [sys.executable, RCON, "query", str(port), "rampant-test", command],
        capture_output=True, text=True, timeout=20,
    )


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=34203)
    ap.add_argument("--rcon-port", type=int, default=27021)
    ap.add_argument("--timeout", type=float, default=300.0)
    args = ap.parse_args()

    with open(SETTINGS, "w") as f:
        f.write('{"name":"Rampant Save/Load","description":"save/load continuity",'
                '"visibility":{"public":false,"lan":false},'
                '"require_user_verification":false,"max_players":5,'
                '"auto_pause":false,"autosave_interval":0}')

    # Phase 1: fresh scenario -> SAVE_READY
    print("Phase 1: fresh scenario (waiting for SAVE_READY)...")
    proc, out = launch(False, args.port, args.rcon_port)
    got = wait_for(OUT, ["SAVE_READY", "TESTS_FAILED"], args.timeout)
    if got != "SAVE_READY":
        print("FAIL: SAVE_READY not reached (got " + str(got) + ")")
        proc.kill()
        sys.exit(1)
    print("SAVE_READY reached")

    # Save
    time.sleep(2)
    print("Saving...")
    rcon(args.rcon_port, "/save rampant-save-load-test")
    time.sleep(3)
    save_path = os.path.join(SAVES_DIR, "rampant-save-load-test.zip")
    if not os.path.exists(save_path):
        print("FAIL: save file not created at " + save_path)
        proc.kill()
        sys.exit(1)
    print("Saved: " + save_path)

    # Kill and reload from the save
    proc.kill()
    time.sleep(2)
    print("Phase 2: reloading save...")
    proc2, out2 = launch(True, args.port, args.rcon_port)
    got = wait_for(OUT, ["CONTINUITY_OK", "TESTS_FAILED", "TESTS_COMPLETE"], args.timeout)
    if got == "CONTINUITY_OK" or got == "TESTS_COMPLETE":
        print("SAVE/LOAD CONTINUITY: PASSED")
        status = 0
    else:
        print("SAVE/LOAD CONTINUITY: FAILED (signal " + str(got) + ")")
        status = 1

    # Show the scenario's verdict lines
    text = open(OUT).read()
    for line in text.splitlines():
        if any(k in line for k in ("[PASS]", "[FAIL]", "[INFO]", "Detail", "Passed:", "Failed:", "CONTINUITY", "TESTS")):
            print("  " + line)

    proc2.kill()
    sys.exit(status)


if __name__ == "__main__":
    main()
