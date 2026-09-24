#!/usr/bin/env python3
"""Save/load continuity driver, workspace-local.

Same flow as tools/save_load.py, but every path is inside the workspace: the
install dir is read-only, and tools/save_load.py also omits --mod-directory, so it
would test the installed mod copy rather than the working tree.
"""
import argparse, os, shutil, subprocess, sys, time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FACTORIO = "/mnt/gallery/gameFiles/factorio/bin/x64/factorio"
B = os.path.join(ROOT, ".bench")
SAVES_DIR = os.path.join(B, "user-data", "saves")
SCENARIOS_DIR = os.path.join(B, "user-data", "scenarios")
MODS = os.path.join(B, "mods")
CONFIG = os.path.join(B, "user-data", "config.ini")
SCRIPT_DIR = os.path.join(ROOT, "tools")
RCON = os.path.join(SCRIPT_DIR, "rcon_client.py")
SETTINGS = os.path.join(B, "saveload-server-settings.json")
OUT = os.path.join(B, "saveload.out")
NAME = "rampant-behavior-save-load"
SAVE_NAME = "rampant-save-load-test"


def stage():
    os.makedirs(SAVES_DIR, exist_ok=True)
    os.makedirs(SCENARIOS_DIR, exist_ok=True)
    src = os.path.join(ROOT, "integration-tests", "behavior-scenarios", "save-load")
    dst = os.path.join(SCENARIOS_DIR, NAME)
    shutil.rmtree(dst, ignore_errors=True)
    shutil.copytree(src, dst)
    shutil.copy(os.path.join(ROOT, "integration-tests", "lib", "scenario-lib.lua"),
                os.path.join(dst, "scenario-lib.lua"))
    prepared = os.path.join(ROOT, "integration-tests", "behavior-saves", NAME + "-s1.zip")
    shutil.copy(prepared, os.path.join(SAVES_DIR, NAME + ".zip"))


def launch(load_save, port, rcon_port):
    args = [FACTORIO, "--mod-directory", MODS, "-c", CONFIG]
    if load_save:
        args += ["--start-server", os.path.join(SAVES_DIR, SAVE_NAME + ".zip")]
    else:
        args += ["--start-server-load-scenario", NAME]
    args += ["--server-settings", SETTINGS, "--port", str(port),
              "--rcon-port", str(rcon_port), "--rcon-password", "rampant-test"]
    out = open(OUT, "w")
    return subprocess.Popen(args, stdout=out, stderr=subprocess.STDOUT), out


def wait_for(needles, timeout):
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            text = open(OUT, errors="replace").read()
        except FileNotFoundError:
            time.sleep(2); continue
        for n in needles:
            if n in text:
                return n
        time.sleep(1)
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=34203)
    ap.add_argument("--rcon-port", type=int, default=27021)
    ap.add_argument("--timeout", type=float, default=300.0)
    args = ap.parse_args()
    stage()
    with open(SETTINGS, "w") as f:
        f.write('{"name":"Rampant Save/Load","description":"save/load continuity",'
                '"visibility":{"public":false,"lan":false},'
                '"require_user_verification":false,"max_players":5,'
                '"auto_pause":false,"autosave_interval":0}')
    print("Phase 1: scenario (waiting for SAVE_READY)...")
    proc, _ = launch(False, args.port, args.rcon_port)
    got = wait_for(["SAVE_READY", "TESTS_FAILED"], args.timeout)
    if got != "SAVE_READY":
        print("FAIL: SAVE_READY not reached (got " + str(got) + ")")
        proc.kill(); sys.exit(1)
    print("SAVE_READY reached")
    time.sleep(2)
    subprocess.run([sys.executable, RCON, "query", str(args.rcon_port),
                    "rampant-test", "/save " + SAVE_NAME],
                   capture_output=True, text=True, timeout=30)
    time.sleep(3)
    save_path = os.path.join(SAVES_DIR, SAVE_NAME + ".zip")
    if not os.path.exists(save_path):
        print("FAIL: save not created at " + save_path)
        proc.kill(); sys.exit(1)
    print("Saved: " + save_path)
    proc.kill(); time.sleep(2)
    print("Phase 2: reloading save...")
    proc2, _ = launch(True, args.port, args.rcon_port)
    got = wait_for(["CONTINUITY_OK", "TESTS_FAILED", "TESTS_COMPLETE"], args.timeout)
    status = 0 if got in ("CONTINUITY_OK", "TESTS_COMPLETE") else 1
    print("SAVE/LOAD CONTINUITY: " + ("PASSED" if status == 0 else "FAILED (signal " + str(got) + ")"))
    for line in open(OUT, errors="replace").read().splitlines():
        if any(k in line for k in ("[PASS]", "[FAIL]", "Detail", "Passed:", "Failed:", "CONTINUITY", "TESTS")):
            print("  " + line)
    proc2.kill()
    sys.exit(status)


if __name__ == "__main__":
    main()
