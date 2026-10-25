#!/usr/bin/env python3
"""Assert that no Rampant prototype is placed by map generation.

Mapgen only ever places structures — enemy bases are spawners, not units. If a
generated spawner inherits vanilla's autoplace (it is a deep copy of the vanilla
prototype), every faction/tier/variant becomes its own autoplaced enemy type and
the map fills with them. That is how 354 autoplaced spawner prototypes produced
a wall of spawners around the origin.

Runs `factorio --dump-data` against a mod directory and inspects the resulting
prototype dump, so it needs no save, no server and no game tick: it fails in
about ten seconds.

usage: check-autoplace.py --mod-directory <dir with the mod installed> [--factorio PATH]
"""

import argparse
import glob
import json
import os
import shutil
import subprocess
import sys
import tempfile

DEFAULT_FACTORIO = "/mnt/gallery/gameFiles/factorio/bin/x64/factorio"
GAME_DATA = "/mnt/gallery/gameFiles/factorio/data"
STRUCTURE_TYPES = ("unit-spawner", "turret")


def dump_prototypes(factorio, mod_directory):
    work = tempfile.mkdtemp(prefix="rampant-autoplace-")
    try:
        config = os.path.join(work, "config.ini")
        with open(config, "w") as f:
            f.write(
                "; version=13\n[path]\n"
                f"read-data={GAME_DATA}\nwrite-data={work}\n"
                "[general]\nlocale=auto\n"
            )
        run = subprocess.run(
            [factorio, "--dump-data", "--mod-directory", mod_directory, "-c", config],
            capture_output=True, text=True,
        )
        if run.returncode != 0:
            sys.stderr.write(run.stdout[-2000:] + run.stderr[-2000:])
            raise SystemExit(f"factorio --dump-data failed (exit {run.returncode})")
        candidates = glob.glob(os.path.join(work, "script-output", "data-raw-dump.json"))
        if not candidates:
            raise SystemExit("data-raw-dump.json not produced")
        with open(candidates[0]) as f:
            return json.load(f)
    finally:
        shutil.rmtree(work, ignore_errors=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--mod-directory", required=True,
                    help="mod directory containing the built Rampant mod")
    ap.add_argument("--factorio", default=DEFAULT_FACTORIO)
    args = ap.parse_args()

    data = dump_prototypes(args.factorio, args.mod_directory)

    problems = []
    totals = {}
    for kind in STRUCTURE_TYPES:
        entries = data.get(kind, {})
        autoplaced = [name for name, proto in entries.items() if proto.get("autoplace")]
        rampant = [name for name in autoplaced if "rampant" in name]
        totals[kind] = (len(entries), len(autoplaced))
        for name in rampant:
            problems.append(f"{kind}: {name} is autoplaced by map generation")

    for kind, (total, autoplaced) in sorted(totals.items()):
        print(f"{kind}: {total} prototypes, {autoplaced} autoplaced")
    print(f"autoplaced Rampant structures: {len(problems)}")

    if problems:
        print("\nFAIL: Rampant prototypes must not be map-generated", file=sys.stderr)
        for line in problems[:20]:
            print("  " + line, file=sys.stderr)
        if len(problems) > 20:
            print(f"  ... and {len(problems) - 20} more", file=sys.stderr)
        print("\nVanilla spawners generate; the mod upgrades them in place.",
              file=sys.stderr)
        return 1

    print("OK: only vanilla structures are map-generated")
    return 0


if __name__ == "__main__":
    sys.exit(main())
