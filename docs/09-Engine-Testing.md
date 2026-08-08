# Engine Testing and Verification (Factorio 2.1.x)

## Purpose

How to run Rampant inside the real Factorio engine for verification, and what we
know about the engine that will bite you. This document exists because debugging
these issues cost hours of ad-hoc experimentation on 2026-08-08. Read it before
launching any headless test.

## Reference Locations

| What | Where |
|---|---|
| Lua runtime API (classes, events, defines) | `/mnt/gallery/gameFiles/factorio/doc-html/` (also `runtime-api.json`, `prototype-api.json` — machine-readable) |
| **Server settings reference** | `/mnt/gallery/gameFiles/factorio/data/server-settings.example.json` |
| Game data (prototype definitions) | `/mnt/gallery/gameFiles/factorio/data/` |
| LSP API definitions | `/mnt/backpack/code/games/factorio/luaDefinitions/` |
| Factorio binary | `/mnt/gallery/gameFiles/factorio/bin/x64/factorio` |

The server-settings example is the authoritative source for server settings — the
Lua API docs do NOT cover them, and unknown keys are **silently ignored**.

## Canonical Test Suites

| Suite | Command | What it verifies |
|---|---|---|
| Entity lifecycle | `just test` | Scenario runs 48 self-contained entity/unit-group tests in `on_init`; validates vanilla APIs and prototypes |
| Active AI smoke | `just test-active` | Full Rampant initialization + tick loop at 60 UPS; scenario signals when `remote.call("Rampant","isInitialized")` is true |
| RCON-driven AI | `just test-rcon` | Drives the live AI via RCON (`spawnEntity`, `advanceTicks`, `queryPheromones`, ...) |

Both `just test` and `just test-active`:
- purge stale saves (`saves/<scenario-name>*`) — a stale save hijacks scenario loading (see gotcha #2)
- capture factorio **stdout** to `/tmp/rampant-*.out` and poll it for `TESTS_COMPLETE`/`TESTS_FAILED` (see gotcha #1)
- kill factorio on failure (never `exit 1` without killing — the backgrounded server holds the shell's stdout pipe open and the caller hangs)
- write server settings with `"auto_pause": false` (see gotcha #6)

## The Critical Server Settings

```json
{
  "name": "Rampant Test Server",
  "visibility": {"public": false, "lan": false},
  "require_user_verification": false,
  "max_players": 5,
  "auto_pause": false
}
```

**`auto_pause` is the setting that matters.** Default is `true`: with no players
connected, the server **pauses** and advances exactly ~1 tick every 3.6s
(measured: 0.276 UPS — the idle heartbeat rate). Any test that needs ticks to
progress (active AI, scenario `on_tick` signals, UPS assertions) silently hangs
or crawls.

**`pause_when_no_players` is NOT a valid key** — it is silently ignored. Do not
use it.

## Engine 2.1.x Quirks and Gotchas

1. **`--console-log` does not capture Lua `print()` output.** It writes only the
   `=== Log opened ===` header. Lua prints go to **stdout**. Always capture and
   poll stdout: `factorio ... > /tmp/run.out 2>&1 &`.

2. **Stale saves hijack `--start-server-load-scenario`.** If `saves/<name>.zip`
   exists, the game loads the save (fires `on_load` — which is typically empty in
   test scenarios) instead of creating the scenario fresh (fires `on_init` —
   where the tests live). Symptom: scenario loads, checksum prints, but tests
   never run and no test output appears. Fix: `rm -f saves/<name>*` before launch.

3. **Scenario `on_init` runs BEFORE mod `on_init` on 2.1.x.** Do not expect mod
   initialization to be complete in the scenario's `on_init`. Poll readiness from
   `on_tick` instead. Remote interfaces ARE available (registered at mod load
   time, before any `on_init`).

4. **`storage` is per-mod (and per-scenario).** The scenario cannot read the
   mod's `storage`; `/silent-command` evaluates in the scenario context and sees
   the scenario's storage. Cross-mod readiness must use the remote interface:
   `remote.call("Rampant", "isInitialized")` (wrapped in `pcall`). Never poll
   `storage._rampantReady` from a scenario — it can never be true there.

5. **`LuaEntityPrototype` has a strict metatable.** Accessing a removed field
   **raises an error**, it does not return nil:
   `LuaEntityPrototype doesn't contain key build_base_evolution_requirement`.
   An error inside `on_tick` is a **non-recoverable error** — the server shuts
   down. Guard version-sensitive prototype access with `pcall`.

6. **Auto-pause** (see above) — the #1 silent test-killer.

7. **RCON is a binary protocol.** Plain `echo "quit" | nc` does NOT work (the
   justfile's RCON quit is cosmetic; the force-kill does the real work). Use
   `tools/rcon_client.py`. For responses, use `rcon.print(...)` in the command
   (LuaRCON global) — `print()` goes to game stdout, not the RCON response.

8. **Unit groups are `LuaCommandable`** in 2.x — there is no `LuaUnitGroup`.
   `defines.group_state` order: gathering=0, moving=1, attacking_distraction=2,
   attacking_target=3, **finished=4**, pathfinding=5, wander_in_group=6
   (`cleanSquads` checks `group.state == 4`).

9. **Table-style calls are valid** where documented: `surface.create_unit_group
   {force=..., position=...}` and `surface.set_multi_command{command=...,
   unit_count=..., unit_search_distance=...}` both have `takes_table: true` in
   the API docs.

10. **The `.lock` race**: `pkill -9 factorio` then immediately relaunching can
    fail with "Couldn't acquire exclusive lock for .../factorio/.lock". Sleep
    ~2s (or remove the lock) between kill and launch.

11. **`info.json` `factorio_version` must match the engine major version.**
    2.1.x refuses mods declaring `"factorio_version": "2.0"` (mod will not load;
    the mod load list shows base mods only, no Rampant).

12. **Rampant must be enabled in `mods/mod-list.json`** for `just test-active` /
    `just test-rcon` (the active-AI scenario waits for Rampant readiness). The
    entity tests are self-contained and pass with or without it. The mod is
    sometimes disabled in the user's local mod-list — check before testing.

13. **`game.speed` has no documented maximum** (min 0.01); achieved rate =
    `min(game.speed, UPS)`. UPS in headless is pure simulation cost (no
    rendering). `game.speed = 100` is used by the RCON test scenario. This is
    the basis for time-lapse testing (see methodology).

## Verification Methodology

For any engine problem, follow this loop — it is what actually solved the
auto-pause mystery:

1. **Measure, don't guess.** Instrument the live game via RCON
   (`tools/rcon_client.py ups <port> <password>`) to get a UPS curve before
   changing anything.
2. **One variable per run.** Change exactly one thing; re-measure; record.
3. **Control experiments.** If changes to the mod have NO effect on the
   symptom, suspect the environment — not the mod. The invariance itself is the
   diagnostic (this is what exposed auto-pause).
4. **UPS is an assertion, not a side note.** A headless server with no players
   must sustain ~60 UPS with `auto_pause: false`. Any test that needs ticks
   should gate on measured UPS, not wall-clock.
5. **Read the reference before theorizing.** The answer to the last mystery was
   in `server-settings.example.json`, not in the code.

### Phase-gated test structure

| Phase | Budget | Gate |
|---|---|---|
| P1 process up | 2s | boot/lock/port diagnostics |
| P2 mod+scenario loaded | 10s | data-stage errors |
| P3 InGame + RCON up | 5s | scenario errors |
| P4 Rampant initialized | 10s | `remote.call("Rampant","isInitialized")` |
| P5 ticks advancing | 5s | sample `game.tick` twice; assert UPS ≥ floor |
| P6 test signal | 60s | `TESTS_COMPLETE` / `TESTS_FAILED` on stdout |
| P7 teardown | 10s | RCON quit (working client), force-kill fallback |

## Scenario Authoring Contract

Any test scenario in `integration-tests/` must:

- **Print `TESTS_COMPLETE` (pass) or `TESTS_FAILED` (fail) to stdout** within the
  runner's budget. The runners poll captured stdout, not the console log.
- **Never busy-wait** for mod readiness (`for i=1,N do ... end` is a
  non-yielding spin that cannot work — the mod's `on_init` cannot run while the
  scenario's `on_init` is spinning). Poll from `on_tick` with a tick budget.
- **Poll readiness via the remote interface**, never via `storage` (per-mod
  scoping, gotcha #4). Pattern (see `integration-tests/active-ai-scenario/control.lua`):
  ```lua
  local function rampantReady()
      local ok, ready = pcall(remote.call, "Rampant", "isInitialized")
      return ok and ready == true
  end
  ```
- Keep `on_load` safe/empty unless the scenario is meant to be save-loaded
  (fresh scenario copies always fire `on_init`; stale saves fire `on_load`).

## Failure-Mode Quick Table

| Symptom | Cause | Diagnostic |
|---|---|---|
| No test output, recipe times out | Stale save hijack (#2) OR auto-pause (#6) | Check `saves/`; measure UPS |
| Ticks crawl at exactly ~0.276 UPS | `auto_pause` true (no players) | Fix server settings |
| Mod never loads | `factorio_version` mismatch (#11) or disabled in mod-list (#12) | Main log mod list |
| Server dies at tick 0 | Non-recoverable error in `on_tick` (#5) | Main log stack trace |
| Scenario never sees mod ready | storage scoping (#4) | Use remote interface |
| RCON returns nothing | Binary protocol / wrong password (#7) | Use `tools/rcon_client.py` |
| Immediate relaunch fails | `.lock` race (#10) | Sleep 2s between kill and launch |

## Post-Mortem (2026-08-08)

The "slow ticks" mystery: headless server showed exactly 0.276 UPS, invariant
across removing `recv_udp`, `processDirtyChunks`, etc. Root cause: the server
was **auto-paused** (default `auto_pause: true`, no players). The wrong key
(`pause_when_no_players`) had been used, silently ignored. Setting
`"auto_pause": false` restored 60 UPS instantly. Lesson: when a symptom is
invariant under every code change, the cause is outside the code — check the
environment configuration against the reference docs.
