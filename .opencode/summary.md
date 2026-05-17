# Rampant Factorio Mod - Factorio 2.0 Upgrade

## Goal
Upgrade Rampant enemy AI mod from Factorio 1.1 to 2.0 compatibility, fix all runtime crashes, and build tooling for testing/visualization.

## Constraints & Preferences
- Polyfills only in test infrastructure, not production code
- Prefer deep-copying vanilla prototypes over hardcoding sprite patterns
- Startup settings for feature flags are acceptable
- User prefers direct communication, no handholding

## Progress

### Completed - Factorio 2.0 API Migration

**Sprite System (data stage):**
- Biters/Spitters: `makeBiter()`/`makeSpitter()` now `util.table.deepcopy(data.raw.unit["small-biter"])` instead of manual sprite defs
- Spawners: `makeUnitSpawner()` deep copies from vanilla `biter-spawner`/`spitter-spawner`, overrides only Rampant-specific fields. `applySpawnerTints()` re-tints inherited animation layers
- Spawner corpses: `makeUnitSpawnerCorpse()` deep copies from vanilla corpses
- Worms: Stripped all `hr_version` blocks (HR sprites removed in 2.0), fixed zero-padded filenames (`worm-attack-01.png` → `worm-attack-1.png`)
- All other visual effects (BeamUtils, DroneUtils, FireUtils, ProjectileUtils, StreamUtils): Stripped all `hr_version` blocks
- Fire/tree-flame custom effects: Stubbed out (`create_fire_pictures()`, `create_small_tree_flame_animations()` return nil)
- StickerUtils default animation: Switched to existing `burnt-patch.png`

**Runtime API Changes:**
- `game.active_mods` → `script.active_mods` (deprecated in 2.0)
- `global.universe` → `storage.universe` (global = nil in 2.0)
- `force.evolution_factor` → `force.get_evolution_factor(surface)` (property became method requiring surface param)
- Collision layers: `player-layer` → `player` (dropped `-layer` suffix)
- `table.contains()` doesn't exist → manual loop check

**Initialization Fixes:**
- `Upgrade.addUniverseProperties()` was wiping critical fields (`awake`, `random`, `evolutionLevel`). Now preserves them during property migration wipe
- `onInit()` sets `Universe.random` and `Universe.evolutionLevel` unconditionally before tick handler runs
- Tick handler guards on `Universe.random` existence (readiness gate)
- `initializeLibraries()` now runs BEFORE `Upgrade.attempt()` so Upgrade's local Universe reference is set
- `MapUtils.getChunkById()`: Added `type(xChunks) == "table"` guard when iterating map objects (metadata fields mixed with chunk grid)

**Settings:**
- Initial peace time changed from minutes to seconds (default 1200s = 20min). User can set to 5s for testing via mod settings

### Completed - UDP Tooling

**Mod-side (`libs/Observability.lua`):**
- UDP server via `helpers.recv_udp()` + `on_udp_packet_received` event
- Polls every tick: `helpers.recv_udp()` + `helpers.recv_udp(1)` (single-player needs player socket, server socket may not exist)
- Commands: `ping`, `info`, `squads`, `bases`, `state`
- Responds as JSON to caller's source port

**Python Client (`tools/udp-client.py`):**
- CLI client: `python3 tools/udp-client.py <command>`
- Binds random port, sends command, receives JSON response

**GTK4 Dashboard (`tools/ramp-dashboard.py`):**
- Real-time AI dashboard with panels for: Game State, Pheromone bars, Pheromone Map (2D heatmap), Squads list, Bases list
- Polls Factorio every second
- Pheromone Map: Colored grid (blue→green→yellow→red), selector buttons for pheromone type, auto-scaling cells

### Known Issues / Remaining Work

**`awake` flag:** Still shows `false` after init due to `addUniverseProperties()` wiping. Functionally the tick handler runs (guards on `random` instead), but the print statement shows stale value. Not blocking.

**Worm animations:** WormUtils still has manual sprite definitions. Deep-copy approach would be ideal but worms don't have direct vanilla equivalents for all Rampant variants. Current stripped-HR version works.

**Integration tests:** Not yet updated for Factorio 2.0 API changes.

## Key Decisions

1. **Deep-copy vanilla prototypes** for biters/spitters/spawners — survives sprite renames, less surface area for breakage
2. **Strip HR blocks** everywhere — Factorio 2.0 removed all HR sprite variants
3. **UDP on player socket** — single-player mode has no server socket; must poll both `recv_udp()` and `recv_udp(player_index)`
4. **Tick handler readiness gate** on `Universe.random` — simpler than a separate `_initialized` flag
5. **Pheromone map as DrawingArea subclass** — GTK4 requires `do_draw()` override, no `connect("draw")` or `SignalController`

## Critical Context

- Factorio 2.0 sets `global = nil` — all code must use `storage` instead
- Collision layers renamed (suffix dropped): `player-layer` → `player`, `object-layer` → `object`
- `force.evolution_factor` is now per-surface method: `get_evolution_factor(surface)`
- UDP requires `--enable-lua-udp PORT` launch flag
- GTK4 PyGObject: No `EventBox`, no `set_width_request()`, no `get_style_context().add_class()`, no `get_children()`, DrawingArea uses `do_draw()` override

## Relevant Files

**Core migration:**
- `prototypes/utils/BiterUtils.lua` — makeBiter/makeSpitter/makeUnitSpawner deep-copy pattern
- `prototypes/utils/WormUtils.lua` — stripped HR blocks, fixed filenames
- `libs/Upgrade.lua` — addUniverseProperties field preservation, collision layer names
- `control.lua` — API updates, init ordering, UDP recv polling, tick handler guards

**Tooling:**
- `libs/Observability.lua` — UDP server + command handlers
- `tools/udp-client.py` — CLI test client
- `tools/ramp-dashboard.py` — GTK4 dashboard with pheromone map
