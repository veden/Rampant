# 3.3.4 → Factorio 2.1 port

Branch `port/334-2.1`, version 3.3.5, cut from tag `3.3.4`.

Rebuilt from the released 1.1 code as **19 verified increments**, each its own commit, each
followed by a load check that reports the first failure. Total diff against 3.3.4:

```
16 files changed, 236 insertions(+), 78 deletions(-)
```

No line-ending churn: every edit preserves the file's own endings.

## Increments and their gate results

The gate is a headless load of a copy of `Starting2`; the right-hand column is what the build
reported *after* that increment.

```
 1  data: declare for 2.1 (version 3.3.5)                ERROR get_first_unused_layer nil
 2  data: named collision layer, not an unused bit       ERROR unknown flag "hidden"
 3  data: hidden is a prototype property                 ERROR beam needs graphics_set
 4  data: masks are { layers = {} }, 2.0 layer names     ERROR beam needs graphics_set
 5  data: hr_version promoted, compressed dropped        ERROR beam needs graphics_set
 6  data: spawner/turret graphics -> graphics_set        ERROR beam needs graphics_set
 7  data: beam sprites -> graphics_set.beam              ERROR drone: unknown flag "hidden"
 8  data: absorptions_per_second / to_join_attack        ERROR drone: unknown flag "hidden"
 9  data: fire emissions is a pollutant dictionary       ERROR drone: unknown flag "hidden"
10  data: build_base_evolution_requirement in `order`    ERROR drone: unknown flag "hidden"
11  control: global -> storage                           ERROR drone: unknown flag "hidden"
12  control: script.active_mods, prototypes.*            ERROR drone: unknown flag "hidden"
13  control: evolution factor per surface                ERROR drone: unknown flag "hidden"
14  control: runtime collision layer names               ERROR drone: unknown flag "hidden"
15  control: unit group ids, LuaEntity.commandable       ERROR drone: unknown flag "hidden"
16  data: drone flags (hidden inside an or-default)      ERROR fire emissions_per_second
17  data: fire spread emissions is a dictionary          ERROR shotgun-pellet hit_collision_mask
18  data: projectile hit-mask loop writes layers dicts   ERROR pollution_to_join_attack
19  data: second spitter unit absorptions site           RUNS (map loaded, hosting, 0 errors)
```

Steps 11-15 were applied blind (the data stage blocked the load), which is why they all show
the same gate: the data-stage failure masked them. They are all needed once the load reaches
the control stage, and step 19's gate exercises them.

## What each increment is

Data stage
- `info.json`: factorio_version 2.1, version 3.3.5
- collision layer: the 1.1 `collision_mask_util.get_first_unused_layer()` (removed in 2.0) is
  replaced by a named layer declared as a `collision-layer` prototype
- `hidden` moved from a sprite/entity flag to a prototype property
- collision masks: `{ "layer" }` → `{ layers = { layer = true } }`, nested masks included
  (`action.target_effects.tile_collision_mask`), with 1.1 names mapped to 2.0
  (`player-layer` → `player`, `train-layer` → `train`, `water-tile` → `water_tile`); `fire`
  keeps its mask as a tile mask
- `hr_version` promoted to the base sprite; `"compressed"` flag dropped
- spawner pictures under `graphics_set.animations` / `.integration`; turret under
  `graphics_set.base_visualisation`; beam under `graphics_set.beam`
- absorption: `pollution_absorption_absolute/proportional` → `absorptions_per_second`;
  `pollution_to_join_attack` → `absorptions_to_join_attack`
- `emissions_per_second` is a pollutant dictionary
- `build_base_evolution_requirement` (removed in 2.0) carried in the prototype `order`
  string and parsed back by `BaseUtils`
- the projectile hit-mask loop writes layer dictionaries

Control stage
- `global` → `storage` (`settings.global` untouched — it is a different table)
- `game.active_mods` → `script.active_mods`; `game.*_prototypes` → `prototypes.*`
- `LuaForce.evolution_factor` → `get_evolution_factor(surface)` (evolution is per surface)
- runtime collision-mask strings renamed to 2.0 layer names
- `group_number` → `unique_id`; `LuaEntity.unit_group` → `LuaEntity.commandable`
  (the group when `is_unit_group`)

## Next

1. probe remote + harness (`rcon_client`, `save_drive`, `measure-port`, `compare-builds`,
   `probe-behaviour`) as one commit, then re-baseline against `checkpoint-h8-loop-design`
2. behaviour scenarios ported for semantics (the port has none)
3. save/load roundtrip and a long soak
