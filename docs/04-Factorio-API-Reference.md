# Factorio API Reference

## Document Purpose

This document provides quick reference to the three critical Factorio codebases available for Rampant development. These paths contain the definitive information about Factorio's runtime capabilities, prototype definitions, and API specifications.

---

## Critical File Paths

### 1. Lua Definitions (LSP/API Documentation)

**Path**: `/mnt/backpack/code/games/factorio/luaDefinitions`

**Purpose**: Comprehensive Lua type definitions and API documentation for Factorio's runtime API.

**Contents**:
- **`library/runtime-api/`** - Runtime Lua API definitions
  - `LuaEntity.lua` - Entity property modification capabilities (2,652 lines)
  - `LuaSurface.lua` - Surface operations (entity creation, decoratives)
  - `LuaForce.lua` - Force-level modifiers and settings
  - `LuaUnitGroup.lua` - Unit group management
  - `LuaInventory.lua` - Inventory operations
  - `LuaPlayer.lua` - Player-specific APIs
  - `LuaGameScript.lua` - Global game operations

- **`library/prototype/`** - Prototype definition schemas
  - Entity prototype structures
  - Item prototype structures
  - Technology prototype structures

**Key Use Cases**:
- **Runtime capabilities**: Check what properties can be modified at runtime
- **Event definitions**: Find available events and their data structures
- **API methods**: Discover available methods on Lua objects
- **Type safety**: Understand expected parameter types and return values

**Example Reference**:
```lua
-- From LuaEntity.lua - See what's writable at runtime
---@class LuaEntity
local entity = {
    -- READ-ONLY properties (cannot be modified)
    prototype = "",        -- READ-ONLY
    name = "",             -- READ-ONLY
    type = "",             -- READ-ONLY
    max_health = 0,        -- READ-ONLY
    
    -- WRITABLE properties (can be modified)
    health = 0,            -- ✅ Can modify
    speed = 0,             -- ✅ Can modify (units only)
    energy = 0,            -- ✅ Can modify
    direction = 0,         -- ✅ Can modify
    color = {},            -- ✅ Can modify
}
```

---

### 2. Game Data Implementation (Source Code)

**Path**: `/mnt/gallery/gameFiles/factorio/data`

**Purpose**: Actual Factorio game implementation code - the "source of truth" for how entities, behaviors, and systems work.

**Key Subdirectories**:

#### **`base/prototypes/`** - Entity and Item Definitions
- **`entity/enemies.lua`** (968 lines) - Native biter/spitter prototypes
  - `small-biter`, `big-biter`, `medium-biter` unit definitions
  - `biter-spawner`, `spitter-spawner` definitions
  - `biter-base` (nest) definitions
  - Worm turret definitions
  - Autoplace configuration

- **`entity/worm-turrets.lua`** - Worm turret prototypes
- **`entity/character.lua`** - Player character prototype
- **`entity/transport-belt.lua`** - Belt and inserter prototypes

#### **`base/prototypes/noise-expressions.lua`** - Autoplace System
- Signal + noise generation for entity placement
- `enemy_autoplace_base` control signal
- `enemy_autoplace` noise expression
- Distance-based difficulty curves

#### **`base/scripts/`** - Game Logic (if present)
- AI behavior scripts
- Spawning logic
- Combat mechanics

**Key Use Cases**:
- **Prototype structure**: See how vanilla entities are defined
- **Autoplace logic**: Understand how Factorio places enemies naturally
- **Behavior definitions**: Check AI settings, attack patterns, resistances
- **Autoplace signals**: Learn how control signals work

**Example Reference**:
```lua
-- From data/base/prototypes/entity/enemies.lua
-- See actual biter prototype structure
data:extend({
  {
    type = "unit",
    name = "small-biter",
    flags = {"placeable-player", "placeable-enemy", "placeable-off-grid"},
    max_health = 15,
    healing_per_tick = 0.01,
    collision_box = {{-0.2, -0.2}, {0.2, 0.2}},
    selection_box = {{-0.4, -0.7}, {0.4, 0.4}},
    attack_parameters = {
      type = "projectile",
      range = 0.5,
      cooldown = 35,
      ammo_category = "melee",
      ammo_type = {
        target_type = "entity",
        action = {
          type = "direct",
          action_delivery = {
            type = "instant",
            target_effects = {
              type = "damage",
              damage = { amount = 7, type = "physical"}
            }
          }
        }
      }
    },
    vision_distance = 30,
    movement_speed = 0.2,
    absorptions_to_join_attack = { pollution = 4 },
    -- ... more properties
  }
})
```

---

### 3. Game Documentation (HTML Reference)

**Path**: `/mnt/gallery/gameFiles/factorio/doc-html`

**Purpose**: HTML documentation generated from Factorio's codebase - searchable reference for API, entities, and mechanics.

**Contents**:
- **API Reference** - Complete runtime API documentation
- **Entity Prototypes** - All entity types and their properties
- **Item Prototypes** - All item types and their properties
- **Technology Definitions** - Tech tree structure and effects
- **Event Definitions** - All available events and handlers
- **Control Settings** - Startup and global settings

**Key Use Cases**:
- **Quick lookup**: Search for specific entity types or API methods
- **Property reference**: Check all available properties for a prototype type
- **Event documentation**: Understand event data structures
- **Effect types**: See available technology effect types

**Access**: Open `index.html` in a web browser for searchable documentation.

---

## Quick Reference Guide

### For Runtime API Questions

**Use**: `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/`

**Common Questions**:
- "Can I modify this entity property at runtime?" → Check `LuaEntity.lua`
- "What events are available?" → Search for `script.on_event` definitions
- "What data does this event provide?" → Check event parameter definitions
- "How do I create this type of entity?" → Check `LuaSurface.lua`

**Key Files**:
- `LuaEntity.lua` - Entity modification (2,652 lines)
- `LuaSurface.lua` - Surface operations
- `LuaForce.lua` - Force modifications
- `defines.lua` - Enumerations and constants

---

### For Prototype Structure Questions

**Use**: `/mnt/gallery/gameFiles/factorio/data/base/prototypes/`

**Common Questions**:
- "How is a biter spawner defined?" → Check `entity/enemies.lua`
- "What properties does a unit have?" → Search unit prototypes
- "How does autoplace work?" → Check `noise-expressions.lua`
- "What are the enemy AI settings?" → Check `entity/biter-ai-settings.lua`

**Key Files**:
- `entity/enemies.lua` - All enemy prototypes (968 lines)
- `entity/worm-turrets.lua` - Worm definitions
- `noise-expressions.lua` - Autoplace signals and noise
- `item/item.lua` - Item definitions

---

### For Complete API Reference

**Use**: `/mnt/gallery/gameFiles/factorio/doc-html/`

**Common Questions**:
- "What's the complete list of entity types?" → Browse entity documentation
- "What effect types exist for technologies?" → Check technology effects
- "What are all the event names?" → Browse event documentation
- "How do I use this specific API method?" → Search HTML documentation

---

## Critical Runtime Limitations (From LuaEntity.lua)

### Read-Only Properties (Cannot Be Modified)
```lua
-- These properties are READ-ONLY at runtime:
entity.prototype       -- Cannot change prototype
entity.name           -- Cannot change name
entity.type           -- Cannot change type
entity.max_health     -- Cannot change max health
entity.bounding_box   -- Cannot change size
entity.collision_box  -- Cannot change collision
entity.minable        -- Cannot change minable state
entity.mineable_properties -- Cannot change mining properties
```

### Writable Properties (Can Be Modified)
```lua
-- These properties CAN be modified at runtime:
entity.health         -- ✅ Current health
entity.speed          -- ✅ Movement speed (units only)
entity.energy         -- ✅ Energy level (machines)
entity.direction      -- ✅ Facing direction
entity.color          -- ✅ Color (for customizable entities)
entity.force          -- ✅ Force ownership (with restrictions)
entity.mine()         -- ✅ Can mine entity
entity.destroy()      -- ✅ Can destroy entity
```

### Implications for Rampant
- **Prototype variations required**: Create multiple entity definitions for progression
- **No runtime stat modification**: Cannot change damage, health, speed on existing entities (except units' speed)
- **Upgrade patterns**: Use `entity.upgrade()` to change prototypes (if supported)
- **Entity replacement**: Destroy + recreate for major changes

---

## Autoplace System Reference

### Control Signal + Noise Pattern

Factorio uses a two-part autoplace system:

```lua
-- From data/base/prototypes/noise-expressions.lua

-- Part 1: Control Signal (determines overall density)
{
    name = "enemy_autoplace_base",
    control_signal = defines.control_signal.enemy_autoplace_base,
    -- This signal is set based on difficulty settings
}

-- Part 2: Noise Expression (determines local variation)
{
    type = "noise-expression",
    name = "enemy_autoplace",
    expression = "smoothed_noise_1 * enemy_autoplace_base",
    -- Combines noise with control signal
}
```

### Autoplace Configuration
```lua
-- From data/base/prototypes/entity/enemies.lua
{
    type = "unit-spawner",
    name = "biter-spawner",
    autoplace = {
        name = "biter-spawner",
        order = "b-d-a",
        control_expression = "enemy_autoplace_base",  -- Control signal
        noise_expression = "enemy_autoplace",         -- Noise variation
        default_control_value = 1.0,
        size = 3,  -- Chunk size for placement
    },
    -- ... rest of prototype
}
```

### Key Insights
- **Control signals** set global density (difficulty-based)
- **Noise expressions** add local variation (natural distribution)
- **Distance curves** are applied to control signal values
- **Chunk-based** placement (not per-entity)

---

## Pheromone System Integration Points

### Existing Rampant Pheromone Types
```lua
-- From libs/Constants.lua (check current definitions)
PHEROMONE_LEVELS = {
    ENEMY = 1,           -- Enemy presence
    PLAYER = 2,          -- Player presence
    POLLUTION = 3,       -- Pollution cloud
    -- Add new types as needed:
    TERRITORY = 4,       -- Territory strength
    TERRITORY_FACTION = 5, -- Faction identity
    TERRITORY_SUPPLY = 6,  -- Supply level
}
```

### Pheromone Processing Location
```lua
-- From libs/Processor.lua (check current implementation)
function processPheromone(chunk, tick)
    -- Existing diffusion logic
    -- Expand to include new pheromone types
end
```

---

## Entity Creation Patterns

### Creating Entities at Runtime
```lua
-- From LuaSurface.lua documentation
surface.create_entity({
    name = "entity-name",
    position = {x, y},
    force = "force-name",
    direction = 0,  -- Optional
    health = 100,   -- Optional (defaults to prototype max)
    create_build_effect_smoke = true,  -- Optional
})
```

### Creating Decoratives (Ground Markings)
```lua
-- From LuaSurface.lua documentation
surface.create_decoratives({
    decoratives = {
        {
            name = "decorative-name",
            position = {x, y},
            amount = 1,
        },
        -- More decoratives...
    }
})
```

### Batch Entity Creation (Performance)
```lua
-- More efficient for many entities
surface.create_entities({
    {
        type = "entity",
        name = "entity-name",
        position = {x, y},
        force = "force-name",
    },
    -- More entities...
})
```

---

## Event Handler Registration

### Common Events for Rampant
```lua
-- From defines.events
script.on_event(defines.events.on_built_entity, onEntityBuilt)
script.on_event(defines.events.on_player_mined_entity, onPlayerMine)
script.on_event(defines.events.on_entity_died, onEntityDied)
script.on_event(defines.events.on_unit_group_created, onUnitGroupCreated)
script.on_event(defines.events.on_research_finished, onResearchFinished)
```

### Custom Events
```lua
-- Define custom event
local my_event = script.generate_event_name()

-- Raise event
script.raise_event(my_event, {
    -- Event data
    entity = entity,
    player = player,
    custom_data = data,
})

-- Listen for event
script.on_event(my_event, onMyCustomEvent)
```

---

## Performance Guidelines

### On_Tick Handlers
- **Cost**: Runs 60 times/second
- **Recommendation**: Use event-driven logic when possible
- **Acceptable**: Early return 99% of time (sparse processing)
- **Unacceptable**: Heavy computation every tick

### Spatial Queries
```lua
-- Expensive for large radii
surface.find_entities_filtered{position = pos, radius = 100}

-- Faster for limit checks
surface.count_entities_filtered{position = pos, radius = 100}

-- Best practice: Cache chunk data, use chunk-based filtering
```

### Entity Count Thresholds
- **Safe**: < 1,000 entities (negligible impact)
- **Acceptable**: 1,000-5,000 entities (batch updates)
- **Warning**: 5,000-10,000 entities (optimize)
- **Danger**: 10,000+ entities (aggressive optimization)

---

## Integration with Rampant Ecosystem

### Existing Rampant Modules
```lua
-- From Rampant codebase
local BaseUtils = require("libs/BaseUtils")
local Processor = require("libs/Processor")
local Squad = require("libs/Squad")
local Constants = require("libs/Constants")
local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
```

### Ecosystem Mods
- **RampantEvolution** - Evolution mechanics
- **RampantArsenal** - Player weapons/equipment
- **RampantMaintenance** - Terrain breakdown
- **RampantIndustry** - Advanced factories
- **RampantResources** - Infinite resources

### Integration Points
- **Evolution**: Hook into RampantEvolution's evolution factor
- **Technology**: Use RampantArsenal for tech unlocks
- **Settings**: Respect Rampant startup settings

---

## Document History

- **v1.0** (Initial): Created comprehensive API reference with all three paths documented

---

## Quick Navigation

| Question | Primary Source | Secondary Source |
|----------|---------------|------------------|
| Can I modify this property at runtime? | LuaDefinitions/LuaEntity.lua | doc-html |
| How is this entity defined? | data/base/prototypes/entity/ | doc-html |
| What events are available? | LuaDefinitions/ | doc-html |
| How does autoplace work? | data/base/prototypes/noise-expressions.lua | doc-html |
| What's the complete API? | LuaDefinitions/ | doc-html |
| What effect types exist? | doc-html | LuaDefinitions/ |

---

**Note**: Always check the actual Factorio implementation (`/mnt/gallery/gameFiles/factorio/data/`) when in doubt - it's the source of truth for how things work.