# Factorio Native Unit Spawner System Analysis

## Executive Summary

This document provides a comprehensive analysis of Factorio's native unit spawner system, including unit spawner mechanics, unit group formation, pollution interaction, and Lua API capabilities. This analysis identifies integration points for Rampant's supply tracking system and delineates what can be controlled via Lua vs. what is hardcoded in the game engine.

---

## 1. Unit Spawner Mechanics

### 1.1 Prototype Structure

Unit spawners are defined as `unit-spawner` entity type with the following key prototype parameters:

```lua
{
  type = "unit-spawner",
  name = "biter-spawner",
  
  -- Unit capacity limits
  max_count_of_owned_units = 7,        -- Max units currently spawned but not released
  max_friends_around_to_spawn = 5,     -- Max friendly units nearby to trigger spawn
  
  -- Spawning timing and location
  spawning_cooldown = {360, 150},      -- {min, max} ticks between spawns (6s to 2.5s)
  spawning_radius = 10,                 -- Radius in which units can spawn
  spawning_spacing = 3,                 -- Minimum distance between spawned units
  max_spawn_shift = 0,                  -- Additional randomness based on nest richness
  
  -- Unit composition
  result_units = {                      -- Weighted unit types by evolution factor
    {"small-biter", {{0.0, 0.3}, {0.6, 0.0}}},
    {"medium-biter", {{0.2, 0.0}, {0.6, 0.3}, {0.7, 0.1}}},
    {"big-biter", {{0.5, 0.0}, {1.0, 0.4}}},
    {"behemoth-biter", {{0.9, 0.0}, {1.0, 0.3}}}
  },
  
  -- Pollution interaction
  absorptions_per_second = { 
    pollution = { 
      absolute = 20,     -- Fixed pollution absorbed per second
      proportional = 0.01 -- Percentage of chunk pollution absorbed
    }
  },
  
  -- Group formation
  time_to_capture = 60 * 20,   -- Ticks for unit to join group (20s for biters)
  call_for_help_radius = 50,   -- Radius to call nearby units
  
  -- AI settings
  ai_settings = {
    destroy_when_commands_fail = true,
    allow_try_return_to_spawner = true
  }
}
```

**Source Files:**
- `/mnt/gallery/gameFiles/factorio/data/base/prototypes/entity/enemies.lua` (lines 92-184 for biter-spawner, 749-840 for spitter-spawner)

### 1.2 Spawn Trigger Conditions

Unit spawners spawn units based on the following conditions (hardcoded in game engine):

1. **Cooldown Check**: Spawning only occurs when `spawning_cooldown` period has elapsed
2. **Capacity Check**: `current_owned_units < max_count_of_owned_units` (7 for standard spawners)
3. **Friend Proximity**: `friends_around <= max_friends_around_to_spawn` (5 for standard spawners)
4. **Pollution Presence**: Spawners absorb pollution from their chunk via `absorptions_per_second`
5. **Evolution Factor**: Unit type selected based on `force.get_evolution_factor(surface)`

**What Cannot Be Controlled:**
- The actual spawning logic is **hardcoded in C++** engine
- Lua cannot directly trigger or prevent individual spawns
- Lua cannot modify spawn cooldowns at runtime (only via prototype changes)

### 1.3 Spawner-Unit Relationship

- Units spawned by a spawner are initially "captured" by it
- `time_to_capture` defines how long until a unit joins a group
- During capture period, spawner counts the unit against `max_count_of_owned_units`
- Once unit joins a group or is released, spawner can spawn another unit

---

## 2. Unit Group Formation and Lifecycle

### 2.1 Group Creation Events

Factorio fires two key events for unit groups:

```lua
-- Event 1: Group created (before any members added)
script.on_event(defines.events.on_unit_group_created, function(event)
    local group = event.group  -- LuaCommandable
    -- Group is empty at this point
end)

-- Event 2: Group finished gathering (ready to execute command)
script.on_event(defines.events.on_unit_group_finished_gathering, function(event)
    local group = event.group
    -- Group has all members, ready to move/attack
end)
```

**Source:** `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/events.lua` (lines 5749-5783)

### 2.2 Group States

Groups transition through these states (defined in `defines.group_state`):

1. **GATHERING**: Units are joining the group
2. **MOVING**: Group is moving to destination
3. **ATTACKING**: Group is attacking a target

```lua
-- State accessible via LuaCommandable.state
group.state  -- Returns defines.group_state.gathering/moving/attacking
```

**Source:** `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaCommandable.lua` (line 88)

### 2.3 Gathering Process

**Gathering Parameters** (from `map_settings.unit_group`):

```lua
{
  -- Timing
  min_group_gathering_time = 3600,        -- 60 seconds minimum
  max_group_gathering_time = 10 * 3600,   -- 10 minutes maximum
  max_wait_time_for_late_members = 2 * 3600,  -- 2 minutes after gathering ends
  
  -- Size limits
  max_gathering_unit_groups = 30,         -- Max groups gathering simultaneously
  max_unit_group_size = 200,              -- Max units per group (auto-created only)
  
  -- Spatial
  max_group_radius = 30.0,                -- Max radius based on unit count
  min_group_radius = 5.0,                 -- Min radius
  
  -- Movement coordination
  max_member_speedup_when_behind = 1.4,
  max_member_slowdown_when_ahead = 0.6,
  max_group_slowdown_factor = 0.3,
  max_group_member_fallback_factor = 3,
  member_disown_distance = 10,            -- Distance before unit removed from group
  tick_tolerance_when_member_arrives = 60
}
```

**Source:** `/mnt/gallery/gameFiles/factorio/data/base/prototypes/map-settings.lua` (lines 98-127)

**Gathering Flow:**
1. Group created via `surface.create_unit_group()` or automatically by spawner
2. `on_unit_group_created` event fires (group is empty)
3. Units within `time_to_capture` radius automatically join
4. Group waits between `min_group_gathering_time` and `max_group_gathering_time` (random)
5. After gathering time expires, `on_unit_group_finished_gathering` event fires
6. Group waits additional `max_wait_time_for_late_members` for stragglers
7. Group begins executing command (or moves autonomously if autonomous)

### 2.4 Group Creation Methods

**Method 1: Manual Creation (Lua API)**
```lua
local group = surface.create_unit_group({
    position = {x = 100, y = 100},
    force = "enemy"  -- Optional, defaults to "enemy"
})
-- Fires on_unit_group_created immediately
```

**Method 2: Automatic Creation (Spawner-triggered)**
- When spawner detects pollution and meets spawn conditions
- Engine automatically creates group at spawner position
- Units join group during `time_to_capture` period
- Cannot be prevented via Lua (only by destroying spawner)

**Source:** `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaSurface.lua` (lines 1664-1673)

---

## 3. Pollution-Triggered Spawning Mechanics

### 3.1 Pollution Absorption

Spawners absorb pollution from their chunk:

```lua
absorptions_per_second = { 
  pollution = { 
    absolute = 20,      -- Fixed 20 pollution units per second
    proportional = 0.01 -- Plus 1% of chunk's total pollution
  }
}
```

**Source:** `/mnt/gallery/gameFiles/factorio/data/base/prototypes/entity/enemies.lua` (line 135 for biter, 789 for spitter)

### 3.2 Pollution-to-Spawn Pipeline

**Hardcoded Engine Behavior:**
1. Pollution diffuses across chunks (controlled by `map_settings.pollution.diffusion_ratio = 0.02`)
2. Spawners absorb pollution from their chunk each tick
3. When spawner has accumulated enough pollution AND cooldown expired AND capacity available:
   - Engine creates unit group
   - Spawns unit(s) into the group
   - Unit joins group after `time_to_capture` ticks

**Evolution Factor Components:**
```lua
-- Accessible via LuaForce API
force.get_evolution_factor(surface)           -- Total evolution (0.0 to 1.0)
force.get_evolution_factor_by_pollution(surface)  -- Pollution component
force.get_evolution_factor_by_time(surface)       -- Time component
force.get_evolution_factor_by_killing_spawners(surface)  -- Destruction component
```

**Source:** `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaForce.lua` (lines 426-447)

**Evolution Calculation** (from `map_settings.enemy_evolution`):
```lua
{
  enabled = true,
  time_factor = 0.000004,        -- Per-second increase
  destroy_factor = 0.002,        -- Per spawner destroyed
  pollution_factor = 0.0000009   -- Per pollution unit
}
```

**Source:** `/mnt/gallery/gameFiles/factorio/data/base/prototypes/map-settings.lua` (lines 36-44)

### 3.3 What Can Be Controlled via Lua

**Can Control:**
- Destroy spawners (increases evolution via `destroy_factor`)
- Read pollution levels and evolution factors
- Modify `map_settings` at game start (not runtime)
- Create custom unit groups manually
- Set commands on groups

**Cannot Control:**
- Direct spawn triggering (hardcoded engine logic)
- Spawn cooldown timing (hardcoded)
- Pollution absorption rate (prototype-defined)
- Automatic group creation from spawners (engine-managed)

---

## 4. Lua Unit Group API Reference

### 4.1 LuaCommandable Interface

Unit groups implement the `LuaCommandable` interface:

**Properties:**
```lua
group.valid                    -- boolean: Is object valid?
group.is_unit_group            -- boolean: Always true for groups
group.is_script_driven         -- boolean: True if created via Lua API
group.force                    -- LuaForce: Group's force
group.surface                  -- LuaSurface: Group's surface
group.position                 -- MapPosition: Current position
group.state                    -- defines.group_state: gathering/moving/attacking
group.command                  -- Command: Current command (if any)
group.moving_state             -- defines.moving_state: Current movement state
group.group_number             -- uint32: Unique group identifier
group.commandable_members      -- LuaCommandable[]: Direct members
group.members                  -- LuaEntity[]: All entity members (recursive)
group.spawner                  -- LuaEntity: Associated spawner (if any)
```

**Methods:**
```lua
-- Lifecycle
group:destroy()                -- Destroy group (unlinks members, doesn't kill them)

-- Membership
group:add_member(member)       -- Add LuaCommandable or LuaEntity to group
                               -- Member must be same force and surface

-- Commands
group:set_command(command)     -- Set AI command
group:set_distraction_command(command)  -- Set distraction behavior
group:set_autonomous()         -- Make group autonomous (auto-attacks pollution)
group:start_moving()           -- Force group to start moving (skip waiting)

-- Utilities
group:release_from_spawner()   -- Release from spawner (allows more spawning)
```

**Source:** `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaCommandable.lua`

### 4.2 Command Types

Available commands for unit groups:

```lua
defines.command.group          -- Join another group
defines.command.wander         -- Wander in area
defines.command.build_base     -- Build base at location
defines.command.attack_area    -- Attack area
defines.command.go_to_location -- Move to location
defines.command.compound       -- Chain multiple commands
defines.command.flee           -- Flee from entity/area
defines.command.stop           -- Stop all actions
```

**Example Commands:**
```lua
-- Wander command
{
  type = defines.command.wander,
  radius = 100,                -- Wander radius
  wander_in_group = true,      -- Stay in group while wandering
  ticks_to_wait = 3600         -- Wait time between movements
}

-- Attack area command
{
  type = defines.command.attack_area,
  radius = 50,                 -- Attack radius
  distraction = defines.distraction.by_anything
}

-- Compound command
{
  type = defines.command.compound,
  structure_type = defines.command.compound_command_return_last,
  commands = {command1, command2, command3}
}
```

### 4.3 Group Control Capabilities

**Can Control:**
- ✅ Create groups manually via `surface.create_unit_group()`
- ✅ Add/remove members via `add_member()`
- ✅ Set commands (wander, attack, move, etc.)
- ✅ Destroy groups (unlinks members)
- ✅ Make groups autonomous
- ✅ Force start moving with `start_moving()`
- ✅ Release units from spawner with `release_from_spawner()`

**Cannot Control:**
- ❌ Prevent automatic group creation from spawners
- ❌ Modify gathering time parameters at runtime
- ❌ Change max group size for auto-created groups
- ❌ Interrupt gathering once started (except by destroying group)
- ❌ Modify group composition (unit types spawned)
- ❌ Control which units join which group

---

## 5. Rampant Integration Analysis

### 5.1 Current Rampant Implementation

Rampant intercepts native unit group events to create custom squads:

```lua
-- control.lua lines 750-834
local function onUnitGroupCreated(event)
    local group = event.group
    
    -- Only handle enemy force, non-script-driven groups
    if (group.force.name ~= "enemy") or group.is_script_driven then
        return
    end
    
    -- Find nearby base
    local base = findNearbyBase(chunk)
    if not base then
        group.destroy()  -- Reject groups without base association
        return
    end
    
    -- Create Rampant squad from native group
    squad = createSquad(nil, map, group, settler, base)
    Universe.groupNumberToSquad[group.group_number] = squad
end

local function onGroupFinishedGathering(event)
    local group = event.group
    local squad = Universe.groupNumberToSquad[group.group_number]
    
    if squad then
        squadDispatch(squad, event.tick)  -- Custom dispatch logic
    end
end

-- Event registration (lines 1095, 1098)
script.on_event(defines.events.on_unit_group_created, onUnitGroupCreated)
script.on_event(defines.events.on_unit_group_finished_gathering, onGroupFinishedGathering)
```

### 5.2 Rampant Squad Creation

Rampant can also create groups manually:

```lua
-- libs/Squad.lua lines 961-1007
function Squad.createSquad(position, map, group, settlers, base)
    local unitGroup = group
    if not unitGroup then
        -- Create new group if not provided
        local query = Queries.createUnitGroup
        setPositionInQuery(query, position)
        unitGroup = map.surface.create_unit_group(query)
    end
    
    -- Wrap in Rampant squad structure
    local squad = {
        group = unitGroup,
        status = SQUAD_GUARDING,
        base = base,
        -- ... other squad properties
    }
    
    return squad
end
```

### 5.3 Integration Points for Supply Tracking

**Recommended Integration Strategy:**

1. **Monitor Native Spawning**
   - Track `on_unit_group_created` events
   - Log spawner-to-group relationships
   - Monitor group sizes and composition

2. **Intercept Group Dispatch**
   - Use `on_unit_group_finished_gathering` to control squad deployment
   - Destroy groups that don't meet supply requirements
   - Redirect groups based on supply availability

3. **Manual Group Creation**
   - Create groups manually when supply permits
   - Control exact group size and timing
   - Set custom commands for tactical behavior

4. **Spawner Management**
   - Monitor spawner health and unit capacity
   - Destroy spawners to limit supply (if needed)
   - Track `max_count_of_owned_units` per spawner

### 5.4 What Rampant Can Implement

**Feasible with Current API:**
- ✅ Supply-based group creation (manual groups only)
- ✅ Group size limits via `add_member()` control
- ✅ Group dispatch timing via event handlers
- ✅ Group destruction when supply insufficient
- ✅ Squad reassignment between bases
- ✅ Custom movement/attack commands

**Requires Engine-Level Changes:**
- ❌ Direct control of spawner spawn rates
- ❌ Modification of pollution absorption
- ❌ Prevention of automatic group creation
- ❌ Real-time evolution factor manipulation
- ❌ Custom unit types in native groups

---

## 6. Recommendations for Supply Tracking System

### 6.1 Short-Term Implementation

1. **Enhanced Event Monitoring**
   ```lua
   -- Track all group creation events
   script.on_event(defines.events.on_unit_group_created, function(event)
       local group = event.group
       if group.is_script_driven then return end
       
       -- Log spawner association
       local spawner = group.spawner
       if spawner then
           recordSpawnerActivity(spawner.unit_number, group.group_number)
       end
   end)
   ```

2. **Supply-Gated Group Dispatch**
   ```lua
   -- In onGroupFinishedGathering
   if not hasSufficientSupply(squad.base) then
       squad.group.destroy()  -- Cancel group
       recordCancelledGroup(squad)
       return
   end
   ```

3. **Manual Group Creation for Controlled Attacks**
   ```lua
   -- When supply permits
   if hasSufficientSupply(base) and shouldLaunchAttack(base) then
       local group = surface.create_unit_group({
           position = base.position,
           force = "enemy"
       })
       
       -- Add specific units to group
       for _, unit in ipairs(getAvailableUnits(base)) do
           group:add_member(unit)
       end
       
       -- Set attack command
       group:set_command({
           type = defines.command.attack_area,
           radius = 100,
           distraction = defines.distraction.by_anything
       })
   end
   ```

### 6.2 Long-Term Considerations

1. **Prototype Modifications**
   - Consider custom spawner prototypes with modified `max_count_of_owned_units`
   - Adjust `spawning_cooldown` values for different difficulty levels
   - Create specialized spawner types for different unit compositions

2. **Hybrid Approach**
   - Suppress native spawners (destroy on spawn)
   - Implement custom spawning via manual group creation
   - Full control over supply, timing, and composition

3. **Remote Interface**
   - Expose supply tracking via `remote.add_interface()`
   - Allow other mods to query supply status
   - Enable coordinated multi-mod AI behavior

---

## 7. Summary: Control Matrix

| Feature | Lua Control | Hardcoded | Notes |
|---------|-------------|-----------|-------|
| Spawn triggering | ❌ No | ✅ Yes | Engine-only logic |
| Spawn cooldown | ❌ No | ✅ Yes | Prototype-only |
| Unit composition | ❌ No | ✅ Yes | Evolution-based |
| Group creation | ✅ Partial | ✅ Yes | Manual only |
| Group size | ✅ Yes (manual) | ✅ Yes (auto) | Max 200 for auto |
| Gathering time | ❌ No | ✅ Yes | Map settings only |
| Group commands | ✅ Yes | ❌ No | Full control |
| Group destruction | ✅ Yes | ❌ No | Can cancel any |
| Spawner capacity | ❌ No | ✅ Yes | Prototype-only |
| Pollution absorption | ❌ No | ✅ Yes | Prototype-only |
| Evolution factor | ⚠️ Read-only | ✅ Yes | Can modify via API |
| Member addition | ✅ Yes | ❌ No | Same force/surface |
| Member removal | ⚠️ Indirect | ❌ No | Destroy group |
| Autonomous mode | ✅ Yes | ❌ No | `set_autonomous()` |

---

## 8. References

### Factorio API Documentation
- LuaSurface.create_unit_group: https://lua-api.factorio.com/latest/classes/LuaSurface.html#create_unit_group
- LuaCommandable: https://lua-api.factorio.com/latest/classes/LuaCommandable.html
- LuaForce evolution methods: https://lua-api.factorio.com/latest/classes/LuaForce.html#get_evolution_factor
- UnitGroupSettings: https://lua-api.factorio.com/latest/types/UnitGroupSettings.html
- Events: https://lua-api.factorio.com/latest/events.html#on_unit_group_created

### Source Files Analyzed
- `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaSurface.lua`
- `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaCommandable.lua`
- `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaForce.lua`
- `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/events.lua`
- `/mnt/gallery/gameFiles/factorio/data/base/prototypes/entity/enemies.lua`
- `/mnt/gallery/gameFiles/factorio/data/base/prototypes/map-settings.lua`
- `/mnt/gallery/gameFiles/factorio/data/base/prototypes/entity/biter-ai-settings.lua`
- `/mnt/backpack/code/games/factorio/Rampant/control.lua`
- `/mnt/backpack/code/games/factorio/Rampant/libs/Squad.lua`
- `/mnt/backpack/code/games/factorio/Rampant/libs/Upgrade.lua`

---

**Document Version:** 1.0
**Last Updated:** 2026-02-28
**Author:** AI Exploration Agent