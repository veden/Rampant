# Current Pillars

## Document Purpose

This document describes the **eight core pillars** of Rampant's Enemy Logistics System. Each pillar includes detailed gameplay mechanics, design intent, and implementation considerations. This is the "how" of the system, grounded in the vision from the Theory document and bounded by the constraints from the Assumptions document.

---

## Pillar 1: Dynamic Network Corridors → Territory Supply Lines

### **Vision**
Enemy supply routes are **procedurally generated** based on Command Center territory overlap. **Physical connector entities** span between Command Centers where territories overlap, creating visible, defendable, interruptible supply lines. Territory pheromone flows through these connections.

### **Gameplay Mechanics**

#### **Network Structure**
- **Command Centers**: Strategic anchors that emit territory pheromone
- **Physical Connectors**: Destroyable entities that appear where territories overlap
- **Territory Lines**: Ground markings showing supply flow between Command Centers
- **Resource Nodes**: Attach to territory (not Command Center proximity)

#### **Connection Rules**
- **Implicit Distance**: Connectors form where territory pheromone from two CCs overlaps sufficiently
- **Territory Threshold**: Connectors exist where combined territory strength > 40 (20 from each CC)
- **Physical Presence**: Connectors are destroyable entities (not just visual)
- **Auto-Disconnect**: Connectors auto-remove when surrounding territory < 20 (no manual cleanup needed)
- **Supply Flow**: Territory pheromone flows through connectors (visible as ground markings)
- **Network Topology**: Hierarchical (Resource CCs → Regular CCs)
- **Emergent Behavior**: Connection distance emerges from pheromone diffusion (not hard-coded)

#### **Player Interaction**
- **Observation**: Physical connectors + ground markings show supply paths
- **Disruption**: Destroy connectors to break supply lines
- **Defense**: Place turrets along corridors to interdict supply
- **Exploitation**: Cut connections to isolate Command Centers, starve downstream CCs
- **Natural Decay**: Connectors auto-remove when territory decays (no player action needed)

### **Prototype Allocation**
| Entity | Count | Notes |
|--------|-------|-------|
| **Command Center** | 15 | 3 types × 5 tiers (Resource, Regular, Super) |
| **Connector** | 10 | 5 types × 2 tiers (basic, reinforced) |
| **Resource Node** | 20 | 4 resource types × 5 tiers (territory-attached) |
| **Territory Markers** | 20 | Ground decoratives for supply line visualization |
| **Total** | **65** | |

### **Technical Implementation**

**Pheromone-Driven Connector System (No Hard-Coded Radii)**:

```lua
-- Territory pheromone diffuses naturally (no explicit radius)
function processPheromone(chunk, tick)
    -- Existing diffusion logic...
    
    -- Territory pheromone diffuses to neighbors
    local territory_sum = 0
    local count = 0
    for _, neighbor in ipairs(get_neighbors(chunk)) do
        territory_sum = territory_sum + (neighbor[TERRITORY_PHEROMONE] or 0)
        count = count + 1
    end
    
    -- Average with decay (5% per tick = ~10 minute half-life)
    chunk[TERRITORY_PHEROMONE] = (territory_sum / count) * 0.95
end

-- Connector lifecycle (integrated with pheromone processing)
function processConnectors(tick)
    if tick % 60 ~= 0 then return end
    
    for _, connector in ipairs(all_connectors) do
        if not connector.valid then
            -- Connector was destroyed (player or auto-remove)
            remove_ground_markings(connector.position)
            all_connectors[connector.unit_number] = nil
            return
        end
        
        -- Check territory strength around connector (no radius check needed)
        local chunk = get_chunk_by_position(connector.position)
        local territory_strength = chunk[TERRITORY_PHEROMONE] or 0
        
        -- Auto-remove if territory too weak (emergent disconnect)
        if territory_strength < 20 then
            connector.destroy()
            remove_ground_markings(connector.position)
            all_connectors[connector.unit_number] = nil
            return
        end
        
        -- Connector is valid, keep it (distance emerges from pheromone diffusion)
    end
end

-- Connector placement (event-driven, when CCs are created/territory expands)
function try_place_connector(cc1, cc2)
    -- Find where territory pheromone from both CCs overlaps
    local search_radius = 50  -- Search locally for overlap point
    local best_position = nil
    local best_strength = 0
    
    for x = -search_radius, search_radius do
        for y = -search_radius, search_radius do
            local pos = {cc1.position.x + x, cc1.position.y + y}
            local chunk = get_chunk_by_position(pos)
            
            -- Check if both CCs contribute to this chunk's territory
            local strength = chunk[TERRITORY_PHEROMONE] or 0
            
            if strength > best_strength and strength > 40 then
                best_strength = strength
                best_position = pos
            end
        end
    end
    
    if not best_position then return end  -- No overlap found
    
    -- Check if connector already exists
    local existing = surface.find_entities_filtered{
        position = best_position,
        radius = 5,
        name = "rampant-connector"
    }
    
    if #existing > 0 then return end  -- Already exists
    
    -- Place new connector at overlap point
    local connector = surface.create_entity{
        name = "rampant-connector",
        position = best_position,
        force = "enemy"
    }
    
    -- Place ground markings between CCs
    place_ground_markings(cc1.position, cc2.position)
    
    -- Track connector
    all_connectors[connector.unit_number] = {
        cc1 = cc1,
        cc2 = cc2,
        created = game.tick
    }
end
```

**Key Points**:
- **No hard-coded radii**: Territory emerges from pheromone diffusion (5% decay per tick)
- **Implicit distance**: Connection distance emerges from where pheromone overlaps sufficiently
- **Auto-removal**: Connectors self-remove when territory < 20 (no manual tracking needed)
- **Event-driven placement**: Connectors placed when CCs created or territory expands
- **Performance**: Same cost as pheromone processing (already happening every 60 ticks)
- **Emergent behavior**: Pheromone diffusion naturally creates connection patterns

### **Design Decisions**
- **Emergent Distance**: Connection distance emerges from pheromone diffusion (no hard-coded value)
- **Visual Style**: Physical connectors + ground markings (both required for visibility)
- **Auto-Disconnect**: Connectors auto-remove when territory < 20 (no manual cleanup needed)
- **Physical + Abstract**: Physical connectors for interaction, ground markings for clarity
- **Integration**: Connector checks integrated with pheromone processing (no separate loop)
- **Pheromone-Driven**: All connection logic driven by pheromone strength (not distance calculations)

### **Critical Principle**
**Physical Visibility Principle**: Players must have physical entities to target. Pheromone cloud alone is insufficient - we need both physical connectors AND pheromone logic.

**Connector Lifecycle**: Connectors are passive entities that auto-manage based on territory strength. No manual cleanup loop needed - they self-remove when territory decays.

---

## Pillar 2: Unit Archetypes with Tier Progression → Supply-Gated Progression

### **Vision**
9 distinct unit roles × 5 tiers = 45 base unit types. Each role has **unique behavior, counterplay, and visual identity**. **Tier access is gated by Command Center supply level** - low supply forces enemy to use lower-tier units.

### **The 9 Archetypes**

#### **Scout**
- **Behavior**: Fast, light, reconnaissance
- **Purpose**: Reveal player positions, test defenses, call reinforcements
- **Counterplay**: Area denial, early warning turrets
- **Stats**: High speed, low health, low damage
- **Visual**: Small, sleek, fast movement animation

#### **Combat**
- **Behavior**: Balanced, standard attack
- **Purpose**: Backbone of enemy forces, general-purpose combat
- **Counterplay**: Standard defenses (machine guns, shotguns)
- **Stats**: Medium speed, medium health, medium damage
- **Visual**: Standard biter/spitter appearance

#### **Tank**
- **Behavior**: Slow, high damage, high health
- **Purpose**: Siege breaker, defense destroyer
- **Counterplay**: Anti-armor (explosives, lasers)
- **Stats**: Low speed, high health, high damage
- **Visual**: Large, armored plating, slow movement

#### **Ranged**
- **Behavior**: Long-range attack, stays behind front lines
- **Purpose**: Soften defenses before assault
- **Counterplay**: Close combat, cover, fast units
- **Stats**: Medium speed, low health, long-range attack
- **Visual**: Spitter-like, attack projectile visible

#### **Support**
- **Behavior**: Heals/buffs nearby allies, low health
- **Purpose**: Keep enemy forces fighting longer
- **Counterplay**: Priority target, burst damage
- **Stats**: Low speed, very low health, healing ability
- **Visual**: Distinctive color/particles, healing effect visible

#### **Builder**
- **Behavior**: Constructs buildings, repairs structures
- **Purpose**: Expand territory, repair damage
- **Counterplay**: Priority target during construction
- **Stats**: Medium speed, low health, building ability
- **Visual**: Tool-carrying, construction animation

#### **Carrier**
- **Behavior**: Transports supply between Command Centers
- **Purpose**: Maintain supply lines, reinforce distant CCs
- **Counterplay**: Intercept carriers to cut supply
- **Stats**: Medium speed, medium health, supply capacity
- **Visual**: Cargo-carrying, supply trail

#### **Elite**
- **Behavior**: High-tier variant of other roles
- **Purpose**: Late-game threat, boss companions
- **Counterplay**: Specialized weapons, coordinated attacks
- **Stats**: High speed, high health, high damage
- **Visual**: Distinctive armor, glowing effects

#### **Boss**
- **Behavior**: Unique mechanics, high health, special abilities
- **Purpose**: Superstructure guardians, raid bosses
- **Counterplay**: Preparation required, specific counters
- **Stats**: Very high health, very high damage, abilities
- **Visual**: Large, distinctive, boss-like presence

### **Tier Progression**

| Tier | Supply Required | Stats Multiplier | Availability |
|------|----------------|------------------|--------------|
| **Tier 1** | 0+ | 100% | Always available |
| **Tier 2** | 20+ | 115% | Low supply |
| **Tier 3** | 40+ | 130% | Medium supply |
| **Tier 4** | 60+ | 145% | High supply |
| **Tier 5** | 80+ | 160% | Maximum supply |

### **Supply Gating Mechanism**

```lua
function getAvailableTier(command_center)
    local supply_level = command_center.supply.current / command_center.supply.max
    
    -- Supply level determines max tier
    if supply_level >= 0.8 then
        return 5  -- All tiers available
    elseif supply_level >= 0.6 then
        return 4  -- Tier 5 blocked
    elseif supply_level >= 0.4 then
        return 3  -- Tiers 4-5 blocked
    elseif supply_level >= 0.2 then
        return 2  -- Tiers 3-5 blocked
    else
        return 1  -- Only Tier 1 (desperate)
    end
end
```

### **Prototype Allocation**
| Unit Type | Count | Notes |
|-----------|-------|-------|
| **9 Roles × 5 Tiers** | 45 | Base unit types |
| **Variations per role** | 270 | 6 variations × 45 base types |
| **Boss variants** | 15 | 3 boss types × 5 tiers |
| **Total** | **330** | |

### **Technical Implementation**
- **Tier Selection**: Check Command Center supply before spawning
- **Prototype Selection**: Use tier-specific prototype (not runtime modification)
- **Supply Check**: Every spawn attempt validates tier availability
- **Fallback**: If supply too low, spawn lower tier (not abort spawn)

### **Design Decisions**
- **Tier Count**: 5 tiers (not 10) - faster progression, clearer decisions
- **Supply Gating**: Supply level directly controls tier access
- **Visible Progression**: Player sees enemy get weaker as supply drops
- **No Runtime Modification**: Use prototype variations, not stat modification

---

## Pillar 3: Resource-Driven Enemy Doctrine → Territory-Attached Resources

### **Vision**
Resources attach to **Command Center territory** (not proximity). Any Command Center whose territory contains a resource gains the resource's faction bonus. Multiple Command Centers can share the same resource via territory overlap.

### **Resource Types & Bonuses**

| Resource | Territory Bonus | Faction Spread | Unit Effect |
|----------|----------------|----------------|-------------|
| **Oil** | +50% territory strength | Acid faction | More spitters, acid damage |
| **Iron** | +30% unit health | Physical faction | More biters, armor plating |
| **Coal** | +20% territory expansion rate | Fire faction | Faster growth, fire units |
| **Uranium** | +40% elite unit spawn rate | Radiation faction | Elite units, radiation damage |
| **None** | Base territory strength | Base faction | Standard units only |

### **Resource Attachment Mechanism**

```lua
function detectResourcesInTerritory(command_center)
    local territory_radius = 150  -- tiles
    local resources = surface.find_entities_filtered{
        position = command_center.position,
        radius = territory_radius,
        type = "resource"
    }
    
    local resource_bonuses = {
        oil = 0,
        iron = 0,
        coal = 0,
        uranium = 0
    }
    
    for _, resource in ipairs(resources) do
        local resource_type = get_resource_type(resource.name)
        resource_bonuses[resource_type] = resource_bonuses[resource_type] + 1
    end
    
    return resource_bonuses
end

function calculateTerritoryStrength(command_center)
    local base_strength = 100
    local resource_bonuses = detectResourcesInTerritory(command_center)
    
    local oil_bonus = resource_bonuses.oil * 50
    local iron_bonus = resource_bonuses.iron * 30
    local coal_bonus = resource_bonuses.coal * 20
    local uranium_bonus = resource_bonuses.uranium * 40
    
    return base_strength + oil_bonus + iron_bonus + coal_bonus + uranium_bonus
end
```

### **Faction Spreading**

```lua
function spreadFactionThroughTerritory(command_center)
    local faction = get_dominant_faction(command_center)
    local territory_radius = 150
    
    -- Spread faction pheromone through territory
    for chunk in chunks_in_radius(command_center.position, territory_radius) do
        chunk[TERRITORY_FACTION] = faction
        chunk[TERRITORY_FACTION_STRENGTH] = get_faction_strength(chunk, command_center)
    end
end
```

### **Player Interaction**
- **Destroy resource node** → enemy loses bonus, player can mine enhanced patch
- **Block territory expansion** → prevent resource claims
- **Capture resource** → player gains access to enhanced patch (value remains)

### **Prototype Allocation**
| Entity | Count | Notes |
|--------|-------|-------|
| **Resource Nodes** | 20 | 4 types × 5 tiers (territory-attached) |
| **Faction Variants** | 150 | Shared pool, different ratios per resource |
| **Total** | **170** | |

### **Technical Implementation**
- **Resource Detection**: Scan territory radius when Command Center created (every 300 ticks)
- **Bonus Application**: Apply to all spawning within territory
- **Faction Spread**: TERRITORY_FACTION pheromone carries faction identity
- **Resource Destruction**: Node destroyed → bonus lost, patch enhanced for player

### **Design Decisions**
- **Attachment Model**: Resources attach to territory (not Command Center proximity)
- **Multiple CCs**: Multiple Command Centers can share same resource
- **Value Persistence**: Resource value remains after destruction (player can mine)
- **Faction Spread**: Faction spreads through territory pheromone (visible before attack)

---

## Pillar 4: Supply-Gated Warfare → Hierarchical Supply Network

### **Vision**
Supply flows hierarchically from **Resource Command Centers** (generate supply) → **Regular Command Centers** (consume supply) → **Nests** (spawn units). Supply lines are visible via physical connectors and ground markings.

### **Supply Network Topology**

```
Resource Command Center A (generates supply)
    ↓ [Supply Line 1]
Regular Command Center B (consumes, relays)
    ↓ [Supply Line 2]
Regular Command Center C (consumes)
    ↓
Nests spawn units (tier gated by supply level)
```

### **Supply Generation & Consumption**

```lua
function calculateSupplyGeneration(command_center)
    local base_supply = 10  -- Base generation per tick
    
    -- Resource bonuses
    local resource_bonuses = detectResourcesInTerritory(command_center)
    local oil_bonus = resource_bonuses.oil * 5
    local iron_bonus = resource_bonuses.iron * 3
    local coal_bonus = resource_bonuses.coal * 2
    local uranium_bonus = resource_bonuses.uranium * 8
    
    local total_supply = base_supply + oil_bonus + iron_bonus + coal_bonus + uranium_bonus
    
    return total_supply
end

function processSupplyFlow(tick)
    for _, cc in ipairs(all_command_centers) do
        local connections = findConnectedCommandCenters(cc)
        
        -- Calculate supply distribution
        local total_supply = calculateSupplyGeneration(cc)
        local supply_to_distribute = total_supply * 0.3  -- 30% shared with neighbors
        
        for _, connection in ipairs(connections) do
            local amount = supply_to_distribute / #connections
            
            -- Transfer supply via territory pheromone
            local territory_chunk = get_chunk_between(cc, connection.target)
            territory_chunk[TERRITORY_SUPPLY] = territory_chunk[TERRITORY_SUPPLY] + amount
        end
    end
end
```

### **Supply Disruption**

```lua
function onConnectorDestroyed(event)
    local connector = event.entity
    local cc1 = get_connected_cc(connector, 1)
    local cc2 = get_connected_cc(connector, 2)
    
    -- Break supply line
    disconnect_command_centers(cc1, cc2)
    
    -- Supply decays in downstream CCs
    for _, cc in ipairs(get_downstream_ccs(cc2)) do
        cc.supply.current = cc.supply.current * 0.5  -- Immediate 50% loss
        -- Then decays 5%/tick
    end
end
```

### **Player Interaction**
- **Destroy connector** → supply line breaks, downstream CCs starve
- **Destroy Resource CC** → network weakens globally
- **Block territory expansion** → prevent new supply routes
- **Intercept carriers** → disrupt supply flow

### **Prototype Allocation**
| Entity | Count | Notes |
|--------|-------|-------|
| **Connectors** | 10 | 5 types × 2 tiers |
| **Territory Markers** | 20 | Ground decoratives |
| **Total** | **30** | (shared with Pillar 1) |

### **Technical Implementation**
- **Supply Flow**: Process every 60 ticks (not per-tick)
- **Disruption**: Event-driven (on connector/CC destruction)
- **Decay**: 5%/tick when supply cut (visible degradation)
- **Visualization**: Ground markings show supply flow strength

### **Design Decisions**
- **Hierarchy**: Resource CCs → Regular CCs → Nests (clear flow)
- **Supply Lines**: Physical connectors + ground markings (both required)
- **Disruption**: Destroy connector → immediate 50% loss + 5%/tick decay
- **Visibility**: Supply strength visible via ground markings (optional scan)

---

## Pillar 5: Offensive Action = Strategic Rewards → Technology Loop + Resource Capture

### **Vision**
Destroying Command Centers drops **Technology Modules** (consumable items). Players process modules into research catalysts, which unlock technologies. Destroying resource nodes leaves **enhanced patches** that players can mine.

### **Technology Reward Loop**

```
Destroy Resource Command Center
    ↓
Drop "Technology Module" (item)
    ↓
Player carries to factory
    ↓
Process: Module + Chemical Science → Research Catalyst (30 seconds)
    ↓
Player puts Catalyst in lab
    ↓
Lab consumes Catalyst during research (+50% speed)
    ↓
Research completes → Unlocks player technology
```

### **Technology Module Types**

| Module Source | Technology Unlocked | Rarity |
|--------------|-------------------|--------|
| **Oil CC** | Acid resistance, chemical weapons | Common |
| **Iron CC** | Armor plating, physical weapons | Common |
| **Coal CC** | Fire weapons, expansion speed | Uncommon |
| **Uranium CC** | Radiation weapons, elite counters | Rare |
| **Superstructure** | Advanced tech, unique equipment | Legendary |

### **Resource Capture**

```lua
function onResourceNodeDestroyed(event)
    local node = event.entity
    local surface = node.surface
    local position = node.position
    
    -- Destroy original node
    node.destroy()
    
    -- Create enhanced version (player can mine)
    local enhanced_node = surface.create_entity{
        name = node.name .. "-enhanced",
        position = position,
        force = "neutral",
        amount = node.amount * 2  -- 2x value
    }
    
    -- Drop technology module
    surface.create_entity{
        name = "rampant-technology-module-" .. get_resource_type(node.name),
        position = position,
        force = "player"
    }
end
```

### **Player Interaction**
- **Destroy CC** → Get technology module
- **Process module** → Research catalyst (factory time)
- **Research** → Unlock technology (RampantArsenal integration)
- **Capture resource** → Mine enhanced patch (2x value)

### **Prototype Allocation**
| Entity | Count | Notes |
|--------|-------|-------|
| **Technology Modules** | 5 | 4 resource types + superstructure |
| **Enhanced Resources** | 4 | Enhanced versions of each resource |
| **Total** | **9** | |

### **Technical Implementation**
- **Drop Logic**: On CC destruction (event-driven)
- **Processing**: New recipe in RampantArsenal (factory time)
- **Research**: Catalyst consumed in lab, speeds up research
- **Enhanced Patches**: New entity prototypes (2x resource amount)

### **Design Decisions**
- **Consumable Flow**: Module → Process → Research (logistics loop)
- **Enhanced Patches**: Value remains after destruction (player incentive)
- **Technology Unlocks**: Player choice (add to queue, research when ready)
- **Rarity**: Common (Oil/Iron), Uncommon (Coal), Rare (Uranium), Legendary (Boss)

---

## Pillar 6: Escalation & Superstructures → Construction Telegraphing

### **Vision**
Superstructures (Queens, Titans) are **upgraded Command Centers** that spawn bosses. Construction is telegraphed via **growing territory pheromone** and **physical construction phases** (foundation → frame → active).

### **Construction Phases**

| Phase | Duration | Health | Territory Effect | Player Choice |
|-------|----------|--------|------------------|---------------|
| **Foundation** | 3 minutes | 100 | Weak pheromone burst | Easy to destroy |
| **Frame** | 5 minutes | 500 | Medium pheromone growth | Medium difficulty |
| **Active** | 2 minutes | 1000 | Strong pheromone burst | Hard to destroy |
| **Complete** | - | - | Boss spawns, territory strengthens | Boss fight |

### **Construction Telegraphing**

```lua
function startSuperstructureConstruction(command_center)
    -- Phase 1: Foundation
    local foundation = surface.create_entity{
        name = "rampant-superstructure-foundation",
        position = command_center.position,
        force = "enemy",
        health = 100
    }
    
    -- Emit weak territory pheromone
    local chunk = get_chunk_by_position(command_center.position)
    chunk[TERRITORY_PHEROMONE] = chunk[TERRITORY_PHEROMONE] + 100
    
    -- Audio cue
    surface.play_sound{
        path = "rampant/superstructure-construction-phase1",
        position = command_center.position
    }
    
    -- Chat notification
    game.print("Enemy constructing Superstructure at [gps=" .. command_center.position.x .. "," .. command_center.position.y .. "]")
    
    -- Schedule next phase
    schedule_phase_transition(command_center, foundation, 2, 180)  -- 3 minutes
end
```

### **Player Choice**

| Timing | Difficulty | Reward | Risk |
|--------|-----------|--------|------|
| **During construction** | Easy (foundation/frame) | Moderate technology module | Low (easy fight) |
| **After completion** | Hard (boss fight) | Rare technology module + boss loot | High (boss patrols) |
| **Ignore** | - | None | Very High (boss patrols indefinitely) |

### **Prototype Allocation**
| Entity | Count | Notes |
|--------|-------|-------|
| **Superstructure Phases** | 12 | 4 boss types × 3 phases |
| **Boss Variants** | 15 | 3 boss types × 5 tiers |
| **Total** | **27** | |

### **Technical Implementation**
- **Phase Progression**: Timer-based (not health-based)
- **Pheromone Growth**: Territory pheromone increases each phase
- **Audio Cues**: Distinctive construction sounds (telegraphing)
- **Chat Notifications**: Player notified of construction location

### **Design Decisions**
- **Construction Time**: 10 minutes total (3+5+2)
- **Telegraphing**: Pheromone growth + audio + chat (multiple signals)
- **Player Choice**: Attack early (easy) or wait (hard, better reward)
- **Boss Behavior**: Patrols area after completion (ongoing threat)

---

## Pillar 7: Dynamic Territory & Captured Outposts → Pheromone Territory System

### **Vision**
Territory is **invisible pheromone cloud** that flows from Command Centers. Players can **scan** to reveal territory levels. Enemy can **gradually overwrite** player territory (not instant flip).

### **Territory Pheromone Types**

| Pheromone | Purpose | Decay Rate | Visibility |
|-----------|---------|------------|------------|
| **TERRITORY_PHEROMONE** | Territory strength | 5%/tick when CC destroyed | Scan to reveal |
| **TERRITORY_FACTION** | Faction identity | 10%/tick | Inferred from units |
| **TERRITORY_SUPPLY** | Supply level | 5%/tick | Scan to reveal |

### **Territory Overwrite Mechanism**

```lua
function processTerritoryOverwrite(tick)
    for _, chunk in ipairs(all_chunks) do
        local enemy_territory = chunk[TERRITORY_PHEROMONE] or 0
        local player_territory = chunk[PLAYER_PHEROMONE] or 0
        
        -- Gradual overwrite (not instant)
        if enemy_territory > player_territory * 1.5 then
            -- Enemy territory slowly overwrites player
            chunk[PLAYER_PHEROMONE] = chunk[PLAYER_PHEROMONE] * 0.95
            chunk[TERRITORY_PHEROMONE] = chunk[TERRITORY_PHEROMONE] * 1.05
        elseif player_territory > enemy_territory * 1.5 then
            -- Player territory slowly overwrites enemy
            chunk[TERRITORY_PHEROMONE] = chunk[TERRITORY_PHEROMONE] * 0.95
            chunk[PLAYER_PHEROMONE] = chunk[PLAYER_PHEROMONE] * 1.05
        end
    end
end
```

### **Player Interaction**
- **Build in territory** → increases player pheromone, reduces enemy territory
- **Destroy Command Center** → enemy territory decays (5%/tick)
- **Scan territory** → reveals pheromone levels (optional visibility)
- **Reclaim territory** → build structures, push back enemy territory

### **Prototype Allocation**
| Entity | Count | Notes |
|--------|-------|-------|
| **Territory Markers** | 20 | Ground decoratives (optional) |
| **Scan Devices** | 5 | Player tools for territory visibility |
| **Total** | **25** | |

### **Technical Implementation**
- **Territory Flow**: Process every 60 ticks (not per-tick)
- **Overwrite Rate**: 5%/tick gradual change (not instant)
- **Scan Mechanic**: Player tool reveals pheromone levels (optional)
- **Visualization**: Ground markings optional (not required for gameplay)

### **Design Decisions**
- **Visibility**: Invisible by default, scan to reveal (optional)
- **Overwrite**: Gradual (5%/tick), not instant
- **Reclaim**: Player builds to push back enemy territory
- **No Instant Flip**: Territory changes gradually (fair, learnable)

---

## Pillar 8: Mobile Threats & Adaptive Tactics → Faction Spreading Through Network

### **Vision**
Enemy adapts by **spreading new faction through territory pheromone**. Adaptation is **telegraphed** via TERRITORY_FACTION changes before attack composition shifts. Space Age units (demolishers, spider-units) used if available, fallback to Rampant-original equivalents.

### **Adaptation Mechanism**

```lua
function processAdaptation(tick)
    for _, cc in ipairs(all_command_centers) do
        -- Check if adaptation needed
        local attack_history = cc.attack_history
        if #attack_history >= 3 then
            -- Enemy adapted 3 times with same defense
            local player_defense = get_player_defense_type(attack_history)
            local new_faction = get_counter_faction(player_defense)
            
            -- Start spreading new faction
            cc.adapting_faction = new_faction
            cc.adaptation_timer = 600  -- 10 minutes before full adaptation
        end
    end
end

function spreadAdaptation(command_center)
    local new_faction = command_center.adapting_faction
    
    -- Spread via TERRITORY_FACTION pheromone
    for chunk in chunks_in_radius(command_center.position, 150) do
        chunk[TERRITORY_FACTION] = new_faction
        chunk[TERRITORY_FACTION_STRENGTH] = chunk[TERRITORY_FACTION_STRENGTH] + 0.1
    end
end
```

### **Telegraphing**

| Time Before Adaptation | Signal | Player Response |
|----------------------|--------|-----------------|
| **10 minutes** | TERRITORY_FACTION starts changing | Notice faction shift |
| **5 minutes** | Construction activity near CC | Prepare for new units |
| **2 minutes** | Scout waves appear | Adjust defenses |
| **0 minutes** | New composition arrives | Fight adapted units |

### **Space Age Integration**

| Unit Type | Space Age | Base Game Fallback |
|-----------|-----------|-------------------|
| **Demolisher** | ✅ Use Space Age demolisher | Rampant-heavy-spawner |
| **Spider-unit** | ✅ Use Space Age stomper | Rampant-fast-swarm |
| **Wasp** | ✅ Use Space Age wasp | Rampant-air-worm |
| **Standard** | ✅ Use if available | Rampant-original |

### **Prototype Allocation**
| Entity | Count | Notes |
|--------|-------|-------|
| **Space Age Units** | 50 | Conditional (if Space Age present) |
| **Base Game Fallbacks** | 75 | Rampant-original equivalents |
| **Total** | **125** | (conditional allocation) |

### **Technical Implementation**
- **Adaptation Logic**: Track player defense, shift faction after 2-3 attacks
- **Telegraphing**: TERRITORY_FACTION changes visible via scan
- **Space Age Detection**: Runtime check for prototype existence
- **Fallback**: Rampant-original units if Space Age not present

### **Design Decisions**
- **Adaptation Speed**: 2-3 attacks before shift (balanced)
- **Telegraphing**: 10-minute warning via pheromone + construction + scouts
- **Space Age**: Conditional (use if available, fallback otherwise)
- **Faction Spread**: TERRITORY_FACTION pheromone carries adaptation

---

## Summary: Total Prototype Budget

| Pillar | Prototype Count | Notes |
|--------|----------------|-------|
| **1. Network** | 65 | Command Centers, connectors, resource nodes |
| **2. Units** | 330 | 9 roles × 5 tiers × 6 variations + bosses |
| **3. Resources** | 170 | Resource nodes, faction variants |
| **4. Supply** | 30 | Shared with Pillar 1 |
| **5. Rewards** | 9 | Technology modules, enhanced resources |
| **6. Escalation** | 27 | Superstructure phases, bosses |
| **7. Territory** | 25 | Territory markers, scan devices |
| **8. Adaptation** | 125 | Space Age + base game fallbacks |
| **TOTAL** | **781** | Within 750-900 target |

---

## Document History

- **v1.0** (Initial): Basic pillar definitions
- **v1.1** (Updated): Refined with Command Center system, territory pheromone, supply network, resource attachment, physical connectors required, updated prototype budget