# System Architecture and Integration

## Document Purpose

This document synthesizes all learnings about Rampant's existing systems and how the new Enemy Logistics System integrates with them. It serves as the **technical blueprint** for implementation, capturing design decisions, integration patterns, and critical assumptions.

---

## 1. Core Architecture: Three Interlocking Systems

### 1.1 The Pheromone Cloud (Backend Logic)

**Essence**: *The environment's memory - eventually consistent, distributed state encoding "where things are important"*

**Existing System**:
- **40 chunks/tick** processing (distributed cost, not all-at-once)
- **Decay factors**: PLAYER (0.98), BASE (0.90), KAMIKAZE (0.95) retention
- **Neighbor averaging**: Creates smooth gradients, no hard boundaries
- **5 pheromone types**: PLAYER, BASE, ENEMY, KAMIKAZE, RESOURCE

**Enhanced System**:
- **3 new pheromone types**:
  - `TERRITORY_PHEROMONE`: Territory strength (0-100)
  - `TERRITORY_FACTION`: Faction identity (acid=1, laser=2, etc.)
  - `TERRITORY_SUPPLY`: Supply level (0.0-1.0)
- **Same diffusion algorithm**: Just more variables in existing loop
- **No separate processing**: Integrated into existing 40-chunks/tick

**Key Pattern**: *Pheromones are backend logic - invisible, but drive all behavior*

```lua
-- Existing pattern (MapUtils.processPheromone)
function processPheromone(chunk, tick)
    -- Get 8 neighbors
    -- Average each pheromone with decay
    -- Store back to chunk
    
    -- NEW: Add territory pheromones (same pattern)
    local territory_sum = 0
    for _, neighbor in ipairs(get_neighbors(chunk)) do
        territory_sum = territory_sum + (neighbor[TERRITORY_PHEROMONE] or 0)
    end
    chunk[TERRITORY_PHEROMONE] = (territory_sum / 8) * 0.95  -- 5% decay
end
```

---

### 1.2 Temperament + Independent Triggers (Base Logic)

**Essence**: *Per-base mood (temperament) scaled by activity metrics (triggers)*

**Existing System**:
- **Per-base tracking**: Each base has `temperamentScore` (-10000 to +10000) → `temperament` (0.0-1.0)
- **Event-driven**: Destroyed buildings, lost units, rocket launches affect score
- **State machine**: Temperament ranges determine AI state (PEACEFUL, AGGRESSIVE, RAIDING, ONSLAUGHT)
- **Evolution scaling**: `planning(evolutionLevel)` called every tick, drives wave sizes with power laws

**Enhanced System**:
- **Independent triggers modulate evolution** (don't replace):
  - Building count → network growth rate
  - Technology tier → building health scaling
  - Pollution output → spawn rates
  - Combat events → unit health scaling
  - Offense/defense ratio → escalation timer
  - Tactic consistency → adaptation speed
- **Triggers affect temperament**: Low supply → temperament -1000 (desperate)
- **Caps prevent runaway**: Health 1.0x-2.0x, spawn rates 0.5x-3.0x

**Key Pattern**: *Evolution sets baseline, triggers fine-tune based on HOW player plays*

```lua
-- Existing pattern (BaseUtils.temperamentPlanner)
function temperamentPlanner(base, evolutionLevel)
    -- Event-based delta calculation
    local delta = 0
    if activeNests > 0 then delta = delta + (5.76 * activeNests) end
    if destroyPlayerBuildings > 0 then delta = delta - (5.55 * destroyPlayerBuildings) end
    
    base.temperamentScore = base.temperamentScore + delta
    base.temperament = normalize(base.temperamentScore)
    
    -- NEW: Independent triggers modulate scaling
    base.supplyLevel = calculateSupplyLevel(playerActivity.buildingCount)
    base.healthMultiplier = calculateHealthMultiplier(playerActivity.techTier)
    
    -- NEW: Supply affects temperament
    if base.supplyLevel < 0.3 then
        base.temperamentScore = base.temperamentScore - 1000  -- Desperate
    end
end
```

---

### 1.3 Physical Manifestation (Frontend Targets)

**Essence**: *Visible, destroyable entities that embody pheromone states*

**New System**:
- **Command Centers**: Emit TERRITORY_PHEROMONE, anchor territory
- **Connectors**: Spawn where territory overlap > 40, gradual health buildup (0%→100% over 5 min)
- **Resource Nodes**: ON ore patches, attach via territory overlap, drop Technology Modules
- **Ground Markings**: Decals showing TERRITORY_SUPPLY flow (stone-path-decal, tinted)

**Integration Pattern**: *Event-driven placement, pheromone-guided lifecycle*

```lua
-- Connector creation (implicit during pheromone diffusion)
function processPheromone(chunk, tick)
    -- ... existing diffusion ...
    
    -- NEW: Check for connector creation
    if chunk[TERRITORY_PHEROMONE] > 40 and not hasConnectorAt(chunk) then
        scheduleConnectorCreation(chunk, {
            health = 0,  -- Starts at 0%
            buildupRate = 10  -- +10% every 300 ticks
        })
    end
end

-- Connector destruction (event-driven)
script.on_event(defines.events.on_entity_died, function(event)
    if event.entity.name:match("rampant-connector") then
        local connector = getConnectorByEntity(event.entity)
        local downstreamCC = connector.downstreamCC
        
        -- Immediate: Cut supply
        downstreamCC.supply.current = 0
        
        -- Propagating: Inject disruption
        local chunk = getChunkByPosition(event.entity.position)
        chunk[TERRITORY_SUPPLY] = 0
        
        -- State: Mark broken
        connector.upstreamCC.connections[downstreamCC.id].broken = true
    end
end)
```

---

### 1.4 Squad Movement (3-Step Lookahead)

**Essence**: *Pheromone-guided gradient following with limited foresight*

**Existing System**:
- **3-step lookahead**: `scoreNeighbors()` scores up to 4 chunks ahead
- **Status-dependent scoring**:
  - RAIDING: Follow PLAYER_PHEROMONE + BASE_PHEROMONE
  - SETTLING: Follow RESOURCE_PHEROMONE
  - RETREATING: Follow KAMIKAZE_PHEROMONE
- **Same algorithm**: Different scoring functions for different objectives

**Enhanced System**:
- **New scoring functions** (plug into existing 3-step lookahead):
  - SUPPLY_CARRIER: Follow TERRITORY_SUPPLY gradient
  - SETTLING (enhanced): Follow RESOURCE_PHEROMONE + TERRITORY_PHEROMONE
- **No pathfinding changes**: Same 3-step algorithm, new pheromone types in scores

**Key Pattern**: *New pheromone types plug into existing scoring - no pathfinding rebuild*

```lua
-- Existing pattern (Squad.scoreAttackLocation)
function scoreAttackLocation(neighborChunk)
    local damage = neighborChunk[BASE_PHEROMONE] +
                   (neighborChunk[PLAYER_PHEROMONE] * 100)
    return damage, preferred
end

-- NEW: Supply carrier scoring
function scoreSupplyLocation(neighborChunk)
    local score = neighborChunk[TERRITORY_SUPPLY] * 2.0
    
    -- Prefer chunks with connectors
    if hasConnectorTo(currentChunk, neighborChunk) then
        score = score + 100
    end
    
    -- Avoid low-supply chunks
    if neighborChunk[TERRITORY_SUPPLY] < 0.2 then
        score = score - 50
    end
    
    return score, false
end

-- Existing squadDispatch() uses these scoring functions
function Squad.squadDispatch(squad, tick)
    if squad.status == SQUAD_RAIDING then
        attackMove(squad, tick)  -- Uses scoreAttackLocation
    elseif squad.status == SQUAD_SUPPLY_CARRIER then  -- NEW
        supplyMove(squad, tick)  -- Uses scoreSupplyLocation
    end
end
```

---

## 2. Critical Design Decisions

### 2.1 Evolution vs. Independent Triggers

**Decision**: **Modulate, don't replace**

- Evolution remains master scaling factor
- Triggers fine-tune (e.g., `final_scaling = evolution × trigger_modifier`)
- **Rationale**: Preserves existing balance, gradual transition, easier to debug

### 2.2 Temperament Integration

**Decision**: **Direct modifier**

- Low supply → temperament -1000 (desperate)
- Fragmented network → temperament -500 (vulnerable)
- **Rationale**: Simple, integrates with existing state machine, no parallel systems

### 2.3 Squad Movement Enhancement

**Decision**: **New scoring functions, keep 3-step lookahead**

- Add `scoreSupplyLocation()`, `scoreTerritoryLocation()`
- Same 3-step algorithm, new pheromone types in scores
- **Rationale**: Minimal changes, leverages existing infrastructure, cost-effective

### 2.4 Connector Health Buildup

**Decision**: **Gradual (0% → 100% over 5 minutes)**

- Player can interrupt during construction
- Visual feedback (construction animation)
- No supply flow until complete
- **Rationale**: Tactical depth, visible progression, satisfying gameplay

### 2.5 Health Scaling Curve

**Decision**: **Diminishing returns with 2.0x cap**

- Tier 0: 100%, Tier 1: 140%, Tier 2: 170%, Tier 3: 190%, Tier 4: 200%
- **Rationale**: Prevents late-game health sponges, balanced challenge

---

## 3. Integration Patterns

### 3.1 Pheromone Integration

**Pattern**: *Add new types to existing diffusion loop*

```lua
-- Before: 5 pheromone types
chunk[PLAYER_PHEROMONE] = (player_sum / 8) * 0.98
chunk[BASE_PHEROMONE] = (base_sum / 8) * 0.90
chunk[ENEMY_PHEROMONE] = (enemy_sum / 8) * 0.92
chunk[KAMIKAZE_PHEROMONE] = (kamikaze_sum / 8) * 0.95
chunk[RESOURCE_PHEROMONE] = (resource_sum / 8) * 0.90

-- After: 8 pheromone types (same loop, 3 more variables)
chunk[TERRITORY_PHEROMONE] = (territory_sum / 8) * 0.95
chunk[TERRITORY_FACTION] = (faction_sum / 8) * 0.90
chunk[TERRITORY_SUPPLY] = (supply_sum / 8) * 0.95
```

**Performance Impact**: Negligible (same number of operations, just more variables)

---

### 3.2 Temperament + Triggers Integration

**Pattern**: *Triggers modulate after event calculation*

```lua
-- Existing: Event-based temperament
local delta = calculateEventDelta(base)
base.temperamentScore = base.temperamentScore + delta
base.temperament = normalize(base.temperamentScore)

-- Enhanced: Triggers modulate
base.supplyLevel = calculateSupplyLevel(playerActivity.buildingCount)
base.healthMultiplier = calculateHealthMultiplier(playerActivity.techTier)

-- NEW: Supply affects temperament
if base.supplyLevel < 0.3 then
    base.temperamentScore = base.temperamentScore - 1000
end

-- NEW: Apply caps
base.healthMultiplier = clamp(1.0, 2.0, base.healthMultiplier)
```

**Performance Impact**: Minimal (simple calculations every 20 seconds per base)

---

### 3.3 Physical Manifestation Integration

**Pattern**: *Event-driven placement, pheromone-guided lifecycle*

```lua
-- Command Center placement (autoplace integration)
function onChunkGenerated(event)
    if distance > 500 and shouldPlaceCommandCenter(event.chunk) then
        local cc = surface.create_entity{...}
        local chunk = getChunkByPosition(cc.position)
        chunk[TERRITORY_PHEROMONE] = 100  -- Emit pheromone
    end
end

-- Connector lifecycle (implicit during diffusion)
function processPheromone(chunk, tick)
    if chunk[TERRITORY_PHEROMONE] > 40 and not hasConnectorAt(chunk) then
        scheduleConnectorCreation(chunk)  -- Event-driven, not per-tick
    end
end

-- Connector destruction (event-driven)
script.on_event(defines.events.on_entity_died, function(event)
    if event.entity.name:match("rampant-connector") then
        -- Cut supply, inject disruption, mark broken
    end
end)
```

**Performance Impact**: Low (event-driven, no per-tick polling)

---

### 3.4 Squad Movement Integration

**Pattern**: *New scoring functions plug into existing algorithm*

```lua
-- Existing: 3-step lookahead
function scoreNeighbors(map, chunk, neighborChunks, scoreFunction)
    -- Score up to 4 chunks ahead using provided function
    -- Returns best path
end

-- NEW: Add scoring functions
function scoreSupplyLocation(neighborChunk)
    return neighborChunk[TERRITORY_SUPPLY] * 2.0, false
end

function scoreResourceLocation(neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE] + 
           neighborChunk[TERRITORY_PHEROMONE] * 0.5, false
end

-- Existing squadDispatch() uses these
function Squad.squadDispatch(squad, tick)
    if squad.status == SQUAD_SUPPLY_CARRIER then
        supplyMove(squad, tick)  -- Calls scoreNeighbors with scoreSupplyLocation
    end
end
```

**Performance Impact**: None (same algorithm, just different scoring function)

---

## 4. Gameplay Loop: How It All Works

### 4.1 Early Game (Hours 0-15): Discovery

**Player Actions**:
- Build factory, create pollution
- Discover Command Centers beyond 500 tiles
- See physical connectors between CCs
- Destroy connector → downstream CC weakens

**System Response**:
- **Pheromone**: TERRITORY_PHEROMONE diffuses, connectors spawn where > 40
- **Temperament**: Pollution ↑ → temperament ↑
- **Triggers**: Building count ↑ → network growth rate ↑ (linear)
- **Squad**: Settlers follow RESOURCE_PHEROMONE + TERRITORY_PHEROMONE
- **Supply**: Connectors at 0% → gradual buildup → supply flows at 100%

**Learning**: "Cutting supply lines works!"

---

### 4.2 Mid Game (Hours 15-30): Understanding

**Player Actions**:
- Research technology (tier 2-3)
- Map network, identify Resource CCs
- Attack Resource Nodes, get modules
- Cut supply lines, fragment network

**System Response**:
- **Pheromone**: TERRITORY_FACTION spreads, TERRITORY_SUPPLY drops
- **Temperament**: Combat events ↑ → temperament volatility ↑
- **Triggers**: Tech tier ↑ → CC health ↑ (150-200%), Disruption ↑ → network depth ↓
- **Squad**: Supply carriers follow TERRITORY_SUPPLY gradient
- **Adaptation**: Defense consistency ↑ → faction shift speed ↑

**Learning**: "Network has hierarchy, Resource CCs are critical!"

---

### 4.3 Late Game (Hours 30+): Mastery

**Player Actions**:
- Destroy Resource CCs systematically
- Reclaim territory, build forward bases
- Maintain offensive pressure
- Use advanced technology

**System Response**:
- **Pheromone**: TERRITORY_PHEROMONE decays, PLAYER_PHEROMONE dominates
- **Temperament**: Offense ratio high → escalation timer long (no superstructures)
- **Triggers**: Network starved → supply low → units regress to Tier 1-2
- **Squad**: Retreating squads follow KAMIKAZE_PHEROMONE
- **Health**: Tech tier high → CC health at 200%, but network starved anyway

**Learning**: "Persistence and strategy win, not just factory size!"

---

## 5. Critical Assumptions

### 5.1 Performance Assumptions

- **40 chunks/tick** processing can handle 8 pheromone types (not 5)
- **Event-driven** connector lifecycle adds negligible overhead
- **3-step squad lookahead** remains efficient with new scoring functions
- **Independent triggers** calculated every 600 ticks (10 seconds) is sufficient

### 5.2 Gameplay Assumptions

- **Players will discover** supply lines through exploration (not tutorials)
- **Connector destruction** provides immediate, visible feedback
- **Network fragmentation** creates meaningful strategic choices
- **Diminishing returns** on health scaling feels fair, not punishing

### 5.3 Integration Assumptions

- **Existing temperament system** can accommodate supply/territory modifiers
- **Existing squad movement** works with new pheromone types
- **Existing evolution scaling** can be modulated by triggers
- **Existing autoplace** can place Command Centers beyond 500 tiles

---

## 6. Open Questions (For Implementation Phase)

1. **Connector buildup rate**: 5 minutes (300 ticks) vs. 10 minutes (600 ticks)?
2. **Territory decay rate**: 5%/tick vs. 3%/tick for slower collapse?
3. **Supply flow rate**: Instant when connector complete vs. gradual ramp-up?
4. **Resource Node health**: Same as CC vs. lower (easier to destroy)?
5. **Module drop rates**: 100% on CC destruction vs. chance-based?

---

## 7. Implementation Priority

### Phase 1: Core Systems (Weeks 1-2)
- [ ] Add 3 new pheromone types to Constants.lua
- [ ] Extend processPheromone() to diffuse new types
- [ ] Create Command Center prototype (1 type, scale via health)
- [ ] Implement territory emission from CCs

### Phase 2: Physical Manifestation (Weeks 3-4)
- [ ] Create Connector prototype with gradual health buildup
- [ ] Implement connector lifecycle (creation, destruction, auto-remove)
- [ ] Create Resource Node prototype
- [ ] Implement ground markings (decoratives)

### Phase 3: Independent Triggers (Weeks 5-6)
- [ ] Implement building count trigger
- [ ] Implement technology tier trigger
- [ ] Implement pollution trigger
- [ ] Implement combat events trigger

### Phase 4: Squad Enhancement (Weeks 7-8)
- [ ] Add scoreSupplyLocation() function
- [ ] Add scoreResourceLocation() enhancement
- [ ] Create SUPPLY_CARRIER squad type
- [ ] Test squad movement with new pheromones

### Phase 5: Integration & Polish (Weeks 9-10)
- [ ] Integrate triggers with temperament system
- [ ] Tune scaling factors and caps
- [ ] Add visual/audio feedback
- [ ] Balance testing and iteration

---

## 8. Related Documents

- **01-Constraints-and-Assumptions.md**: Technical constraints, prototype budget
- **02-Theory-and-Vision.md**: Core vision, design philosophy, player experience
- **03-Current-Pillars.md**: 8 core pillars with detailed mechanics
- **04-Factorio-API-Reference.md**: Factorio codebase paths and API reference

---

**Document Version**: 1.0  
**Last Updated**: 2026-03-01  
**Status**: Ready for Implementation Planning