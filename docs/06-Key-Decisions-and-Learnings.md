# Enemy Logistics System: Key Decisions & Learnings

## Document Purpose

This document captures all critical design decisions, assumptions, and learnings from our ideation phase. It serves as the **single source of truth** for implementation, preventing repeated discussions and ensuring consistency.

---

## 1. Core Design Decisions (Finalized)

### 1.1 Architecture: Three-Layer Integration

**Decision**: **Enhance existing systems, don't replace**

- **Pheromone Cloud**: Add 3 new types (TERRITORY, TERRITORY_FACTION, TERRITORY_SUPPLY) to existing 40-chunks/tick diffusion
- **Temperament System**: Keep existing event-driven state machine, add independent triggers as modifiers
- **Squad Movement**: Keep existing 3-step lookahead, add new scoring functions for new squad types
- **Physical Manifestation**: Event-driven placement, pheromone-guided lifecycle

**Rationale**: Leverages existing infrastructure, minimal performance impact, easier to debug

---

### 1.2 Evolution vs. Independent Triggers

**Decision**: **Modulate, don't replace**

- Evolution remains master scaling factor (time-based progression)
- Independent triggers fine-tune based on player activity
- **Formula**: `final_scaling = evolution × trigger_modifier`
- **Caps**: Health 1.0x-2.0x, spawn rates 0.5x-3.0x, network growth 0.5x-2.0x

**Rationale**: Preserves existing balance, gradual transition, player strategy shapes HOW enemy scales

---

### 1.3 Pheromone Processing

**Decision**: **Integrated into existing diffusion loop**

- Add 3 new pheromone types to existing `processPheromone()` function
- Same decay pattern: TERRITORY (0.95), TERRITORY_FACTION (0.90), TERRITORY_SUPPLY (0.95)
- No separate loops, no performance impact
- Connector creation scheduled during diffusion (event-driven, not per-tick)

**Rationale**: 40 chunks/tick can handle 8 types (not just 5), leverages existing infrastructure

---

### 1.4 Connector Lifecycle

**Decision**: **Gradual health buildup (0% → 100% over 5 minutes)**

- Connector spawns at 0% health
- Builds +10% every 300 ticks (5 minutes to 100%)
- No supply flow until 100% complete
- Player can interrupt during buildup (easy target)
- Auto-remove when territory strength < 20

**Rationale**: Tactical depth, visible progression, satisfying gameplay loop

---

### 1.5 Health Scaling Curve

**Decision**: **Diminishing returns with 2.0x cap**

- Player Tech Tier 0 → Enemy health 100%
- Player Tech Tier 1 → Enemy health 140%
- Player Tech Tier 2 → Enemy health 170%
- Player Tech Tier 3 → Enemy health 190%
- Player Tech Tier 4 → Enemy health 200% (cap)

**Rationale**: Prevents late-game health sponges, balanced challenge, fair progression

---

### 1.6 Squad Movement Enhancement

**Decision**: **New scoring functions, keep 3-step lookahead**

- Add `scoreSupplyLocation()` for SUPPLY_CARRIER squads
- Add `scoreResourceLocation()` enhancement for SETTLING squads
- Existing attack/retreat squads unchanged
- Same 3-step algorithm, new pheromone types in scores

**Rationale**: Minimal changes, leverages existing infrastructure, cost-effective

---

### 1.7 Temperament Integration

**Decision**: **Direct modifier from supply/territory**

- Low supply (< 0.3) → temperament -1000 (desperate behavior)
- Network fragmented → temperament -500 (vulnerable behavior)
- Temperament remains event-driven, triggers add context
- No separate parallel system

**Rationale**: Simple integration with existing state machine, creates meaningful feedback loops

---

### 1.8 Resource Node Placement

**Decision**: **Separate building ON ore patches**

- Resource Node is physical entity (destroyable, not just marker)
- Spawns on oil/iron/coal/uranium patches
- Command Center territory "claims" node via overlap
- Destroying node drops Technology Module + leaves enhanced patch (2x resources)

**Rationale**: Physical visibility, meaningful targets, rewards player for disruption

---

## 2. Critical Assumptions

### 2.1 Performance Assumptions

- ✅ **40 chunks/tick** processing can handle 8 pheromone types (not 5)
- ✅ **Event-driven** connector lifecycle adds negligible overhead
- ✅ **3-step squad lookahead** remains efficient with new scoring functions
- ✅ **Independent triggers** calculated every 600 ticks (10 seconds) is sufficient
- ✅ **Connector health buildup** tracked in sparse queue (not per-tick)

### 2.2 Gameplay Assumptions

- ✅ **Players will discover** supply lines through exploration (not tutorials)
- ✅ **Connector destruction** provides immediate, visible feedback (supply cuts instantly)
- ✅ **Network fragmentation** creates meaningful strategic choices (which CCs to target?)
- ✅ **Diminishing returns** on health scaling feels fair, not punishing
- ✅ **Gradual connector buildup** creates tactical window (interrupt during construction)

### 2.3 Integration Assumptions

- ✅ **Existing temperament system** can accommodate supply/territory modifiers
- ✅ **Existing squad movement** works with new pheromone types
- ✅ **Existing evolution scaling** can be modulated by triggers
- ✅ **Existing autoplace** can place Command Centers beyond 500 tiles
- ✅ **Native spawning** can be intercepted for supply-gated tier access

---

## 3. System Essence (Core Patterns)

### 3.1 Pheromone System: Environment's Memory

**Pattern**: Eventually consistent, distributed state encoding "where things are important"
- **40 chunks/tick** processing (distributed cost)
- **Decay factors** create natural half-lives (0.92, 0.95, 0.98 retention)
- **Neighbor averaging** creates smooth gradients (no hard boundaries)
- **Multiple types** encode different information layers

**Design Essence**: *The world remembers where players, enemies, and resources have been, and this memory guides future behavior.*

---

### 3.2 Squad Movement: Gradient Following

**Pattern**: 3-step lookahead pheromone scoring, not full pathfinding
- **Score neighbors** using context-dependent functions (attack vs. settle vs. retreat)
- **3-step lookahead** balances planning vs. cost (not infinite, not 1-step)
- **Status-dependent** scoring (same algorithm, different objectives)
- **Movement penalty tracking** creates emergent kamikaze behavior

**Design Essence**: *Units follow the steepest gradient toward their goal, with limited foresight and local decision-making.*

---

### 3.3 Temperament System: Base-Level Mood

**Pattern**: Event-driven score accumulation normalized to behavioral state
- **Per-base tracking** (not global - each base has personality)
- **Event accumulation** (destroyed buildings, lost units, rocket launches)
- **Normalized to 0-1** for state machine thresholds
- **State machine** maps temperament to AI behavior (peaceful → aggressive → onslaught)

**Design Essence**: *Each base has a mood that evolves based on its success/failure, driving its aggression level.*

---

### 3.4 Evolution Scaling: Power Laws

**Pattern**: Single evolution factor drives multiple scaling dimensions with power laws
- **Evolution^1.4** for attack wave sizes (super-linear)
- **Evolution^1.66667** for settler waves (more aggressive)
- **Evolution^2.5** for point generation (very aggressive)
- **Threshold gating** for unit tiers (0.3, 0.5, 0.7, 0.9)

**Design Essence**: *Game progression scales exponentially, not linearly - late game is dramatically harder.*

---

## 4. Integration Patterns

### 4.1 Pheromone Integration

```lua
-- Existing: 5 pheromone types in diffusion loop
chunk[PLAYER_PHEROMONE] = (player_sum / 8) * 0.98
chunk[BASE_PHEROMONE] = (base_sum / 8) * 0.90
chunk[ENEMY_PHEROMONE] = (enemy_sum / 8) * 0.92
chunk[KAMIKAZE_PHEROMONE] = (kamikaze_sum / 8) * 0.95
chunk[RESOURCE_PHEROMONE] = (resource_sum / 8) * 0.90

-- Enhanced: 8 pheromone types (same loop, 3 more variables)
chunk[TERRITORY_PHEROMONE] = (territory_sum / 8) * 0.95
chunk[TERRITORY_FACTION] = (faction_sum / 8) * 0.90
chunk[TERRITORY_SUPPLY] = (supply_sum / 8) * 0.95
```

**Performance**: Negligible (same number of operations, just more variables)

---

### 4.2 Temperament + Triggers Integration

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

**Performance**: Minimal (simple calculations every 20 seconds per base)

---

### 4.3 Squad Movement Integration

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

**Performance**: None (same algorithm, just different scoring function)

---

## 5. Gameplay Loop Summary

### Early Game (Hours 0-15): Discovery
- **Player**: Builds factory, discovers Command Centers, experiments with connector destruction
- **Enemy**: Network expands, supply flows, units at base strength
- **Learning**: "Cutting supply lines works!"

### Mid Game (Hours 15-30): Understanding
- **Player**: Researches tech, maps network, attacks Resource Nodes, fragments network
- **Enemy**: Adapts faction, health scales with player tech, network depth decreases
- **Learning**: "Network has hierarchy, Resource CCs are critical!"

### Late Game (Hours 30+): Mastery
- **Player**: Destroys Resource CCs systematically, reclaims territory, maintains offense
- **Enemy**: Network starved, units regress despite high evolution, no superstructures
- **Learning**: "Persistence and strategy win, not just factory size!"

---

## 6. Open Questions (For Implementation Phase)

1. **Connector buildup rate**: 5 minutes (300 ticks) vs. 10 minutes (600 ticks)?
   - **Current decision**: 5 minutes (tactical window, not too long)

2. **Territory decay rate**: 5%/tick vs. 3%/tick for slower collapse?
   - **Current decision**: 5%/tick (matches existing pheromone patterns)

3. **Supply flow rate**: Instant when connector complete vs. gradual ramp-up?
   - **Current decision**: Instant (clean feedback, "cut = starve immediately")

4. **Resource Node health**: Same as CC vs. lower (easier to destroy)?
   - **Current decision**: Lower (70% of CC health, accessible target)

5. **Module drop rates**: 100% on CC destruction vs. chance-based?
   - **Current decision**: 100% (consistent reward, no RNG frustration)

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

- **01-Constraints-and-Assumptions.md**: Technical constraints, prototype budget, finalized decisions
- **02-Theory-and-Vision.md**: Core vision, design philosophy, player experience
- **03-Current-Pillars.md**: 8 core pillars with detailed mechanics
- **04-Factorio-API-Reference.md**: Factorio codebase paths and API reference
- **05-System-Architecture-And-Integration.md**: Technical blueprint, integration patterns

---

**Document Version**: 1.0  
**Last Updated**: 2026-03-01  
**Status**: Ready for Implementation  
**Next Step**: Begin Phase 1 implementation (Core Systems)