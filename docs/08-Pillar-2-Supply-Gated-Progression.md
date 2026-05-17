# Pillar 2: Supply-Gated Progression

## Document Purpose

This document describes the complete design for Pillar 2: Supply-Gated Progression. It captures the vision, mechanics, constraints, and design rationale for the supply gating system that makes supply disruption visibly impactful and creates meaningful player choices.

---

## 1. Vision

Supply **retroactively constrains** spawner tiers at the chunk level, while **evolution gates the maximum tier** at the surface level. This creates a two-layer constraint system where players see immediate, visible feedback when supply lines are disrupted.

### Core Design Goal

Make supply disruption **visibly impactful** - players see enemy spawners downgrade when they cut supply lines, creating clear cause-and-effect feedback.

---

## 2. Design Philosophy

### 2.1 Two-Layer Constraint System

**Evolution (Surface-Level)**: Sets the **maximum possible tier** across the entire surface.

**Supply (Chunk-Level)**: Sets the **actual tier** at each specific location.

**Formula**: `actualTier = min(evolutionTier, supplyTier)`

**Rationale**: Evolution is a familiar metric players already understand. Supply adds local variation within that ceiling.

### 2.2 Retroactive Effect

**Supply changes immediately affect existing spawners** - not just new placements.

- **Downgrade**: High-tier spawner → Low-tier spawner (visible explosion)
- **Upgrade**: Low-tier spawner → High-tier spawner (visible flash)
- **Health preserved**: New spawner gets old spawner's health percentage

**Rationale**: Players see immediate cause-and-effect, not delayed feedback.

### 2.3 Integrated Processing

**No separate loops** - supply tier checking integrated into existing pheromone diffusion.

- **Single pass**: One loop handles pheromone diffusion AND supply tier checks
- **Eventually consistent**: Chunks checked at natural visit rate
- **Performance**: No additional iteration overhead, cache-friendly
- **Simplicity**: One code path, one place to maintain

**Rationale**: Efficient, simple, acceptable latency.

---

## 3. Core Mechanics

### 3.1 Two-Layer Constraint

| Layer | Scope | Effect | Player Understanding |
|-------|-------|--------|---------------------|
| **Evolution** | Surface-level | Maximum tier available on entire surface | "At evolution 0.5, max Tier 5 units" |
| **Supply** | Chunk-level | Actual tier constraint within surface | "This area is starved, only Tier 2 units" |

**Example Scenarios**:

| Scenario | Surface Evolution | Chunk Supply | Result |
|----------|------------------|--------------|--------|
| **High evolution, high supply** | 0.8 (Tier 8) | 0.9 (Tier 9) | Tier 8 spawner placed |
| **High evolution, low supply** | 0.8 (Tier 8) | 0.2 (Tier 2) | Tier 2 spawner placed |
| **Low evolution, high supply** | 0.3 (Tier 3) | 0.9 (Tier 9) | Tier 3 spawner placed |
| **Low evolution, low supply** | 0.3 (Tier 3) | 0.2 (Tier 2) | Tier 2 spawner placed |

### 3.2 Tier Calculation

```lua
-- Evolution sets max tier (surface-level)
local surfaceEvolution = game.forces.enemy.evolution_factor[surface.index]
local evolutionTier = evoToTier(surfaceEvolution, 5)  -- Existing Rampant function

-- Supply sets actual tier (chunk-level)
local supplyLevel = chunk[TERRITORY_SUPPLY] or 0
local supplyTier = math.floor(supplyLevel * 10) + 1

-- Final tier constrained by both
local actualTier = math.min(evolutionTier, supplyTier)
```

### 3.3 Spawner Tier Change Logic

**Integrated into pheromone processing** (no separate loop):

```lua
function processPheromone(chunk, tick)
    -- Existing pheromone diffusion...
    
    -- NEW: Supply tier check (integrated)
    local currentSupply = chunk[TERRITORY_SUPPLY] or 0
    local recordedSupply = chunk.recordedSupply or currentSupply
    local lastUpgradeTick = chunk.lastUpgradeTick or 0
    
    -- Hysteresis check (10% threshold)
    if math.abs(currentSupply - recordedSupply) > 0.10 then
        -- Cooldown check (300 ticks minimum)
        if tick - lastUpgradeTick > 300 then
            local currentTier = math.floor(recordedSupply * 10) + 1
            local newTier = math.floor(currentSupply * 10) + 1
            
            -- Evolution cap
            local surfaceEvolution = getSurfaceEvolution(chunk.surface)
            local maxTier = evoToTier(surfaceEvolution, 5)
            newTier = math.min(newTier, maxTier)
            
            if currentTier ~= newTier then
                upgradeOrDowngradeSpawners(chunk, currentTier, newTier)
                chunk.lastUpgradeTick = tick
            end
        end
    end
    
    chunk.recordedSupply = currentSupply
end
```

### 3.4 Upgrade/Downgrade Implementation

```lua
function upgradeOrDowngradeSpawners(chunk, oldTier, newTier)
    local spawners = findSpawnersInChunk(chunk)
    
    for _, spawner in ipairs(spawners) do
        local currentTier = spawner.prototype.tier
        
        -- Only act if spawner tier doesn't match supply tier
        if currentTier ~= newTier then
            local position = spawner.position
            local faction = getSpawnerFaction(spawner)
            local healthPercent = spawner.health / spawner.max_health
            
            -- Destroy old spawner (triggers explosion effect)
            spawner.destroy()
            
            -- Create new spawner at correct tier
            local newPrototype = getSpawnerPrototype(faction, newTier)
            local newSpawner = surface.create_entity{
                name = newPrototype,
                position = position
            }
            
            -- Preserve health percentage
            newSpawner.health = newSpawner.max_health * healthPercent
        end
    end
end
```

### 3.5 Visual Feedback

**Explosion + Spawn effect** (no floating text):
- **Downgrade**: Small explosion when old spawner destroyed
- **Upgrade**: Larger explosion/flash when new spawner created
- **No floating text**: Clean visual, no UI clutter

**Rationale**: Visible effect without text spam, players see the change clearly.

---

## 4. Design Parameters

| Parameter | Value | Rationale |
|-----------|-------|-----------|
| **Hysteresis threshold** | 0.10 (10% supply change) | Stable with irregular timing |
| **Cooldown between changes** | 300 ticks (5 seconds) | Prevents rapid up/down cycling |
| **Health preservation** | Yes (all tier changes) | Fair, maintains spawner state |
| **Visual feedback** | Explosion + Spawn effect | Maximum clarity, no UI clutter |
| **Processing location** | Integrated into pheromone loop | Single pass, efficient |
| **Visit rate handling** | Accept variability | Hot zones respond faster (desirable) |

---

## 5. Player Experience

### 5.1 Action → Effect Mapping

| Player Action | System Effect | Player Feeling |
|--------------|---------------|----------------|
| **Destroy connector** | Supply drops → spawners downgrade | "I cut their supply - their base got weaker!" |
| **Maintain disruption** | Spawners stay at low tiers | "They can't recover their strength" |
| **Restore supply** | Spawners upgrade back | "They're rebuilding their power" |
| **Ignore supply lines** | Spawners stay at high tiers | "They're getting stronger every day" |

### 5.2 Visibility & Clarity

| Aspect | Implementation | Player Understanding |
|--------|---------------|---------------------|
| **Evolution ceiling** | Surface evolution level | "At this evolution, max Tier X units" |
| **Supply constraint** | Spawner tier changes | "This area is starved, only Tier Y units" |
| **Tier changes** | Explosion + spawn effect | "Their spawner just downgraded!" |
| **Health state** | Preserved as percentage | "Fair - timing doesn't punish me" |

---

## 6. Technical Implementation

### 6.1 State Tracking

**Per-chunk state** (minimal overhead):

```lua
chunk.recordedSupply = 0.65       -- Last recorded supply level
chunk.lastUpgradeTick = 12500     -- Last tier change tick
```

**No global tracking required** - state lives in chunk properties.

### 6.2 Processing Integration

**Integrated into existing pheromone loop**:

```lua
-- Existing: 40 chunks/tick processed
-- Enhanced: 40 chunks/tick processed (same cost, more work per chunk)

function processPheromone(chunk, tick)
    -- Diffuse PLAYER, BASE, RESOURCE, ENEMY, KAMIKAZE pheromones
    
    -- NEW: Diffuse TERRITORY, TERRITORY_SUPPLY, TERRITORY_FACTION
    
    -- NEW: Supply tier check (integrated)
    if math.abs(chunk[TERRITORY_SUPPLY] - chunk.recordedSupply) > 0.10 then
        -- Check cooldown, upgrade/downgrade spawners
    end
end
```

**Performance Impact**: Negligible (same number of chunks processed, simple math per chunk).

### 6.3 Tier Lookup

**Reuse existing Rampant infrastructure**:

```lua
-- Existing function (from BaseUtils.lua)
local function evoToTier(evolutionFactor, maxSkips)
    local v
    local skipsRemaining = maxSkips
    for i=TIERS,1,-1 do
        if EVO_TO_TIER_MAPPING[i] <= evolutionFactor then
            v = i
            if (skipsRemaining == 0) or (Universe.random() <= 0.75) then
                break
            end
            skipsRemaining = skipsRemaining - 1
        end
    end
    return v
end
```

**No new tier calculation logic required** - reuse existing evolution-to-tier mapping.

---

## 7. Design Rationale

### 7.1 Why Two-Layer Constraint?

**Problem**: Supply alone would be unpredictable (no ceiling).

**Solution**: Evolution sets familiar ceiling, supply adds local variation.

**Benefits**:
- ✅ **Predictable** - Players understand evolution metric
- ✅ **Flexible** - Supply creates local variation
- ✅ **Meaningful** - Both mechanics matter

**Trade-offs**:
- ⚠️ Two systems to balance
- ✅ Clear separation of concerns

---

### 7.2 Why Retroactive Effect?

**Problem**: Supply only affecting new placements has delayed feedback.

**Solution**: Existing spawners upgrade/downgrade when supply changes.

**Benefits**:
- ✅ **Immediate feedback** - Players see cause-and-effect
- ✅ **Meaningful disruption** - Cutting supply has visible impact
- ✅ **Strategic depth** - Players can target supply for immediate effect

**Trade-offs**:
- ⚠️ Requires spawner tracking and recreation
- ✅ Acceptable performance (integrated into pheromone loop)

---

### 7.3 Why Integrated Processing?

**Problem**: Separate loops add overhead and complexity.

**Solution**: Supply tier checking integrated into pheromone diffusion.

**Benefits**:
- ✅ **Efficient** - Single pass, no additional iteration
- ✅ **Simple** - One code path, one place to maintain
- ✅ **Cache-friendly** - Same data accessed together

**Trade-offs**:
- ⚠️ Irregular check timing (chunks visited at different rates)
- ✅ Hysteresis + cooldown handle irregular timing

---

### 7.4 Why Explosion Feedback?

**Problem**: Floating text creates UI clutter, may be missed.

**Solution**: Explosion + spawn effect (visual, no text).

**Benefits**:
- ✅ **Visible** - Players see the change clearly
- ✅ **Clean** - No UI clutter or text spam
- ✅ **Consistent** - Matches Factorio's visual language

**Trade-offs**:
- ⚠️ May be missed in busy combat
- ✅ Still visible as distinct visual event

---

## 8. Design Constraints

### 8.1 Technical Constraints

| Constraint | Value | Rationale |
|------------|-------|-----------|
| **Processing integration** | Pheromone loop | No separate loops, efficient |
| **State tracking** | Per-chunk only | Minimal overhead, no global tracking |
| **Tier calculation** | Reuse existing evoToTier | Leverage existing infrastructure |
| **Spawner recreation** | Destroy + create | Simple, no prototype modification |

### 8.2 Gameplay Constraints

| Constraint | Value | Rationale |
|------------|-------|-----------|
| **Hysteresis threshold** | 0.10 | Stable with irregular timing |
| **Cooldown** | 300 ticks | Prevents rapid cycling |
| **Health preservation** | Yes (all changes) | Fair, maintains state |
| **Evolution cap** | Surface-level | Familiar metric, predictable ceiling |

### 8.3 Prototype Budget

| Entity Type | Count | Notes |
|-------------|-------|-------|
| **Spawner tiers** | 10 | 10 tiers × existing factions |
| **Visual effects** | 2 | Explosion (downgrade), flash (upgrade) |
| **Total** | 12 | Minimal overhead |

---

## 9. Success Criteria

Pillar 2 is successful when:
- ✅ **Evolution provides ceiling** - Players understand max threat level
- ✅ **Supply provides local constraint** - Players see immediate feedback
- ✅ **Tier changes visible** - Explosions communicate changes clearly
- ✅ **Health preserved** - Fair, no timing-based punishment
- ✅ **Efficient processing** - Integrated into pheromone loop
- ✅ **Stable behavior** - Hysteresis + cooldown prevent oscillation
- ✅ **Meaningful disruption** - Cutting supply = immediate enemy weakening

---

## 10. Open Questions (For Implementation)

1. **Explosion effect size**: Small (downgrade) vs. Large (upgrade)?
   - **Recommendation**: Small for downgrade, medium for upgrade

2. **Hysteresis threshold**: 0.10 or 0.15?
   - **Recommendation**: 0.10 (with 300-tick cooldown, oscillation prevented)

3. **Health preservation**: All changes or upgrades only?
   - **Recommendation**: All changes (fair, predictable)

4. **Visit rate variability**: Accept or track for fairness?
   - **Recommendation**: Accept (hot zones faster is desirable)

---

## 11. Related Documents

- **01-Constraints-and-Assumptions.md**: Technical constraints, prototype budget
- **02-Theory-and-Vision.md**: Core vision, design philosophy
- **03-Current-Pillars.md**: 8 core pillars overview
- **05-System-Architecture-And-Integration.md**: Technical blueprint
- **06-Key-Decisions-and-Learnings.md**: Finalized design decisions
- **07-Pillar-1-Territory-Supply-Lines.md**: Pillar 1 design

---

**Document Version**: 1.0  
**Last Updated**: 2026-03-01  
**Status**: Ready for Implementation  
**Next Step**: Pillar 3: Territory-Attached Resources