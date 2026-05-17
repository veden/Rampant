# Event-Driven Multi-Layer Pheromone System with ACO

## Executive Summary

**Goal**: Replace wave-based pheromone processing with event-driven system while adding scout trails and fallback heuristics for emergent gameplay.

**Key Benefits**:
- **487× faster propagation**: Real-time vs 8-second lag
- **Accurate decay**: Decay-on-read ensures values reflect actual time elapsed
- **Emergent behavior**: Scout trails create organic discovery patterns
- **Smooth CPU usage**: Round-robin scheduling eliminates spikes
- **Graceful degradation**: Fallback heuristics handle pheromone-free zones

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│ Layer 1: Event-Driven Pheromones (Known Areas)                  │
│ - Player, Base, Enemy, Death, Resource pheromones               │
│ - Decay-on-read with 4 pre-computed tables (0.90, 0.92, 0.95, 0.98) │
│ - Priority queue processing (40 chunks/tick)                    │
│ - Ranges: 30-150 chunks based on decay rate                     │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ Layer 2: Scout Trails (Exploration)                             │
│ - Occasional scouts + all unit groups lay trails                │
│ - Minimal spreading (0.95 factor) + decay (0.97/tick)           │
│ - Handles diagonal movement, smooths chunk-corner roughness     │
│ - Self-organizing toward interesting locations                  │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ Layer 3: Fallback Heuristics (No Signals)                       │
│ - Move toward nearest base                                      │
│ - Seek unexplored territory                                     │
│ - Random walk with memory                                       │
└─────────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Core Event-Driven Infrastructure (Week 1-2)

### Goal
Replace wave-based processing with dirty chunk tracking and decay-on-read.

### Tasks

#### 1.1 Add Decay Tables to Constants.lua
**File**: `libs/Constants.lua`

**Add**:
```lua
-- Decay table depth (10,000 ticks = ~2.8 hours)
constants.DECAY_TABLE_DEPTH = 10000

-- Pre-computed decay tables
local function createDecayTable(rate)
    local table = {}
    for i = 0, constants.DECAY_TABLE_DEPTH do
        table[i] = rate ^ i
    end
    return table
end

constants.DECAY_090 = createDecayTable(0.90)  -- Base, Resource, Enemy
constants.DECAY_092 = createDecayTable(0.92)  -- Generators
constants.DECAY_095 = createDecayTable(0.95)  -- Kamikaze
constants.DECAY_098 = createDecayTable(0.98)  -- Player
constants.DECAY_TRAIL = createDecayTable(0.97) -- Scout trails
```

**Memory**: 5 tables × 10,000 × 8 bytes = 400KB

#### 1.2 Add Dirty Chunk Tracking to Universe
**File**: `control.lua` (in `onInit()` and `onLoad()`)

**Add**:
```lua
Universe.dirtyChunks = {
    priority1 = {},  -- Player-adjacent (within 5 chunks)
    priority2 = {},  -- Squad-active chunks
    priority3 = {},  -- Recent events (last 10 ticks)
    priority4 = {},  -- Background propagation
    priority5 = {}   -- Stale chunks
}

Universe.dirtyChunkSet = {}  -- Quick lookup to prevent duplicates
```

#### 1.3 Add Timestamp Fields to Chunks
**File**: `libs/ChunkUtils.lua` (in `createChunk()` or `initialScan()`)

**Add**:
```lua
chunk.playerLastRefresh = 0
chunk.playerBaseLastRefresh = 0
chunk.deathLastRefresh = 0
chunk.resourceLastRefresh = 0
chunk.trailLastRefresh = 0
```

#### 1.4 Implement markChunkDirty()
**File**: `libs/MapUtils.lua`

**Add**:
```lua
function MapUtils.markChunkDirty(chunkId, priority, tick)
    if not chunkId or chunkId == -1 then return end
    
    -- Prevent duplicates
    if Universe.dirtyChunkSet[chunkId] then
        -- Update priority if higher
        local existingPriority = Universe.dirtyChunkSet[chunkId]
        if priority < existingPriority then
            -- Remove from old priority list
            local oldList = Universe.dirtyChunks[existingPriority]
            for i, id in ipairs(oldList) do
                if id == chunkId then
                    table.remove(oldList, i)
                    break
                end
            end
            -- Add to new priority list
            table.insert(Universe.dirtyChunks[priority], chunkId)
            Universe.dirtyChunkSet[chunkId] = priority
        end
        return
    end
    
    -- Add to priority list
    table.insert(Universe.dirtyChunks[priority], chunkId)
    Universe.dirtyChunkSet[chunkId] = priority
end
```

#### 1.5 Implement getEffectivePheromone()
**File**: `libs/ChunkPropertyUtils.lua`

**Add**:
```lua
function ChunkPropertyUtils.getEffectivePheromone(chunk, type, tick)
    local value = chunk[type] or 0
    if value == 0 then return 0 end
    
    local timestampField = type .. "LastRefresh"
    local lastRefresh = chunk[timestampField] or 0
    local ticksElapsed = tick - lastRefresh
    
    -- Select appropriate decay table
    local decayTable
    if type == "PLAYER_PHEROMONE" then
        decayTable = Constants.DECAY_098
    elseif type == "BASE_PHEROMONE" or type == "RESOURCE_PHEROMONE" or type == "ENEMY_PHEROMONE" then
        decayTable = Constants.DECAY_090
    elseif type == "KAMIKAZE_PHEROMONE" then
        decayTable = Constants.DECAY_095
    else
        return value  -- No decay
    end
    
    -- Apply decay
    local decayFactor = decayTable[ticksElapsed] or decayTable[Constants.DECAY_TABLE_DEPTH]
    return value * decayFactor
end
```

#### 1.6 Implement processDirtyChunks()
**File**: `libs/Processor.lua`

**Add**:
```lua
function Processor.processDirtyChunks(tick)
    local processed = 0
    local maxPerTick = 40  -- Fixed budget
    
    for priority = 1, 5 do
        local chunkList = Universe.dirtyChunks[priority]
        while processed < maxPerTick and #chunkList > 0 do
            local chunkId = table.remove(chunkList, 1)
            Universe.dirtyChunkSet[chunkId] = nil
            
            local chunk = MapUtils.getChunkById(chunkId)
            if chunk and chunk.valid then
                MapUtils.processPheromone(chunk, tick)
                processed = processed + 1
                
                -- Mark neighbors dirty for next tick
                local neighbors = MapUtils.getNeighborChunks(chunk.map, chunk.x, chunk.y)
                for i = 1, 8 do
                    if neighbors[i] ~= -1 then
                        MapUtils.markChunkDirty(neighbors[i].id, priority + 1, tick)
                    end
                end
            end
        end
        
        if processed >= maxPerTick then break end
    end
end
```

#### 1.7 Modify Event Handlers
**File**: `control.lua`

**Modify**:
```lua
-- onBuild event
local function onBuild(event)
    local entity = event.entity
    local chunk = MapUtils.getChunkByPosition(Universe.maps[entity.surface.index], entity.position)
    if chunk and chunk ~= -1 then
        -- Add generator based on entity type
        -- ... existing logic ...
        
        -- Mark chunk dirty
        MapUtils.markChunkDirty(chunk.id, 3, game.tick)
    end
end

-- onDeath event
local function onDeath(event)
    local entity = event.entity
    local chunk = MapUtils.getChunkByPosition(Universe.maps[entity.surface.index], entity.position)
    if chunk and chunk ~= -1 then
        -- Add death generator
        ChunkPropertyUtils.addDeathGenerator(chunk, -Constants.FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
        chunk.deathLastRefresh = game.tick
        
        -- Mark chunk dirty
        MapUtils.markChunkDirty(chunk.id, 3, game.tick)
    end
end

-- onChunkGenerated event
local function onChunkGenerated(event)
    local surface = event.surface
    local chunkPosition = event.position
    
    -- Mark chunk as dirty for initial processing
    -- ... existing logic ...
end
```

---

## Phase 2: Source Refresh System (Week 2-3)

### Goal
Smooth, staggered refresh of constant sources without CPU spikes.

### Tasks

#### 2.1 Implement Round-Robin Scheduler
**File**: `libs/Processor.lua`

**Add**:
```lua
function Processor.refreshSources(tick)
    -- Player structures: 5,000 sources, refresh every 30 ticks
    -- Batch size: 5000 / 30 = ~167 per tick
    local structureSources = Universe.structureSources
    local batchSize = math.ceil(#structureSources / 30)
    local startIndex = Universe.sourceRefreshIndex.structureSources or 0
    local endIndex = (startIndex + batchSize) % #structureSources
    
    for i = startIndex, endIndex do
        local chunk = structureSources[i]
        if chunk and chunk.valid then
            chunk.playerBaseGenerator = chunk.playerBaseGenerator or 0
            chunk.playerBaseLastRefresh = tick
            MapUtils.markChunkDirty(chunk.id, 2, tick)
        end
    end
    
    Universe.sourceRefreshIndex.structureSources = endIndex
    
    -- Repeat for other source types with different intervals
    -- Enemy spawners: every 60 ticks (~83/tick)
    -- Hives: every 120 ticks (~42/tick)
end
```

#### 2.2 Modify processPlayers()
**File**: `libs/Processor.lua`

**Change from**:
```lua
-- Current: Direct pheromone setting
addPlayerGenerator(playerChunk, playerMaxGenerator)
```

**Change to**:
```lua
-- New: Dirty marking
playerChunk.playerGenerator = (playerChunk.playerGenerator or 0) + playerMaxGenerator
playerChunk.playerLastRefresh = tick
MapUtils.markChunkDirty(playerChunk.id, 1, tick)  -- Highest priority
```

---

## Phase 3: Scout Trail System (Week 3-4)

### Goal
Add ant colony optimization for exploration in empty zones.

### Tasks

#### 3.1 Add Trail Fields to Chunks
**File**: `libs/ChunkUtils.lua`

**Add**:
```lua
chunk.positiveTrail = 0
chunk.negativeTrail = 0
chunk.trailLastRefresh = 0
```

#### 3.2 Implement Trail Spreading
**File**: `libs/MapUtils.lua` (in `processPheromone()`)

**Add after line 615**:
```lua
-- Trail spreading (minimal diffusion, 0.95 factor)
local chunkPosTrail = chunk.positiveTrail or 0
local chunkNegTrail = chunk.negativeTrail or 0
local neighborPosTrailSum = 0
local neighborNegTrailSum = 0
local neighborCount = 1

for i = 1, 8 do
    local neighbor = tempNeighbors[i]
    if neighbor ~= -1 then
        neighborPosTrailSum = neighborPosTrailSum + (neighbor.positiveTrail or 0)
        neighborNegTrailSum = neighborNegTrailSum + (neighbor.negativeTrail or 0)
        neighborCount = neighborCount + 1
    end
end

-- Apply minimal spreading
chunk.positiveTrail = (neighborPosTrailSum / neighborCount) * 0.95
chunk.negativeTrail = (neighborNegTrailSum / neighborCount) * 0.95

-- Decay trails
local ticksElapsed = tick - (chunk.trailLastRefresh or tick)
chunk.positiveTrail = chunk.positiveTrail * (Constants.DECAY_TRAIL[ticksElapsed] or 0)
chunk.negativeTrail = chunk.negativeTrail * (Constants.DECAY_TRAIL[ticksElapsed] or 0)

-- Clean up near-zero trails
if chunk.positiveTrail < 0.001 then chunk.positiveTrail = 0 end
if chunk.negativeTrail < 0.001 then chunk.negativeTrail = 0 end
```

#### 3.3 Implement Scout Selection
**File**: `libs/Squad.lua`

**Add**:
```lua
function Squad.refreshScouts(tick)
    if tick % 600 ~= 0 then return end  -- Every 10 seconds
    
    for _, squad in pairs(Universe.squads) do
        squad.isScout = (Universe.random() < 0.07)  -- 7% chance
        squad.lastScoutSelection = tick
    end
end
```

#### 3.4 Add Trail-Laying Mechanism
**File**: `libs/Squad.lua` (in movement functions)

**Add to `settleMove()` (line 456)**:
```lua
-- Before moving to new chunk
if chunk and chunk ~= -1 and chunk.id ~= squad.lastTrailChunk then
    local trailAmount = Constants.TRAIL_BASE_AMOUNT  -- 10
    if squad.isScout then
        trailAmount = trailAmount * Constants.TRAIL_SCOUT_MULTIPLIER  -- 25
    end
    
    -- Lay positive trail
    chunk.positiveTrail = (chunk.positiveTrail or 0) + trailAmount
    chunk.trailLastRefresh = tick
    MapUtils.markChunkDirty(chunk.id, 4, tick)
    
    squad.lastTrailChunk = chunk.id
end
```

**Add to retreat behavior**:
```lua
-- When retreating, lay negative trail
chunk.negativeTrail = (chunk.negativeTrail or 0) + trailAmount
```

#### 3.5 Modify Scoring Functions
**File**: `libs/Squad.lua`

**Example: `scoreAttackLocation()` (line 186-201)**

**Change from**:
```lua
local damage = neighborChunk[BASE_PHEROMONE] +
    (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)
```

**Change to**:
```lua
local pheromoneScore = neighborChunk[BASE_PHEROMONE] +
    (neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER)

local trailScore = (neighborChunk.positiveTrail or 0) * Constants.TRAIL_WEIGHT  -- 0.3

local damage = pheromoneScore * Constants.PHEROMONE_WEIGHT + trailScore  -- 0.7 + 0.3
```

**Example: `scoreRetreatLocation()` (line 129-132)**

**Change from**:
```lua
return (-neighborChunk[KAMIKAZE_PHEROMONE] +
        -((neighborChunk.playerBaseGenerator or 0) * 1000))
```

**Change to**:
```lua
local pheromoneScore = -neighborChunk[KAMIKAZE_PHEROMONE] +
    -((neighborChunk.playerBaseGenerator or 0) * 1000)

-- Negative trails are repulsive for retreating units
local trailScore = -(neighborChunk.negativeTrail or 0) * Constants.TRAIL_WEIGHT

return pheromoneScore * Constants.PHEROMONE_WEIGHT + trailScore
```

---

## Phase 4: Fallback Heuristics (Week 4)

### Goal
Graceful degradation when no signals exist.

### Tasks

#### 4.1 Add Chunk Exploration State
**File**: `libs/ChunkUtils.lua`

**Add**:
```lua
chunk.explorationState = "unexplored"  -- "unexplored", "partially-explored", "explored"
```

#### 4.2 Implement Fallback Behavior
**File**: `libs/Squad.lua`

**Add to scoring functions**:
```lua
-- Check if any signals exist
local hasPheromones = (neighborChunk[BASE_PHEROMONE] > 0) or
                      (neighborChunk[PLAYER_PHEROMONE] > 0) or
                      (neighborChunk[ENEMY_PHEROMONE] > 0)
local hasTrails = (neighborChunk.positiveTrail or 0) > 0

if not hasPheromones and not hasTrails then
    -- Apply fallback heuristic
    if squad.type == SQUAD_SETTLING then
        -- Move toward nearest known base
        local nearestBase = BaseUtils.findNearestBase(neighborChunk)
        if nearestBase then
            score = score + 50  -- Small bonus for direction toward base
        end
    elseif squad.type == SQUAD_ATTACKING then
        -- Random walk with memory (avoid recently visited)
        if not neighborChunk.lastVisited or (tick - neighborChunk.lastVisited > 300) then
            score = score + 20
        end
    end
end
```

---

## Phase 5: Testing and Optimization (Week 5)

### Goal
Validate performance and tune parameters.

### Tasks

#### 5.1 Profile CPU Usage
- Measure time per tick for each phase
- Target: <5ms total
- Identify bottlenecks

#### 5.2 Test Propagation Speed
- Verify real-time pheromone spread
- Measure time for signal to travel 10, 30, 50 chunks
- Compare to current 8-second lag

#### 5.3 Test Scout Trail Discovery
- Empty zone exploration behavior
- Trail formation and evaporation
- Squad response to trails

#### 5.4 Tune Parameters
- Decay rates (0.90-0.98)
- Refresh intervals (5-120 ticks)
- Trail weights (0.3)
- Scout selection rate (7%)

---

## Configuration Constants

```lua
-- Decay rates
constants.DECAY_BASE = 0.90
constants.DECAY_GENERATOR = 0.92
constants.DECAY_KAMIKAZE = 0.95
constants.DECAY_PLAYER = 0.98
constants.DECAY_TRAIL = 0.97

-- Trail system
constants.TRAIL_BASE_AMOUNT = 10
constants.TRAIL_SCOUT_MULTIPLIER = 2.5
constants.TRAIL_WEIGHT = 0.3
constants.PHEROMONE_WEIGHT = 0.7
constants.TRAIL_SPREAD_FACTOR = 0.95

-- Scout selection
constants.SCOUT_SELECTION_RATE = 0.07
constants.SCOUT_REFRESH_TICKS = 600

-- Source refresh intervals
constants.REFRESH_PLAYER_TICKS = 5
constants.REFRESH_STRUCTURES_TICKS = 30
constants.REFRESH_SPAWNERS_TICKS = 60
constants.REFRESH_HIVES_TICKS = 120

-- Processing budget
constants.PROCESS_DIRTY_CHUNKS_PER_TICK = 40
```

---

## Memory Impact

| Component | Current | Event-Driven | Delta |
|-----------|---------|--------------|-------|
| Chunk fields | ~12 | ~18 | +6 fields |
| Decay tables | 0 | 400KB | +400KB |
| Dirty queue | 0 | ~50KB | +50KB |
| Source tracking | ~100KB | ~150KB | +50KB |
| Scout trails | 0 | ~100KB | +100KB |
| **Total** | ~2.5MB | ~3.2MB | **+700KB** |

**Assessment**: +28% memory increase, well within acceptable range.

---

## CPU Budget Analysis

| Operation | Current | Event-Driven | Notes |
|-----------|---------|--------------|-------|
| Propagation | 40 chunks/tick | 40 chunks/tick | Same budget |
| Source refresh | ~80 chunks/tick | ~80 chunks/tick | Staggered, smooth |
| Decay-on-read | N/A | +32 lookups/chunk | 8 neighbors × 4 pheromones |
| Trail spreading | N/A | +8 lookups/chunk | Trail diffusion |
| **Total** | ~120 ops/tick | ~160 ops/tick | +33% but more accurate |

**Target**: <5ms/tick total (current system ~3-4ms)

---

## Success Criteria

1. **Performance**: <5ms/tick CPU usage, no spikes
2. **Propagation**: Real-time pheromone spread (1 chunk/tick vs 487 ticks)
3. **Accuracy**: Decay reflects actual time elapsed
4. **Emergence**: Scout trails create organic discovery patterns
5. **Graceful degradation**: Units behave sensibly in pheromone-free zones
6. **Smoothness**: No CPU spikes, consistent frame times