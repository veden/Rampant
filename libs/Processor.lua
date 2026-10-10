-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if ProcessorG then
    return ProcessorG
end
local Processor = {}

--

local TargetPosition
local Universe
local Queries

-- imports

local Utils = require("Utils")
local MapUtils = require("MapUtils")
local Squad = require("Squad")
local Constants = require("Constants")
local ChunkUtils = require("ChunkUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local BaseUtils = require("BaseUtils")
local MathUtils = require("MathUtils")

-- Constants

local CHUNK_SIZE = Constants.CHUNK_SIZE
local EIGHTH_CHUNK_SIZE = Constants.EIGHTH_CHUNK_SIZE

local PLAYER_PHEROMONE_GENERATOR_AMOUNT = Constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local CLEANUP_QUEUE_SIZE = Constants.CLEANUP_QUEUE_SIZE

local PROCESS_PLAYER_BOUND = Constants.PROCESS_PLAYER_BOUND

local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE

local PROPAGATION_RELATIVE_EPSILON = Constants.PROPAGATION_RELATIVE_EPSILON
local PROPAGATION_ABSOLUTE_EPSILON = Constants.PROPAGATION_ABSOLUTE_EPSILON

local AI_VENGENCE_SQUAD_COST = Constants.AI_VENGENCE_SQUAD_COST

local COOLDOWN_DRAIN = Constants.COOLDOWN_DRAIN
local COOLDOWN_RALLY = Constants.COOLDOWN_RALLY
local COOLDOWN_RETREAT = Constants.COOLDOWN_RETREAT

local BUILDING_HIVE_TYPE_LOOKUP = Constants.BUILDING_HIVE_TYPE_LOOKUP

local DEV_HIVE_TTL = Constants.DEV_HIVE_TTL
local MAX_HIVE_TTL = Constants.MAX_HIVE_TTL
local MIN_HIVE_TTL = Constants.MIN_HIVE_TTL

-- imported functions

local distortPositionConcentricCircles = MathUtils.distortPositionConcentricCircles
local linearInterpolation = MathUtils.linearInterpolation
local gaussianRandomRangeRG = MathUtils.gaussianRandomRangeRG

local createChunk = ChunkUtils.createChunk
local initialScan = ChunkUtils.initialScan

local removeChunkFromMap = MapUtils.removeChunkFromMap
local chunkPassScan = ChunkUtils.chunkPassScan

local unregisterEnemyBaseStructure = ChunkUtils.unregisterEnemyBaseStructure
local setPositionInQuery = Utils.setPositionInQuery

local addPlayerGenerator = ChunkPropertyUtils.addPlayerGenerator
local getPlayerGeneratorChunks = ChunkPropertyUtils.getPlayerGeneratorChunks
local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local removeChunkToNest = MapUtils.removeChunkToNest

local queueCreation = BaseUtils.queueCreation

local getCombinedDeathGeneratorRating = ChunkPropertyUtils.getCombinedDeathGeneratorRating
local processBaseMutation = BaseUtils.processBaseMutation

local processNestActiveness = ChunkPropertyUtils.processNestActiveness

local formSquads = Squad.formSquads
local formVengenceSquad = Squad.formVengenceSquad
local formVengenceSettler = Squad.formVengenceSettler
local formSettlers = Squad.formSettlers

local getChunkByPosition = MapUtils.getChunkByPosition
local getChunkByXY = MapUtils.getChunkByXY
local markChunkDirty = MapUtils.markChunkDirty

local validPlayer = Utils.validPlayer

local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount

local canAttack = BaseUtils.canAttack
local canMigrate = BaseUtils.canMigrate

local tableSize = table_size

local next = next

-- module code

--[[
    Event-driven pheromone processing replaces the old wave-based processMap:
    generators mark chunks dirty and processDirtyChunks propagates outward
    with a bounded per-tick budget.
--]]

--[[
    Localized player radius where processing takes place in realtime, doesn't store state
    between calls.
--]]
function Processor.processPlayers(players, tick)
    -- put down player pheromone for player hunters
    -- randomize player order to ensure a single player isn't singled out
    -- not looping everyone because the cost is high enough already in multiplayer
    local playerCount = #players
    local playerMaxGenerator = playerCount * PLAYER_PHEROMONE_GENERATOR_AMOUNT
    for i=1,playerCount do
        local player = players[i]
        if validPlayer(player) then
            local char = player.character
            local map = Universe.maps[char.surface.index]
            if map then
                local playerChunk = getChunkByPosition(map, char.position)

                if (playerChunk ~= -1) then
                    addPlayerGenerator(playerChunk, playerMaxGenerator)
                    markChunkDirty(playerChunk.id, 1, tick)  -- Priority 1: player-adjacent
                end
            end
        end
    end

    -- A player generator only decays while its chunk is processed, and walking
    -- away does not mark the chunk that was left behind. Re-mark every chunk
    -- still holding one, or its source never expires and the player field keeps
    -- pointing at where the player used to be. The set is one entry per
    -- player-occupied chunk, so this is a handful of marks.
    local playerGeneratorChunks = getPlayerGeneratorChunks()
    if playerGeneratorChunks then
        for id, chunk in pairs(playerGeneratorChunks) do
            if chunk.valid then
                markChunkDirty(id, 1, tick)
            else
                playerGeneratorChunks[id] = nil
            end
        end
    end

    if (#players == 0) then
        return
    end
    local player = players[Universe.random(#players)]
    if not validPlayer(player) then
        return
    end
    local char = player.character
    local map = Universe.maps[char.surface.index]
    if not map then
        return
    end
    local playerChunk = getChunkByPosition(map, char.position)

    if (playerChunk == -1) then
        return
    end
    local base = findNearbyBase(playerChunk)
    if not base then
        return
    end
    local allowingAttacks = canAttack(base)
    local vengence = allowingAttacks and
        (base.unitPoints >= AI_VENGENCE_SQUAD_COST) and
        ((getEnemyStructureCount(playerChunk) > 0) or
            (getCombinedDeathGeneratorRating(playerChunk) < Universe.retreatThreshold))

    local quadrant = Universe.random(4)

    local pX = playerChunk.x
    local pY = playerChunk.y
    local pXStart
    local pYStart
    local pXEnd
    local pYEnd

    if quadrant == 1 then
        pXStart = pX - PROCESS_PLAYER_BOUND
        pXEnd = pX
        pYStart = pY - PROCESS_PLAYER_BOUND
        pYEnd = pY
    elseif quadrant == 2 then
        pXStart = pX
        pXEnd = pX + PROCESS_PLAYER_BOUND
        pYStart = pY - PROCESS_PLAYER_BOUND
        pYEnd = pY
    elseif quadrant == 3 then
        pXStart = pX - PROCESS_PLAYER_BOUND
        pXEnd = pX
        pYStart = pY
        pYEnd = pY + PROCESS_PLAYER_BOUND
    elseif quadrant == 4 then
        pXStart = pX
        pXEnd = pX + PROCESS_PLAYER_BOUND
        pYStart = pY
        pYEnd = pY + PROCESS_PLAYER_BOUND
    end

    for x=pXStart, pXEnd, CHUNK_SIZE do
        for y=pYStart, pYEnd, CHUNK_SIZE do
            local chunk = getChunkByXY(map, x, y)

            if (chunk ~= -1) then
                if chunk.nestCount then
                    processNestActiveness(chunk, tick)

                    if vengence then
                        Universe.vengenceQueue[chunk.id] = chunk
                    end
                end
            end
        end
    end
end

local function processCleanUp(chunks, tick, duration)
    local chunkId, eventTick = next(chunks, nil)
    if not chunkId then
        return
    end
    if (tick - eventTick) > duration then
        chunks[chunkId] = nil
    end
end

function Processor.cleanUpMapTables(tick)
    local retreats = Universe.chunkToRetreats
    local rallys = Universe.chunkToRallys
    local drained = Universe.chunkToDrained

    for _=1,CLEANUP_QUEUE_SIZE do
        processCleanUp(retreats, tick, COOLDOWN_RETREAT)

        processCleanUp(rallys, tick, COOLDOWN_RALLY)

        processCleanUp(drained, tick, COOLDOWN_DRAIN)
    end
end

--[[
    Chunk generation is handled by events (on_chunk_generated) and the
    initial scan registers structures/generators. Pheromone processing for
    new chunks is driven by the dirty-chunk queue.
--]]

function Processor.processVengence()
    local vengenceQueue = Universe.vengenceQueue
    local chunkId, chunk = next(vengenceQueue, nil)
    if not chunkId then
        if (next(vengenceQueue) == nil) then
            Universe.vengenceQueue = {}
        end
        return
    end

    vengenceQueue[chunkId] = nil
    local map = chunk.map
    if not map.surface.valid then
        return
    end
    local base = chunk.base
    if base then
        if canMigrate(base) and (Universe.random() < 0.075) then
            formVengenceSettler(chunk)
        else
            formVengenceSquad(chunk)
        end
    end
end

function Processor.processNests(tick)
    local chunkId = Universe.processNestIterator
    local chunk
    if not chunkId then
        chunkId,chunk = next(Universe.chunkToNests, nil)
    else
        chunk = Universe.chunkToNests[chunkId]
    end
    if not chunkId then
        Universe.processNestIterator = nil
        return
    end

    Universe.processNestIterator = next(Universe.chunkToNests, chunkId)
    local map = chunk.map
    if not map.surface.valid then
        removeChunkToNest(chunkId)
        return
    end
    processNestActiveness(chunk, tick)

    if Universe.NEW_ENEMIES then
        processBaseMutation(chunk,
                            map,
                            chunk.base)
    end
end

local function processSpawnersBody(iterator, chunks)
    local chunkId = Universe[iterator]
    local chunk
    if not chunkId then
        chunkId,chunk = next(chunks, nil)
    else
        chunk = chunks[chunkId]
    end
    if not chunkId then
        Universe[iterator] = nil
        return
    end
    Universe[iterator] = next(chunks, chunkId)
    local map = chunk.map
    if not map.surface.valid then
        if (iterator == "processMigrationIterator") then
            removeChunkToNest(chunkId)
        else
            chunks[chunkId] = nil
        end
        return
    end
    local base = chunk.base
    local migrate = canMigrate(base)
    local attack = canAttack(base)
    if migrate then
        formSettlers(chunk)
    end
    if attack then
        formSquads(chunk)
    end
end

function Processor.processAttackWaves()
    processSpawnersBody(
        "processActiveSpawnerIterator",
        Universe.chunkToActiveNest
    )
    processSpawnersBody(
        "processActiveRaidSpawnerIterator",
        Universe.chunkToActiveRaidNest
    )
    processSpawnersBody(
        "processMigrationIterator",
        Universe.chunkToNests
    )
end

function Processor.processClouds(tick)
    local eventId, builderPack = next(Universe.settlePurpleCloud, nil)
    if not builderPack or (builderPack.tick > tick) then
        return
    end

    Universe.settlePurpleCloud[eventId] = nil
    local map = builderPack.map
    if not builderPack.group.valid or not map.surface.valid then
        return
    end

    setPositionInQuery(
        Universe.obaCreateBuildCloudQuery,
        builderPack.group.position
    )
    map.surface.create_entity(Universe.obaCreateBuildCloudQuery)
end

function Processor.processHives(tick)
    local entityId = Universe.hiveIterator
    local hiveData
    if not entityId then
        entityId, hiveData = next(Universe.activeHives, nil)
    else
        hiveData = Universe.activeHives[entityId]
    end
    if not entityId then
        Universe.hiveIterator = nil
        return
    end
    if tick < hiveData.tick then
        return
    end
    Universe.hiveIterator = next(Universe.activeHives, entityId)
    local base = hiveData.base
    local map = base.map
    if not map.surface.valid then
        Universe.activeHives[entityId] = nil
        return
    end
    if not hiveData.e.valid then
        Universe.activeHives[entityId] = nil
        Universe.hives[entityId] = nil
        return
    end
    hiveData.tick = tick +
        gaussianRandomRangeRG(
            linearInterpolation(Universe.evolutionLevel, MAX_HIVE_TTL, MIN_HIVE_TTL),
            DEV_HIVE_TTL,
            MIN_HIVE_TTL,
            MAX_HIVE_TTL,
            Universe.random
        )

    local timeDelay = 0

    TargetPosition.x = hiveData.position.x
    TargetPosition.y = hiveData.position.y

    distortPositionConcentricCircles(
        Universe.random,
        TargetPosition,
        EIGHTH_CHUNK_SIZE * hiveData.tier,
        EIGHTH_CHUNK_SIZE
    )

    if hiveData.nest < hiveData.maxNests then
        local entityType = "biter-spawner"
        if Universe.random() < 0.5 then
            entityType = "spitter-spawner"
        end
        queueCreation(base, TargetPosition, entityType, timeDelay, hiveData)
    elseif hiveData.turret < hiveData.maxTurrets then
        queueCreation(base, TargetPosition, "turret", timeDelay, hiveData)
    elseif hiveData.hive < hiveData.maxHives then
        queueCreation(base, TargetPosition, "hive", timeDelay, hiveData)
    else
        Universe.activeHives[entityId] = nil
    end
end

function Processor.cleanHivesData()
    local entityId = Universe.hiveDataIterator
    local hiveData
    if not entityId then
        entityId, hiveData = next(Universe.hiveData, nil)
    else
        hiveData = Universe.hiveData[entityId]
    end
    if not entityId then
        Universe.hiveDataIterator = nil
        return
    end

    Universe.hiveDataIterator = next(Universe.hiveData, entityId)
    if not Universe.hives[hiveData.hiveId] then
        Universe.hiveData[entityId] = nil
    end
end

function Processor.processPendingChunks(tick, flush)
    local pendingChunks = Universe.pendingChunks
    local eventId, event = next(pendingChunks, nil)

    if not eventId then
        if (next(pendingChunks) == nil) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.pendingChunks = {}
        end
        return
    end

    local endCount = 1
    local flushAllChunks = flush or Universe.flushPendingChunks
    if flushAllChunks then
        endCount = tableSize(pendingChunks)
        Universe.flushPendingChunks = false
    end
    for _=1,endCount do
        if not flushAllChunks and (event.tick > tick) then
            return
        end
        local newEventId, newEvent = next(pendingChunks, eventId)
        pendingChunks[eventId] = nil
        local map = event.map
        if not map.surface.valid then
            return
        end

        local topLeft = event.area.left_top
        local x = topLeft.x
        local y = topLeft.y

        if not map[x] then
            map[x] = {}
        end

        if map[x][y] then
            local oldChunk = map[x][y]
            local chunk = initialScan(oldChunk, map, tick)
            if (chunk == -1) then
                removeChunkFromMap(map, oldChunk)
            else
                markChunkDirty(chunk.id, 4, tick)
            end
        else
            local initialChunk = createChunk(map, x, y)
            map[x][y] = initialChunk
            local chunk = initialScan(initialChunk, map, tick)
            if (chunk ~= -1) then
                markChunkDirty(chunk.id, 4, tick)
            else
                map[x][y] = nil
            end
        end

        eventId = newEventId
        event = newEvent
        if not eventId then
            return
        end
    end
end

function Processor.processPendingUpgrades(tick)
    local upgradeId, entityData = next(Universe.pendingUpgrades, nil)
    if not upgradeId then
        if Universe.pendingUpgradesLength == 0 then
            Universe.pendingUpgrades = {}
        end
        return
    end
    local entity = entityData.entity
    if (entityData.state ~= 2) and not entity.valid then
        Universe.pendingUpgrades[upgradeId] = nil
        Universe.pendingUpgradesLength = Universe.pendingUpgradesLength - 1
        return
    end

    if entityData.delayTLL and tick < entityData.delayTLL then
        return
    end

    local state = entityData.state
    if state == 1 then
        local map = entityData.map
        unregisterEnemyBaseStructure(map, entity, nil, true)
        entity.destroy()
        if not entityData.name or not entityData.position then
            Universe.pendingUpgrades[upgradeId] = nil
            Universe.pendingUpgradesLength = Universe.pendingUpgradesLength - 1
            return
        end
        entityData.state = 2
    else
        Universe.pendingUpgrades[upgradeId] = nil
        Universe.pendingUpgradesLength = Universe.pendingUpgradesLength - 1
        local query = Queries.createEntityQuery
        local name = entityData.name
        query.name = name
        local position = entityData.position
        local surface = entityData.map.surface
        query.position = {position.x, position.y}
        local newEntity = surface.create_entity(query)
        if not (newEntity and newEntity.valid) then
            -- The original was destroyed in state 1: a blocked create position
            -- would LOSE the structure silently (observed: the upgrade-
            -- threshold scenario's intermittent no-replacement). Retry at the
            -- nearest free position; count if even that fails.
            if MetricsG then
                MetricsG.inc("upgrade.createBlocked")
            end
            local retryPosition = surface.find_non_colliding_position(
                name,
                position,
                16,
                1,
                true
            )
            if retryPosition then
                query.position = {retryPosition.x, retryPosition.y}
                newEntity = surface.create_entity(query)
            end
        end
        if newEntity and newEntity.valid then
            local hiveType = BUILDING_HIVE_TYPE_LOOKUP[name]
            if hiveType == "hive" then
                Universe.hives[newEntity.unit_number] = entityData.hive
            else
                local hiveData = entityData.hive
                if hiveData then
                    Universe.hiveData[newEntity.unit_number] = entityData.hive
                    local adjustedHiveType = (
                        (
                            (hiveType == "spitter-spawner")
                            or (hiveType == "biter-spawner")
                        )
                        and "nest"
                    ) or hiveType
                    hiveData[adjustedHiveType] = hiveData[adjustedHiveType] + 1
                end
            end
        end
        if remote.interfaces["kr-creep"] then
            remote.call(
                "kr-creep",
                "spawn_creep_at_position",
                surface,
                position,
                false,
                name
            )
        end
    end
end


function Processor.processScanChunks()
    local chunkId, chunk = next(Universe.chunkToPassScan, nil)
    if not chunkId then
        if (next(Universe.chunkToPassScan) == nil) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.chunkToPassScan = {}
        end
        return
    end

    Universe.chunkToPassScan[chunkId] = nil
    local map = chunk.map
    if not map.surface.valid then
        return
    end

    if (chunkPassScan(chunk, map) == -1) then
        removeChunkFromMap(map, chunk)
    end
end

function Processor.init(universe)
    Universe = universe
    Queries = universe.processorQueries
    if Queries then
        TargetPosition = Queries.targetPosition
    end
end

-- ============================================
-- ACO: Process Dirty Chunks
-- ============================================

-- PURE: Process a single dirty chunk (no side effects on Universe)
-- `counters` is a per-tick accumulator (a plain local table passed in by the
-- caller) so the hot path does not do a string-keyed Metrics hash lookup per
-- chunk; the caller flushes it to Metrics once per tick, keeping the public
-- metric names.
function Processor.processDirtyChunk(chunk, tick, priority, counters)
    if not chunk or not chunk.valid then
        return false, 0
    end

    -- Snapshot the maintained fields as LOCALS — the previous snapshot tables
    -- allocated twice per chunk (the single biggest per-chunk allocation). The
    -- predicate is identical: changed iff any field moved beyond both epsilons.
    -- The original short-circuited at the first changed field; this evaluates
    -- all of them (same boolean, no loop, no abs call).
    -- Only kamikaze and death are maintained here: base, enemy, resource and
    -- player are solved by the source-versioned solver and this pass does not
    -- write them, so their delta is identically 0.
    -- deathGenerator is included: it decays ONLY while the chunk is processed,
    -- so excluding it would freeze death clouds forever.
    local bKamikaze = chunk[KAMIKAZE_PHEROMONE] or 0
    local bDeath = chunk.deathGenerator or 0

    -- Neighbours are needed TWICE per chunk: the pheromone sum (inside
    -- processPheromone) and the propagation marking below. Compute them ONCE
    -- and pass them down. The shared NeighborChunks table is consumed
    -- immediately in each use, so one computation serves both.
    local neighbors
    if chunk and chunk.map then
        neighbors = MapUtils.getNeighborChunks(chunk.map, chunk.x, chunk.y)
    end

    -- Process pheromones
    MapUtils.processPheromone(chunk, tick, nil, neighbors)

    -- Only propagate when something materially changed
    local aKamikaze = chunk[KAMIKAZE_PHEROMONE] or 0
    local aDeath = chunk.deathGenerator or 0

    local d5 = aKamikaze - bKamikaze
    if d5 < 0 then d5 = -d5 end
    local d6 = aDeath - bDeath
    if d6 < 0 then d6 = -d6 end
    local a5 = bKamikaze
    if a5 < 0 then a5 = -a5 end
    local a6 = bDeath
    if a6 < 0 then a6 = -a6 end

    local changed = (d5 > PROPAGATION_ABSOLUTE_EPSILON and d5 > a5 * PROPAGATION_RELATIVE_EPSILON)
        or (d6 > PROPAGATION_ABSOLUTE_EPSILON and d6 > a6 * PROPAGATION_RELATIVE_EPSILON)

    -- Mark neighbors dirty DIRECTLY (no per-mark tables, no list handed to the
    -- drain). The previous version built a {id=, priority=} table per neighbor
    -- (up to 8 per chunk) for the drain to read back. markChunkDirty dedupes,
    -- so marking here is equivalent.
    if changed then
        if counters then
            counters.propagationMarks = counters.propagationMarks + 1
        end
        -- Neighbours were computed once above (chunks always carry a map
        -- reference; if a chunk ever lacks one the old pcall silently skipped
        -- its neighbours -- errors beyond this guard are real bugs).
        if neighbors then
            local neighborPriority = priority + 1
            if neighborPriority > 5 then neighborPriority = 5 end
            for i = 1, 8 do
                local neighbor = neighbors[i]
                if neighbor and neighbor ~= -1 then
                    MapUtils.markChunkDirty(neighbor.id, neighborPriority, tick)
                end
            end
        end
    elseif counters then
        counters.epsilonSkips = counters.epsilonSkips + 1
    end

    return true
end

-- PUBLIC: Process dirty chunks (reads storage.universe)
function Processor.processDirtyChunks(tick, universe)
    universe = universe or storage.universe
    local processed = 0
    -- This pass maintains the transient fields only (base/enemy/resource are
    -- solved by the source-versioned solver). Their decay is time-based
    -- (DECAY_REFERENCE_TICKS), so this budget sets freshness alone.
    local maxPerTick = Constants.PROCESS_TRANSIENT_CHUNKS_PER_TICK

    -- Per-tick metric accumulator: the hot path increments plain fields and this
    -- flushes to Metrics once at the end (string-keyed Metrics lookups per chunk
    -- were the same class as the queue keys).
    local counters = { propagationMarks = 0, epsilonSkips = 0, chunksProcessed = 0 }

    -- Intrusive bucket lists: dirtyHeads[p] is the head chunkId (0 = empty),
    -- dirtyNodes[chunkId] is the node {p, prev, next}. Unlink is O(1), so an
    -- upgrade moves the node instead of leaving a stale duplicate, and a chunk
    -- occupies exactly one slot.
    local heads = universe.dirtyHeads
    if not heads then return end
    local nodes = universe.dirtyNodes
    if not nodes then return end
    local tails = universe.dirtyTails

    -- Process by priority (1 = highest, 5 = lowest)
    for priority = 1, 5 do
        local chunkId = heads[priority]
        while processed < maxPerTick and chunkId ~= 0 do
            local node = nodes[chunkId]
            local nxt = node.next
            -- Unlink the head (O(1)).
            nodes[chunkId] = nil
            if nxt ~= 0 then nodes[nxt].prev = 0 else tails[priority] = 0 end
            heads[priority] = nxt
            universe.dirtyCount = universe.dirtyCount - 1

            -- Get chunk from map
            local chunk = MapUtils.getChunkById(chunkId, universe)
            local success = Processor.processDirtyChunk(chunk, tick, priority, counters)

            if success then
                processed = processed + 1
                counters.chunksProcessed = counters.chunksProcessed + 1
            end
            chunkId = nxt
        end

        if processed >= maxPerTick then break end
    end


    -- Flush the per-tick counters once (public metric names preserved).
    if MetricsG then
        if counters.propagationMarks > 0 then MetricsG.inc("dirty.propagationMarks", counters.propagationMarks) end
        if counters.epsilonSkips > 0 then MetricsG.inc("dirty.epsilonSkips", counters.epsilonSkips) end
        if counters.chunksProcessed > 0 then MetricsG.inc("dirty.chunksProcessed", counters.chunksProcessed) end
    end
end

ProcessorG = Processor
return Processor
