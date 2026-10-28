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
local SquadLogic = require("squad_logic")
local Constants = require("Constants")
local mFloor = math.floor
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
-- Per-chunk scan timing. [SUB] pendingChunks times the whole batch, so one
-- expensive chunk (a dense base, a big resource field, a first pass after a
-- long pause) hides inside it. Log each scan with its chunk coordinates and
-- the profiler when telemetry is on; the log then gives count / total / max
-- and the chunk that cost the most.
-- Chunk scans per tick. Steady state keeps up with a moving player without a
-- burst; the flush slice bounds the initial scan instead of doing hundreds of
-- chunks in one tick.
-- One chunk per tick (60/s at 60 UPS). The cost is not the average (32us/chunk)
-- but the worst case: a dense chunk reaches 4.9-9.5ms, so two per tick meant two
-- of those could land together. One per tick bounds a tick to a single chunk.
local SCAN_PER_TICK = 1
-- Load flush: on the FIRST load of a map (prepMap queues every generated chunk
-- with flush set) the whole backlog is scanned in one call, so the game reaches a
-- consistent state immediately instead of being half-blind for minutes. After
-- that the rate is 1/tick. The cap only guards against a pathological queue.
-- Bounded by MEASURED cost, not by a guess: chunk scans average 55us but p99 is
-- 289us and the worst chunk 3.7ms on a dense late-game save, so 3,000 per call was
-- 165ms minimum and 848ms at worst (the user's 40ms+ spike was this). 64 per call
-- is ~3.5ms average; the backlog then finishes at 32/tick.
local FLUSH_SCAN_PER_TICK = 64

local function timedInitialScan(chunk, map, tick)
    if not (MetricsG and MetricsG.isEnabled()) and not Constants.SUBSYSTEM_TELEMETRY then
        return initialScan(chunk, map, tick)
    end
    local profiler = helpers.create_profiler()
    local result = initialScan(chunk, map, tick)
    log({"", "[SCAN] tick=", tick, " chunk=", MapUtils.chunkX(chunk), ",", MapUtils.chunkY(chunk), " ", profiler})
    return result
end


local removeChunkFromMap = MapUtils.removeChunkFromMap
local chunkPassScan = ChunkUtils.chunkPassScan

local unregisterEnemyBaseStructure = ChunkUtils.unregisterEnemyBaseStructure
local setPositionInQuery = Utils.setPositionInQuery

local addPlayerGenerator = ChunkPropertyUtils.addPlayerGenerator
local getPlayerGeneratorChunks = ChunkPropertyUtils.getPlayerGeneratorChunks
local getDeathGeneratorChunks = ChunkPropertyUtils.getDeathGeneratorChunks
local decayDeathGenerator = ChunkPropertyUtils.decayDeathGenerator
local decayPlayerGenerator = ChunkPropertyUtils.decayPlayerGenerator
local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local removeChunkToNest = MapUtils.removeChunkToNest

local queueCreation = BaseUtils.queueCreation

local processBaseMutation = BaseUtils.processBaseMutation

local processNestActiveness = ChunkPropertyUtils.processNestActiveness

local formSquads = Squad.formSquads
local formVengenceSquad = Squad.formVengenceSquad
local formVengenceSettler = Squad.formVengenceSettler
local formSettlers = Squad.formSettlers

local getChunk = ChunkUtils.getChunk
local getChunkByPosition = MapUtils.getChunkByPosition
local getChunkByXY = MapUtils.getChunkByXY

local validPlayer = Utils.validPlayer

local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount

local canAttack = BaseUtils.canAttack
local canMigrate = BaseUtils.canMigrate

local tableSize = table_size

local next = next

-- module code

--[[
    Localized player radius where processing takes place in realtime, doesn't store state
    between calls.
--]]
-- The death generator's decay is time-based and must run whether or not the
-- chunk is touched by anything else, so its own registry drives it. The set is
-- one entry per chunk with a live death cloud.
-- Decay is time-based (rate^elapsed since the chunk was last touched), so it is
-- exact to run each chunk on a cadence rather than every tick. Spreading the set
-- over four ticks turns a 350us/tick loop into ~90us/tick with identical values.
local DECAY_CADENCE = 4

function Processor.processDeathGenerators(tick)
    local deathChunks = getDeathGeneratorChunks()
    if not deathChunks then return end
    local phase = tick % DECAY_CADENCE
    local profile = Universe.allocProfile
    local kbBefore = profile and collectgarbage("count") or 0
    local n = 0
    for id in pairs(deathChunks) do
        if (id % DECAY_CADENCE) == phase then
            n = n + 1
            local chunk = getChunk(id)
            if chunk then
                decayDeathGenerator(chunk, tick)
                if chunk.deathGenerator == nil then
                    deathChunks[id] = nil
                end
            else
                deathChunks[id] = nil
            end
        end
    end
    if profile then
        if MetricsG then
            MetricsG.allocAdd("dg.chunks", (collectgarbage("count") - kbBefore) * 1024)
        end
        profile["dg.count"] = (profile["dg.count"] or 0) + n
    end
end

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
                end
            end
        end
    end

    -- The player generator is the player field's source. Its decay is driven
    -- here rather than by the dirty pass: the pass would only reach a chunk it
    -- happens to process, and walking away does not mark the chunk left behind,
    -- so a stale source would keep the field pointing at where the player used
    -- to be. The set is one entry per player-occupied chunk.
    local playerGeneratorChunks = getPlayerGeneratorChunks()
    if playerGeneratorChunks then
        for id in pairs(playerGeneratorChunks) do
            local chunk = getChunk(id)
            if chunk then
                decayPlayerGenerator(chunk, tick)
                if chunk.playerGenerator == nil then
                    playerGeneratorChunks[id] = nil
                end
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
            (SquadLogic.deathGeneratorRating(playerChunk, tick) < Universe.retreatThreshold))

    local quadrant = Universe.random(4)

    local pX = MapUtils.chunkX(playerChunk)
    local pY = MapUtils.chunkY(playerChunk)
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
                        Universe.vengenceQueue[chunk.id] = true
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
    local chunkId = next(vengenceQueue, nil)
    if not chunkId then
        if (next(vengenceQueue) == nil) then
            Universe.vengenceQueue = {}
        end
        return
    end

    vengenceQueue[chunkId] = nil
    -- The queue is a membership set now; resolve the record or drop a stale id.
    local chunk = getChunk(chunkId)
    if not chunk then return end
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
        chunkId = next(Universe.chunkToNests, nil)
    else
        chunk = getChunk(chunkId)
    end
    if chunkId then chunk = chunk or getChunk(chunkId) end
    if not chunkId then
        Universe.processNestIterator = nil
        return
    end
    -- The chunk can be gone between two calls (surface release, chunk removal);
    -- the stored iterator key then resolves to nil.
    if not chunk then
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


-- Nest/upgrade creation telemetry. Counts by origin kind so a cluster of new
-- spawners can be attributed per range without a traceback: "upgrade" (the
-- in-place replacement), "nest"/"turret"/"hive" (queueCreation from a hive).
-- `near64` is how many landed within 64 tiles of the origin, `maxShift` how
-- far the placed entity moved from the requested position.
-- Cached spawn metric names: "spawn." .. kind concatenated per creation.
local SPAWN_METRIC = {}
local function spawnMetric(kind)
    local name = SPAWN_METRIC[kind]
    if not name then
        name = "spawn." .. kind
        SPAWN_METRIC[kind] = name
    end
    return name
end

local function spawnStats()
    local st = Universe.spawnTelemetry
    if not st then
        st = { total = 0, near64 = 0, maxShift = 0, blocked = 0 }
        Universe.spawnTelemetry = st
    end
    return st
end

local function processSpawnersBody(iterator, chunks)
    local chunkId = Universe[iterator]
    local chunk
    if not chunkId then
        chunkId = next(chunks, nil)
        chunk = getChunk(chunkId)
    else
        chunk = getChunk(chunkId)
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

local function scanBucketOf(event)
    local topLeft = event.area.left_top
    local px = Universe.scanFocusX or 0
    local py = Universe.scanFocusY or 0
    local dx = topLeft.x - px
    local dy = topLeft.y - py
    local bucket = mFloor(math.sqrt(dx * dx + dy * dy) / 256)
    if bucket > 15 then bucket = 15 end
    return bucket
end

--- Nearest-first pick: the lowest non-empty bucket whose front event is due.
--- The visible region comes online in seconds while the far map drains behind.
local function bucketOf(event)
    local topLeft = event.area.left_top
    local dx = topLeft.x - (Universe.scanFocusX or 0)
    local dy = topLeft.y - (Universe.scanFocusY or 0)
    local bucket = mFloor(math.sqrt(dx * dx + dy * dy) / 256)
    if bucket > 15 then bucket = 15 end
    return bucket
end

local function addToBucket(event)
    local buckets = Universe.pendingChunkBuckets
    if not buckets then
        buckets = {}
        Universe.pendingChunkBuckets = buckets
    end
    local bucket = bucketOf(event)
    event.bucket = bucket
    local bucketTable = buckets[bucket]
    if not bucketTable then
        bucketTable = {}
        buckets[bucket] = bucketTable
    end
    bucketTable[event.id] = event
end

local function pickPendingChunk(tick, pendingChunks)
    local buckets = Universe.pendingChunkBuckets
    if buckets then
        for b = 0, 15 do
            local bucket = buckets[b]
            if bucket then
                local id, event = next(bucket, nil)
                if id and ((event.tick or 0) <= tick) then
                    return id, event
                end
            end
        end
    end
    -- Fallback: an event can be in the queue without a bucket entry (queued by an
    -- older build, so its bucket field did not exist when it was persisted).
    -- Adopt it into a bucket on the way out so the fast path works again.
    for id, event in next, pendingChunks do
        if (event.tick or 0) <= tick then
            if not event.bucket then
                addToBucket(event)
            end
            return id, event
        end
    end
    return nil, nil
end

function Processor.processPendingChunks(tick, flush)
    local pendingChunks = Universe.pendingChunks
    local eventId, event = pickPendingChunk(tick, pendingChunks)

    if not eventId then
        if (next(pendingChunks) == nil) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.pendingChunks = {}
            Universe.pendingChunkBuckets = nil
            Universe.flushPendingChunks = false
        end
        return
    end

    -- Uniform distribution: a player moving fast generates several chunks per
    -- tick, and the flush path used to scan ALL queued chunks in one tick (9ms
    -- at map load, tens of ms on a large map). Scan a bounded number per tick
    -- and leave the rest queued, so moving produces a steady cost instead of a
    -- burst. The flush is merely a larger slice, not an unbounded one.
    local flushAllChunks = flush or Universe.flushPendingChunks
    Universe.flushPendingChunks = flushAllChunks
    local endCount
    if flushAllChunks then
        endCount = FLUSH_SCAN_PER_TICK
    else
        endCount = SCAN_PER_TICK
    end
    for _=1,endCount do
        if not flushAllChunks and (event.tick > tick) then
            return
        end
        local newEventId, newEvent = pickPendingChunk(tick, pendingChunks)
        pendingChunks[eventId] = nil
        local buckets = Universe.pendingChunkBuckets
        if buckets and event.bucket then
            local bucket = buckets[event.bucket]
            if bucket then bucket[eventId] = nil end
        end
        local map = event.map
        if not map.surface.valid then
            return
        end

        local topLeft = event.area.left_top
        local x = topLeft.x
        local y = topLeft.y

        if not map.chunks then
            map.chunks = {}
        end
        local chunkKey = MapUtils.chunkKey(x, y)

        if map.chunks[chunkKey] then
            -- A chunk that already exists is being scanned again: either a
            -- re-queue (rescan) or a duplicate queue entry.
            if MetricsG then MetricsG.inc("scan.repeat") end
            local oldChunk = map.chunks[chunkKey]
            local chunk = timedInitialScan(oldChunk, map, tick)
            if (chunk == -1) then
                removeChunkFromMap(map, oldChunk)
            end
        else
            local initialChunk = createChunk(map, x, y)
            map.chunks[chunkKey] = initialChunk
            local chunk = timedInitialScan(initialChunk, map, tick)
            if (chunk == -1) then
                map.chunks[chunkKey] = nil
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
    -- The entry is recycled at the END of this call: the branches below keep
    -- reading entityData after they drop it from the queue.
    local toRelease
    local upgradeId, entityData = next(Universe.pendingUpgrades, nil)
    if not upgradeId then
        if Universe.pendingUpgradesLength == 0 then
            Universe.pendingUpgrades = {}
        end
        return
    end
    local entity = entityData.entity
    if (entityData.state ~= 2) and not entity.valid then
        toRelease = Universe.pendingUpgrades[upgradeId]
        Universe.pendingUpgrades[upgradeId] = nil
        Universe.pendingUpgradesLength = Universe.pendingUpgradesLength - 1
        BaseUtils.releasePendingUpgradeEntry(toRelease)
        toRelease = nil
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
            toRelease = Universe.pendingUpgrades[upgradeId]
            Universe.pendingUpgrades[upgradeId] = nil
            Universe.pendingUpgradesLength = Universe.pendingUpgradesLength - 1
            return
        end
        entityData.state = 2
    else
        toRelease = Universe.pendingUpgrades[upgradeId]
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
            if TelemetryG and TelemetryG.isEnabled() then
                TelemetryG.emit("upgrade.blocked", {
                    name = name, x = position.x, y = position.y,
                    kind = entityData.kind, base = entityData.baseId,
                })
            end
            spawnStats().blocked = spawnStats().blocked + 1
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
            if not (newEntity and newEntity.valid) then
                -- The upgrade produced nothing. That is allowed - it is a
                -- no-op, not a devolution - but the structure's identity must
                -- drop to 0 so it is a candidate again instead of being
                -- remembered as upgraded. Negative would mean "worse than the
                -- previous tier"; zero means "no rampant identity".
                local base = entityData.base
                local oldUnitNumber = entityData.oldUnitNumber
                if base and base.structures and oldUnitNumber then
                    base.structures[oldUnitNumber] = 0
                    if base.structureEntities then
                        base.structureEntities[oldUnitNumber] = nil
                    end
                end
                if TelemetryG and TelemetryG.isEnabled() then
                    TelemetryG.emit("upgrade.none", {
                        name = name, base = base and base.id or nil,
                        x = position.x, y = position.y,
                    })
                end
            end
        end
        if newEntity and newEntity.valid then
            local st = spawnStats()
            local kind = entityData.kind or "other"
            st.total = st.total + 1
            st[kind] = (st[kind] or 0) + 1
            local dx = newEntity.position.x - position.x
            local dy = newEntity.position.y - position.y
            local shift = math.sqrt(dx * dx + dy * dy)
            if shift > st.maxShift then st.maxShift = shift end
            if (math.abs(newEntity.position.x) < 64) and (math.abs(newEntity.position.y) < 64) then
                st.near64 = st.near64 + 1
            end
            if MetricsG then
                MetricsG.inc(spawnMetric(kind))
                MetricsG.highWater("spawn.shift", shift)
            end
            if TelemetryG and TelemetryG.isEnabled() then
                TelemetryG.emit("upgrade.create", {
                    name = name, kind = kind,
                    x = position.x, y = position.y,
                    shift = shift, base = entityData.baseId,
                    id = newEntity.unit_number,
                })
            end
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
    BaseUtils.releasePendingUpgradeEntry(toRelease)
end


function Processor.processScanChunks()
    local chunkId = next(Universe.chunkToPassScan, nil)
    local chunk = getChunk(chunkId)
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

ProcessorG = Processor
return Processor
