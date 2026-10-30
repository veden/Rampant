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


if ChunkPropertyUtilsG then
    return ChunkPropertyUtilsG
end
local ChunkPropertyUtils = {}

--

local Universe
local Position
local SourceVersionedFieldReader

--

local Constants = require("Constants")
local Utils = require("Utils")
local chunkX = Utils.chunkX
local setAreaInQueryChunkSize = Utils.setAreaInQueryChunkSize
local chunkY = Utils.chunkY
local MathUtils = require("MathUtils")

-- Constants

local DURATION_ACTIVE_NEST = Constants.DURATION_ACTIVE_NEST

local PLAYER_GENERATOR_PERSISTANCE = Constants.PLAYER_GENERATOR_PERSISTANCE
local PLAYER_PHEROMONE_GENERATOR_AMOUNT = Constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local COOLDOWN_DRAIN = Constants.COOLDOWN_DRAIN

local RAIDING_MINIMUM_BASE_THRESHOLD = Constants.RAIDING_MINIMUM_BASE_THRESHOLD

local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE
local DEATH_GAIN_PHEROMONE = Constants.DEATH_GAIN_PHEROMONE
local DEATH_LOSS_PHEROMONE = Constants.DEATH_LOSS_PHEROMONE
local DEATH_FIELD_SOURCE_SCALE = Constants.DEATH_FIELD_SOURCE_SCALE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE

local MOVEMENT_GENERATOR_PERSISTANCE = Constants.MOVEMENT_GENERATOR_PERSISTANCE
local MOVEMENT_GENERATOR_DECAY_PER_TICK = Constants.MOVEMENT_GENERATOR_DECAY_PER_TICK
local PLAYER_GENERATOR_DECAY_PER_TICK = Constants.PLAYER_GENERATOR_DECAY_PER_TICK
local MOVEMENT_GENERATOR_DECAY_TABLE = Constants.DECAY_MOVEMENT_GENERATOR_PER_TICK
local PLAYER_GENERATOR_DECAY_TABLE = Constants.DECAY_PLAYER_GENERATOR_PER_TICK
local DECAY_TABLE_DEPTH = Constants.DECAY_TABLE_DEPTH
local CHUNK_ALL_DIRECTIONS = Constants.CHUNK_ALL_DIRECTIONS

local MAGIC_MAXIMUM_NUMBER = Constants.MAGIC_MAXIMUM_NUMBER

-- imported functions

local euclideanDistancePoints = MathUtils.euclideanDistancePoints
local tableSize = table_size

local mMin = math.min

-- module code

-- Injected by init (control.lua) so this module stays free of a direct
-- dependency on the runtime. The player field is source-versioned: every write
-- to chunk.playerGenerator is a source change the solver has to hear about.
local SourceVersionedFieldRuntime

function ChunkPropertyUtils.setSourceVersionedFieldRuntime(runtime)
    SourceVersionedFieldRuntime = runtime
end

-- The kamikaze field is the player field's sources at a faster decay, scaled
-- 0.95 (see the runtime's FIELD_KAMIKAZE). Both are driven by the one generator,
-- so one write queues both.
local KAMIKAZE_SCALE = 0.95

-- A source change below this fraction of the current value is not queued: the
-- player generator decays every tick, so every tick used to queue a player and a
-- kamikaze change, and those two fields consumed the whole 96-pop quantum every
-- tick (measured wPlayer=33 wKamikaze=34 per tick, sourceFields avg 0.6ms).
-- The field tolerates eps = 5e-4, so skipping a 5% step is far inside its noise,
-- and it cuts the propagation rate by roughly 5x.
local PLAYER_SOURCE_CHANGE_FRACTION = 0.05

-- A source change is queued when it differs from what the FIELD was last told, by
-- at least this fraction of that value, or when this many ticks have passed (so a
-- slow decay still tracks). Throttling against the current value instead compounds:
-- each step is measured against an already-lagging value, and the field drifts
-- arbitrarily far under the exact solve (measured: 116 base chunks 12% low, then
-- 7,201 chunks with maxRel 100% once the throttle also filtered increases).
local SOURCE_CHANGE_FLUSH_TICKS = 300

local function shouldQueue(chunk, stateKey, value, tick)
    local queued = chunk[stateKey]
    local queuedTick = chunk[stateKey .. "Tick"]
    if queued == nil then
        chunk[stateKey] = value
        chunk[stateKey .. "Tick"] = tick
        return true
    end
    local delta = value - queued
    if delta < 0 then delta = -delta end
    local reference = queued < 0 and -queued or queued
    local flush = not queuedTick or (tick - queuedTick) >= SOURCE_CHANGE_FLUSH_TICKS
    if reference == 0 or delta >= reference * PLAYER_SOURCE_CHANGE_FRACTION or flush then
        chunk[stateKey] = value
        chunk[stateKey .. "Tick"] = tick
        return true
    end
    return false
end

local function queuePlayerSourceChange(chunk, oldValue, newValue)
    local runtime = SourceVersionedFieldRuntime
    if not runtime or not runtime.isInitialized() then return end
    if not chunk or chunk.id == nil then return end
    if oldValue == newValue then return end
    if not shouldQueue(chunk, "playerGenReported", newValue, game.tick) then return end
    -- Report the delta since the LAST REPORTED value, not since the previous tick.
    local reported = chunk.playerGenReportedPrev or oldValue
    chunk.playerGenReportedPrev = newValue
    runtime.queueSource(chunk, PLAYER_PHEROMONE, reported, newValue)
    runtime.queueSource(chunk, KAMIKAZE_PHEROMONE,
        reported * KAMIKAZE_SCALE, newValue * KAMIKAZE_SCALE)
end

-- The death field is signed, so one generator write queues two fields (the
-- gain and the loss), each carrying its own side of the clamped combined value.
-- The scale must match the runtime's sourceValue or the decrease comparison in
-- Runtime.process sees a spurious change.
local function queueDeathSourceChange(chunk, oldCombined)
    local runtime = SourceVersionedFieldRuntime
    if not runtime or not runtime.isInitialized() then return end
    if not chunk or chunk.id == nil then return end
    local newCombined = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    if oldCombined == newCombined then return end
    -- Smooth decay must not queue a removal every tick: each one is a local
    -- re-propagation, and on a save with 197 decaying generators they arrive
    -- faster than the per-tick removal budget retires them, so the leftover heaps
    -- grew without bound (measured 496,018 nodes, 377MB of Lua state, 87KB/tick
    -- allocated). Only propagate a decrease once it is a visible fraction of the
    -- value, and always propagate extinction.
    -- Same rule as the player generator: throttle against the last reported value
    -- so the drift is bounded, and flush periodically so slow decay still tracks.
    if not shouldQueue(chunk, "deathGenReported", newCombined, game.tick) then return end
    oldCombined = chunk.deathGenReportedPrev or oldCombined
    chunk.deathGenReportedPrev = newCombined
    local oldGain = oldCombined > 0 and oldCombined or 0
    local oldLoss = oldCombined < 0 and -oldCombined or 0
    local newGain = newCombined > 0 and newCombined or 0
    local newLoss = newCombined < 0 and -newCombined or 0
    if oldGain ~= newGain then
        runtime.queueSource(chunk, DEATH_GAIN_PHEROMONE,
            oldGain * DEATH_FIELD_SOURCE_SCALE, newGain * DEATH_FIELD_SOURCE_SCALE)
    end
    if oldLoss ~= newLoss then
        runtime.queueSource(chunk, DEATH_LOSS_PHEROMONE,
            oldLoss * DEATH_FIELD_SOURCE_SCALE, newLoss * DEATH_FIELD_SOURCE_SCALE)
    end
end

-- Chunks holding a live death generator. Its decay is time-based and must run
-- whether or not anything else touches the chunk, so the set is tracked and
-- refreshed on a pick rather than relying on a chunk being queued.
local function registerDeathGeneratorChunk(chunk)
    if Universe.deathGeneratorChunks then
        Universe.deathGeneratorChunks[chunk.id] = true
    end
end

function ChunkPropertyUtils.getDeathGeneratorChunks()
    return Universe.deathGeneratorChunks
end

-- Chunks holding a live player generator. A generator only decays while its
-- chunk is processed, and nothing re-marks the chunk a player walked away from
-- (processPlayers marks only the current one), so the set is tracked explicitly
-- and marked each pass. Without this a stale source would keep the player field
-- pointing at where the player used to be.
local function registerPlayerGeneratorChunk(chunk)
    if Universe.playerGeneratorChunks then
        Universe.playerGeneratorChunks[chunk.id] = true
    end
end

local function unregisterPlayerGeneratorChunk(chunk)
    if Universe.playerGeneratorChunks then
        Universe.playerGeneratorChunks[chunk.id] = nil
    end
end

function ChunkPropertyUtils.getPlayerGeneratorChunks()
    return Universe.playerGeneratorChunks
end

local function getActiveTick(chunk)
    return chunk.activeTick or 0
end

local function registerActiveSpawner(chunk, register, activeType, tick)
    local chunkId = chunk.id
    if Universe[register][chunkId] then
        return
    end

    chunk.activeTick = tick + DURATION_ACTIVE_NEST

    local base = chunk.base
    base[activeType] = base[activeType] + 1
    Universe[register][chunkId] = true
end

local function unregisterActiveSpawner(chunk, register, iterator, activeType)
    local chunkId = chunk.id
    if not Universe[register][chunkId] then
        return
    end

    chunk.activeTick = nil

    local base = chunk.base
    base[activeType] = base[activeType] - 1
    Universe[register][chunkId] = nil
    if (Universe[iterator] == chunkId) then
        Universe[iterator] = nil
    end
end

local function registerActiveNest(chunk, tick)
    registerActiveSpawner(chunk, "chunkToActiveNest", "activeNests", tick)
end

local function registerActiveRaidNest(chunk, tick)
    registerActiveSpawner(chunk, "chunkToActiveRaidNest", "activeRaidNests", tick)
end

local function unregisterActiveNest(chunk)
    unregisterActiveSpawner(
        chunk,
        "chunkToActiveNest",
        "processActiveSpawnerIterator",
        "activeNests"
    )
end

local function unregisterActiveRaidNest(chunk)
    unregisterActiveSpawner(
        chunk,
        "chunkToActiveRaidNest",
        "processActiveRaidSpawnerIterator",
        "activeRaidNests"
    )
end

local function addEnemyStructure(chunk, unitNumber, ids, counts, register)
    local chunkId = chunk.id
    local entityIds = chunk[ids]
    if not entityIds then
        entityIds = {}
        chunk[ids] = entityIds
    end

    if entityIds[unitNumber] then
        return false
    end

    entityIds[unitNumber] = true
    chunk[counts] = (chunk[counts] or 0) + 1
    if register then
        Universe[register][chunkId] = true
    end
    return true
end

local function removeEnemyStructure(chunk, unitNumber, ids, counts, register)
    local chunkId = chunk.id
    local entityIds = chunk[ids]
    if not (entityIds and entityIds[unitNumber]) then
        return false
    end

    entityIds[unitNumber] = nil
    chunk[counts] = chunk[counts] - 1
    if chunk[counts] > 0 then
        return true
    end

    chunk[ids] = nil
    chunk[counts] = nil
    if register then
        Universe[register][chunkId] = nil
    end
    if (ids == "nestIds") then
        unregisterActiveNest(chunk)
        unregisterActiveRaidNest(chunk)
        if (Universe.processMigrationIterator == chunkId) then
            Universe.processMigrationIterator = nil
        end
        if (Universe.processNestIterator == chunkId) then
            Universe.processNestIterator = nil
        end
    end
    return true
end

function ChunkPropertyUtils.addTurretCount(chunk, unitNumber)
    return addEnemyStructure(chunk, unitNumber, "turretIds", "turretCount")
end

function ChunkPropertyUtils.removeTurretCount(chunk, unitNumber)
    return removeEnemyStructure(chunk, unitNumber, "turretIds", "turretCount")
end

function ChunkPropertyUtils.addUtilitiesCount(chunk, unitNumber)
    return addEnemyStructure(chunk, unitNumber, "utilityIds", "utilityCount", "chunkToUtilities")
end

function ChunkPropertyUtils.removeUtilitiesCount(chunk, unitNumber)
    return removeEnemyStructure(chunk, unitNumber, "utilityIds", "utilityCount", "chunkToUtilities")
end

function ChunkPropertyUtils.addHiveCount(chunk, unitNumber)
    return addEnemyStructure(chunk, unitNumber, "hiveIds", "hiveCount", "chunkToHives")
end

function ChunkPropertyUtils.removeHiveCount(chunk, unitNumber)
    return removeEnemyStructure(chunk, unitNumber, "hiveIds", "hiveCount", "chunkToHives")
end

function ChunkPropertyUtils.addNestCount(chunk, unitNumber)
    return addEnemyStructure(chunk, unitNumber, "nestIds", "nestCount", "chunkToNests")
end

function ChunkPropertyUtils.removeNestCount(chunk, unitNumber)
    return removeEnemyStructure(chunk, unitNumber, "nestIds", "nestCount", "chunkToNests")
end

function ChunkPropertyUtils.isActiveNest(chunk)
    return Universe.chunkToActiveNest[chunk.id] ~= nil
end

function ChunkPropertyUtils.isActiveRaidNest(chunk)
    return Universe.chunkToActiveRaidNest[chunk.id] ~= nil
end

function ChunkPropertyUtils.addBaseResourceChunk(base, chunk)
    if not chunk.resourceGenerator then
        return
    end

    base.resourceChunkCount = base.resourceChunkCount + 1
    base.resourceChunks[chunk.id] = true
end

function ChunkPropertyUtils.removeBaseResourceChunk(base, chunk)
    if not base.resourceChunks[chunk.id] then
        return
    end

    base.resourceChunkCount = base.resourceChunkCount - 1
    base.resourceChunks[chunk.id] = nil
end

function ChunkPropertyUtils.removeChunkBase(chunk, base)
    if not chunk.base then
        return
    end

    base.chunkCount = base.chunkCount - 1
    base.totalX = base.totalX - chunkX(chunk)
    base.totalY = base.totalY - chunkY(chunk)
    base.x = base.totalX / base.chunkCount
    base.y = base.totalY / base.chunkCount
    chunk.base = nil
end

function ChunkPropertyUtils.setChunkBase(chunk, base)
    if chunk.base then
        return
    end

    base.chunkCount = base.chunkCount + 1
    base.totalX = base.totalX + chunkX(chunk)
    base.totalY = base.totalY + chunkY(chunk)
    base.x = base.totalX / base.chunkCount
    base.y = base.totalY / base.chunkCount
    chunk.base = base
end

function ChunkPropertyUtils.getEnemyStructureCount(chunk)
    return (chunk.nestCount or 0)
        + (chunk.turretCount or 0)
        + (chunk.utilityCount or 0)
        + (chunk.hiveCount or 0)
end

function ChunkPropertyUtils.setSourceVersionedFieldReader(reader)
    SourceVersionedFieldReader = reader
end

local function readField(fieldType, chunk, rawValue)
    if SourceVersionedFieldReader then
        local value = SourceVersionedFieldReader(fieldType, chunk)
        if value ~= nil then
            return value
        end
    end
    return rawValue
end

function ChunkPropertyUtils.setDrainPylons(map, entity1, entity2)
    local pair = {entity1, entity2}
    map.drainPylons[entity1.unit_number] = pair
    map.drainPylons[entity2.unit_number] = pair
end

function ChunkPropertyUtils.removeDrainPylons(map, unitNumber)
    local pair = map.drainPylons[unitNumber]
    if pair then
        local target = pair[1]
        local pole = pair[2]
        if target.unit_number == unitNumber then
            map.drainPylons[unitNumber] = nil
            if pole.valid then
                map.drainPylons[pole.unit_number] = nil
                pole.destroy()
            end
        elseif (pole.unit_number == unitNumber) then
            map.drainPylons[unitNumber] = nil
            if target.valid then
                map.drainPylons[target.unit_number] = nil
                target.die()
            end
        end
    end
end

function ChunkPropertyUtils.getDrainPylonPair(map, unitNumber)
    return map.drainPylons[unitNumber]
end

function ChunkPropertyUtils.isDrained(chunk, tick)
    local drainTick = Universe.chunkToDrained[chunk.id]
    if not drainTick then
        return false
    end
    return (tick - drainTick) < COOLDOWN_DRAIN
end

function ChunkPropertyUtils.setDrainedTick(chunk, tick)
    Universe.chunkToDrained[chunk.id] = tick
end

function ChunkPropertyUtils.getRetreatTick(chunk)
    return Universe.chunkToRetreats[chunk.id] or 0
end

function ChunkPropertyUtils.getRallyTick(chunk)
    return Universe.chunkToRallys[chunk.id] or 0
end

function ChunkPropertyUtils.setRallyTick(chunk, tick)
    Universe.chunkToRallys[chunk.id] = tick
end

function ChunkPropertyUtils.setRetreatTick(chunk, tick)
    Universe.chunkToRetreats[chunk.id] = tick
end

function ChunkPropertyUtils.setResourceGenerator(chunk, resourceGenerator)
    if (resourceGenerator <= 0) then
        chunk.resourceGenerator = nil
    else
        chunk.resourceGenerator = resourceGenerator
    end
end

function ChunkPropertyUtils.addResourceGenerator(chunk, delta)
    chunk.resourceGenerator = (chunk.resourceGenerator or 0) + delta
end

function ChunkPropertyUtils.getPassable(chunk)
    return chunk.passable or CHUNK_ALL_DIRECTIONS
end

function ChunkPropertyUtils.setPassable(chunk, value)
    if (value == CHUNK_ALL_DIRECTIONS) then
        chunk.passable = nil
    else
        chunk.passable = value
    end
end

function ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    local amount = (chunk.deathGenerator or 0) + (chunk.permanentDeathGenerator or 0)
    if (amount > 1) then
        return 1
    elseif (amount < -1) then
        return -1
    else
        return amount
    end
end

function ChunkPropertyUtils.addPermanentDeathGenerator(chunk, amount)
    local oldCombined = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    local adjustedAmount = (amount * 0.25) + (chunk.permanentDeathGenerator or 0)
    if (adjustedAmount > 0.75) then
        chunk.permanentDeathGenerator = 0.75
    elseif (adjustedAmount < -0.75) then
        chunk.permanentDeathGenerator = -0.75
    else
        chunk.permanentDeathGenerator = adjustedAmount
    end
    queueDeathSourceChange(chunk, oldCombined)
end

function ChunkPropertyUtils.addDeathGenerator(chunk, value)
    local oldCombined = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    -- A deposit restarts the decay interval, so the decay stays time-based.
    chunk.deathGeneratorDecayTick = nil
    local currentAmount = (chunk.deathGenerator or 0) + value
    if (currentAmount > 1) then
        chunk.deathGenerator = 1
    elseif (currentAmount < -1) then
        chunk.deathGenerator = -1
    else
        chunk.deathGenerator = currentAmount
    end
    registerDeathGeneratorChunk(chunk)
    queueDeathSourceChange(chunk, oldCombined)
end

-- PUBLIC: Add victory generator (reads storage.universe)
function ChunkPropertyUtils.addVictoryGenerator(chunk, value)
    local universe = storage.universe
    local cToV = universe.chunkToVictory
    local chunkId = chunk.id
    
    if not cToV[chunkId] then
        cToV[chunkId] = { v = 0 }
    end
    cToV[chunkId].v = cToV[chunkId].v + value
end

-- PURE: Calculate decayed death generator value
function ChunkPropertyUtils.calculateDeathGeneratorDecay(currentValue, persistence)
    if not currentValue then
        return nil
    end
    
    local v = currentValue * persistence
    if (v < 0.0001) and (v > -0.0001) then
        return nil
    else
        return v
    end
end

-- IMPURE: Apply decayed death generator to chunk
function ChunkPropertyUtils.applyDeathGeneratorDecay(chunk, newValue)
    local oldCombined = ChunkPropertyUtils.getCombinedDeathGenerator(chunk)
    chunk.deathGenerator = newValue
    queueDeathSourceChange(chunk, oldCombined)
end

-- PUBLIC: Decay death generator (wrapper).
-- Time-based: the factor is per tick against a fixed reference cadence, so the
-- maintenance cadence no longer sets the cloud's lifetime. The writers clear the
-- timestamp, so a deposit restarts the interval.
function ChunkPropertyUtils.decayDeathGenerator(chunk, tick)
    local gen = chunk.deathGenerator
    if not gen then return end
    local last = chunk.deathGeneratorDecayTick
    chunk.deathGeneratorDecayTick = tick
    if not last then return end
    local elapsed = tick - last
    if elapsed <= 0 then return end
    local factor = MOVEMENT_GENERATOR_DECAY_TABLE[elapsed]
        or MOVEMENT_GENERATOR_DECAY_TABLE[DECAY_TABLE_DEPTH]
    local newValue = ChunkPropertyUtils.calculateDeathGeneratorDecay(gen, factor)
    ChunkPropertyUtils.applyDeathGeneratorDecay(chunk, newValue)
end

-- PURE: Calculate decayed player generator value
function ChunkPropertyUtils.calculatePlayerGeneratorDecay(currentValue, persistence)
    if not currentValue then
        return nil
    end
    
    local v = currentValue * persistence
    if (v < 0.0001) and (v > -0.0001) then
        return nil
    else
        return v
    end
end

-- IMPURE: Apply decayed player generator to chunk
function ChunkPropertyUtils.applyPlayerGeneratorDecay(chunk, newValue)
    local oldValue = chunk.playerGenerator or 0
    chunk.playerGenerator = newValue
    if newValue == nil then
        unregisterPlayerGeneratorChunk(chunk)
    end
    queuePlayerSourceChange(chunk, oldValue, newValue or 0)
end

-- PUBLIC: Decay player generator (wrapper). Time-based, as above.
function ChunkPropertyUtils.decayPlayerGenerator(chunk, tick)
    local gen = chunk.playerGenerator
    if not gen then return end
    local last = chunk.playerGeneratorDecayTick
    chunk.playerGeneratorDecayTick = tick
    if not last then return end
    local elapsed = tick - last
    if elapsed <= 0 then return end
    local factor = PLAYER_GENERATOR_DECAY_TABLE[elapsed]
        or PLAYER_GENERATOR_DECAY_TABLE[DECAY_TABLE_DEPTH]
    local newValue = ChunkPropertyUtils.calculatePlayerGeneratorDecay(gen, factor)
    ChunkPropertyUtils.applyPlayerGeneratorDecay(chunk, newValue)
end

function ChunkPropertyUtils.addPlayerGenerator(chunk, playerMaxGenerator)
    local oldValue = chunk.playerGenerator or 0
    -- A deposit restarts the decay interval, so the decay stays time-based.
    chunk.playerGeneratorDecayTick = nil
    local value = chunk.playerGenerator
    if value then
        chunk.playerGenerator = mMin(
            value + PLAYER_PHEROMONE_GENERATOR_AMOUNT,
            playerMaxGenerator
        )
    else
        chunk.playerGenerator = PLAYER_PHEROMONE_GENERATOR_AMOUNT
    end
    registerPlayerGeneratorChunk(chunk)
    queuePlayerSourceChange(chunk, oldValue, chunk.playerGenerator or 0)
end

function ChunkPropertyUtils.addSquadToChunk(chunk, squad)
    if (chunk ~= -1)
        and (
            (squad.chunk == -1)
            or (squad.chunk.id ~= chunk.id)
        )
    then
        ChunkPropertyUtils.removeSquadFromChunk(squad)
        local squads = chunk.squads
        if not squads then
            squads = {}
            chunk.squads = squads
        end
        squads[squad.id] = squad
        squad.chunk = chunk
    end
end

function ChunkPropertyUtils.removeSquadFromChunk(squad)
    local chunk = squad.chunk
    if (chunk ~= -1) then
        local squads = chunk.squads
        if squads then
            squads[squad.id] = nil
            if (next(squads) == nil) then
                chunk.squads = nil
            end
        end
    end
end

function ChunkPropertyUtils.setPlayerBaseGenerator(chunk, playerBaseGenerator)
    if (playerBaseGenerator <= 0) then
        chunk.playerBaseGenerator = nil
        return 0
    else
        chunk.playerBaseGenerator = playerBaseGenerator
        return playerBaseGenerator
    end
end

function ChunkPropertyUtils.addPlayerBaseGenerator(chunk, playerBaseGenerator)
    local amount = (chunk.playerBaseGenerator or 0) + playerBaseGenerator
    if amount <= 0 then
        chunk.playerBaseGenerator = nil
        return 0
    else
        chunk.playerBaseGenerator = amount
        return amount
    end
end

function ChunkPropertyUtils.findNearbyBase(chunk)
    -- getChunkByPosition returns the number -1 when a position is outside the
    -- tracked grid, and several callers pass that sentinel straight through.
    -- Indexing it killed the death handler:
    --   attempt to index local 'chunk' (a number value)
    if type(chunk) ~= "table" then
        return nil
    end
    local foundBase = chunk.base
    if foundBase then
        return foundBase
    end

    return ChunkPropertyUtils.findNearbyBaseByPosition(
        chunk.map,
        chunkX(chunk),
        chunkY(chunk)
    )
end

function ChunkPropertyUtils.findNearbyBaseByPosition(map, x, y)
    -- A surface can be untracked or released for having no enemies, so callers
    -- that resolve a map from a surface index can hand us nil.
    if not map then return nil end
    local foundBase

    local closest = MAGIC_MAXIMUM_NUMBER
    for _, base in pairs(map.bases) do
        local distance = euclideanDistancePoints(base.x, base.y, x, y)
        if (distance <= base.distanceThreshold) and (distance < closest) then
            closest = distance
            foundBase = base
        end
    end

    return foundBase
end

--- The spawners on one chunk, resolved once per activeness check.
local function chunkSpawners(chunk)
    local query = Universe.isFilteredEntitiesSpawnerQuery
    local map = chunk.map
    if not query or not map or not map.surface then return nil end
    setAreaInQueryChunkSize(query, chunkX(chunk), chunkY(chunk))
    return map.surface.find_entities_filtered(query)
end

--- The chunk's pollution. Pollution is per chunk, so the position only has to be inside it.
local function pollutionAt(surface, x, y)
    Position.x = x
    Position.y = y
    if MetricsG then MetricsG.inc("pollution.samples") end
    return surface.get_pollution(Position)
end

function ChunkPropertyUtils.processNestActiveness(chunk, tick)
    if getActiveTick(chunk) > tick then
        return
    end
    if not chunk.nestCount then
        unregisterActiveNest(chunk)
        unregisterActiveRaidNest(chunk)
        return
    end
    -- The entities are the signal, both for "is there anything here" and for pollution.
    local spawners = chunkSpawners(chunk)
    if not spawners or #spawners == 0 then
        -- A chunk whose nests are all gone (a destroyed spawner between two passes) must not
        -- keep an active mark; the registry count alone cannot tell.
        if MetricsG then MetricsG.inc("nest.noEntity") end
        unregisterActiveNest(chunk)
        unregisterActiveRaidNest(chunk)
        return
    end
    local surface = chunk.map.surface
    local playerPheromone = readField(PLAYER_PHEROMONE, chunk, chunk[PLAYER_PHEROMONE] or 0)
    local basePheromone = readField(BASE_PHEROMONE, chunk, chunk[BASE_PHEROMONE] or 0)

    if Universe.attackUsePlayer and (playerPheromone > Universe.attackPlayerThreshold) then
        registerActiveNest(chunk, tick)
        return
    end
    if not (basePheromone > 0) then
        unregisterActiveNest(chunk)
        unregisterActiveRaidNest(chunk)
        return
    end

    -- Pollution, two halves:
    --
    --   * the CHUNK's pollution, one read (it is per chunk: "Pollution is stored per chunk, so
    --     this will return the same value for all positions in one chunk"). This is the gate
    --     that makes activeness fall again - without it a nest that ever absorbed stays active
    --     for as long as it still holds anything, and it drains at spawn rate: measured after
    --     the cloud left, chunk pollution 0.00 while the spawners still read 234.5 and were
    --     active for another 80 s, 30 of those stalled at 52.5 with units capped.
    --   * what THIS spawner is holding (LuaEntity.absorbed_pollution, spawner only), so a
    --     polluted chunk whose nests are not absorbing is not called active.
    local absorbing = false
    if pollutionAt(surface, chunkX(chunk), chunkY(chunk)) > 0 then
        for i = 1, #spawners do
            local entity = spawners[i]
            if entity.valid and entity.absorbed_pollution > 0 then
                absorbing = true
                break
            end
        end
    end

    if absorbing then
        registerActiveNest(chunk, tick)
    else
        unregisterActiveNest(chunk)
        if (basePheromone > RAIDING_MINIMUM_BASE_THRESHOLD) then
            registerActiveRaidNest(chunk, tick)
        else
            unregisterActiveRaidNest(chunk)
        end
    end
end

function ChunkPropertyUtils.init(universe, mapUtils)
    Universe = universe
    if universe.chunkPropertyUtilsQueries then
        Position = universe.chunkPropertyUtilsQueries.position
    end
end

ChunkPropertyUtilsG = ChunkPropertyUtils
return ChunkPropertyUtils
