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

--

local Constants = require("Constants")
local MathUtils = require("MathUtils")

-- Constants

local DURATION_ACTIVE_NEST = Constants.DURATION_ACTIVE_NEST

local PLAYER_GENERATOR_PERSISTANCE = Constants.PLAYER_GENERATOR_PERSISTANCE
local PLAYER_PHEROMONE_GENERATOR_AMOUNT = Constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT

local COOLDOWN_DRAIN = Constants.COOLDOWN_DRAIN

local RAIDING_MINIMUM_BASE_THRESHOLD = Constants.RAIDING_MINIMUM_BASE_THRESHOLD

local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE

local MOVEMENT_GENERATOR_PERSISTANCE = Constants.MOVEMENT_GENERATOR_PERSISTANCE
local CHUNK_ALL_DIRECTIONS = Constants.CHUNK_ALL_DIRECTIONS

local MAGIC_MAXIMUM_NUMBER = Constants.MAGIC_MAXIMUM_NUMBER

-- imported functions

local activateMap

local euclideanDistancePoints = MathUtils.euclideanDistancePoints
local tableSize = table_size

local mMin = math.min

-- module code

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
    Universe[register][chunkId] = chunk
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
    activateMap(chunk.map)
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
        Universe[register][chunkId] = chunk
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
    base.totalX = base.totalX - chunk.x
    base.totalY = base.totalY - chunk.y
    base.x = base.totalX / base.chunkCount
    base.y = base.totalY / base.chunkCount
    chunk.base = nil
end

function ChunkPropertyUtils.setChunkBase(chunk, base)
    if chunk.base then
        return
    end

    base.chunkCount = base.chunkCount + 1
    base.totalX = base.totalX + chunk.x
    base.totalY = base.totalY + chunk.y
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

function ChunkPropertyUtils.getResourceGenerator(chunk)
    return chunk.resourceGenerator or 0
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

function ChunkPropertyUtils.getPathRating(chunk)
    return chunk.pathRating or 1
end

function ChunkPropertyUtils.setPathRating(chunk, value)
    if (value == 1) then
        chunk.pathRating = nil
    else
        chunk.pathRating = value
    end
end

function ChunkPropertyUtils.getDeathGeneratorRating(chunk)
    return 1 + (chunk.deathGenerator or 0)
end

function ChunkPropertyUtils.getCombinedDeathGeneratorRating(chunk)
    local amount = 1 + ((chunk.deathGenerator or 0) + (chunk.permanentDeathGenerator or 0))
    if (amount > 1) then
        return 1
    elseif (amount < 0) then
        return 0
    else
        return amount
    end
end

function ChunkPropertyUtils.getDeathGenerator(chunk)
    return chunk.deathGenerator or 0
end

function ChunkPropertyUtils.getPermanentDeathGeneratorRating(chunk)
    return 1 + (chunk.permanentDeathGenerator or 0)
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
    local adjustedAmount = (amount * 0.25) + (chunk.permanentDeathGenerator or 0)
    if (adjustedAmount > 0.75) then
        chunk.permanentDeathGenerator = 0.75
    elseif (adjustedAmount < -0.75) then
        chunk.permanentDeathGenerator = -0.75
    else
        chunk.permanentDeathGenerator = adjustedAmount
    end
end

function ChunkPropertyUtils.addDeathGenerator(chunk, value)
    local currentAmount = (chunk.deathGenerator or 0) + value
    if (currentAmount > 1) then
        chunk.deathGenerator = 1
    elseif (currentAmount < -1) then
        chunk.deathGenerator = -1
    else
        chunk.deathGenerator = currentAmount
    end
end

function ChunkPropertyUtils.setDeathGenerator(chunk, value)
    if (value > 1) then
        chunk.deathGenerator = 1
    elseif (value < -1) then
        chunk.deathGenerator = -1
    else
        chunk.deathGenerator = value
    end
end

function ChunkPropertyUtils.addVictoryGenerator(chunk, value)
    local cToV = Universe.chunkToVictory
    local chunkId = chunk.id
    if not cToV[chunkId] then
        cToV[chunkId] = {
            chunk = chunk,
            v = 0
        }
    end
    cToV[chunkId].v = cToV[chunkId].v + value
end

function ChunkPropertyUtils.decayDeathGenerator(chunk)
    local gen = chunk.deathGenerator
    if gen then
        local v = gen * MOVEMENT_GENERATOR_PERSISTANCE
        if (v < 0.0001) and (v > -0.0001) then
            chunk.deathGenerator = nil
        else
            chunk.deathGenerator = v
        end
    end
end

function ChunkPropertyUtils.decayPlayerGenerator(chunk)
    local gen = chunk.playerGenerator
    if gen then
        local v = gen * PLAYER_GENERATOR_PERSISTANCE
        if (v < 0.0001) and (v > -0.0001) then
            chunk.playerGenerator = nil
        else
            chunk.playerGenerator = v
        end
    end
end

function ChunkPropertyUtils.addPlayerGenerator(chunk, playerMaxGenerator)
    local value = chunk.playerGenerator
    if value then
        chunk.playerGenerator = mMin(
            value + PLAYER_PHEROMONE_GENERATOR_AMOUNT,
            playerMaxGenerator
        )
    else
        chunk.playerGenerator = PLAYER_PHEROMONE_GENERATOR_AMOUNT
    end
end

function ChunkPropertyUtils.getPlayerGenerator(chunk)
    return chunk.playerGenerator or 0
end

function ChunkPropertyUtils.getPlayerBaseGenerator(chunk)
    return chunk.playerBaseGenerator or 0
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
        squads[squad.groupNumber] = squad
        squad.chunk = chunk
    end
end

function ChunkPropertyUtils.removeSquadFromChunk(squad)
    local chunk = squad.chunk
    if (chunk ~= -1) then
        local squads = chunk.squads
        if squads then
            squads[squad.groupNumber] = nil
            if (tableSize(squads) == 0) then
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
    local foundBase = chunk.base
    if foundBase then
        return foundBase
    end

    return ChunkPropertyUtils.findNearbyBaseByPosition(
        chunk.map,
        chunk.x,
        chunk.y
    )
end

function ChunkPropertyUtils.findNearbyBaseByPosition(map, x, y)
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

function ChunkPropertyUtils.processNestActiveness(chunk, tick)
    if getActiveTick(chunk) > tick then
        return
    end

    if chunk.nestCount then
        local surface = chunk.map.surface
        if Universe.attackUsePlayer and (chunk[PLAYER_PHEROMONE] > Universe.attackPlayerThreshold) then
            registerActiveNest(chunk, tick)
        elseif (chunk[BASE_PHEROMONE] > 0) then
            local getPollution = surface.get_pollution
            if (getPollution(chunk) > 0) then
                registerActiveNest(chunk, tick)
            else
                local x = chunk.x
                local y = chunk.y
                local pollutionThreshold = Universe.pollutionDiffuseMinimum
                Position.x = x + 32
                Position.y = y
                if (getPollution(Position) > pollutionThreshold) then
                    registerActiveNest(chunk, tick)
                else
                    Position.x = x - 32
                    if (getPollution(Position) > pollutionThreshold) then
                        registerActiveNest(chunk, tick)
                    else
                        Position.x = x
                        Position.y = y - 32
                        if (getPollution(Position) > pollutionThreshold) then
                            registerActiveNest(chunk, tick)
                        else
                            Position.y = y + 32
                            if (getPollution(Position) > pollutionThreshold) then
                                registerActiveNest(chunk, tick)
                            else
                                unregisterActiveNest(chunk)
                                if (chunk[BASE_PHEROMONE] > RAIDING_MINIMUM_BASE_THRESHOLD) then
                                    registerActiveRaidNest(chunk, tick)
                                else
                                    unregisterActiveRaidNest(chunk)
                                end
                            end
                        end
                    end
                end
            end
        else
            unregisterActiveNest(chunk)
            unregisterActiveRaidNest(chunk)
        end
    else
        unregisterActiveNest(chunk)
        unregisterActiveRaidNest(chunk)
    end
end

function ChunkPropertyUtils.init(universe, mapUtils)
    Universe = universe
    activateMap = mapUtils.activateMap
    if universe.chunkPropertyUtilsQueries then
        Position = universe.chunkPropertyUtilsQueries.position
    end
end

ChunkPropertyUtilsG = ChunkPropertyUtils
return ChunkPropertyUtils
