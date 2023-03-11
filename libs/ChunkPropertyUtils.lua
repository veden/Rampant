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

--

local Constants = require("Constants")
local MathUtils = require("MathUtils")

-- Constants

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

local manhattenDistancePoints = MathUtils.manhattenDistancePoints
local tableSize = table_size

local mMin = math.min

-- module code

function ChunkPropertyUtils.getTurretCount(map, chunk)
    return map.chunkToTurrets[chunk.id] or 0
end

function ChunkPropertyUtils.getTrapCount(map, chunk)
    return map.chunkToTraps[chunk.id] or 0
end

function ChunkPropertyUtils.getUtilityCount(map, chunk)
    return map.chunkToUtilities[chunk.id] or 0
end

function ChunkPropertyUtils.getHiveCount(map, chunk)
    return map.chunkToHives[chunk.id] or 0
end

function ChunkPropertyUtils.addTurretCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToTurretIds[chunk.id] then
        map.chunkToTurretIds[chunk.id] = {}
    end
    if not map.chunkToTurretIds[chunk.id][unitNumber] then
        map.chunkToTurretIds[chunk.id][unitNumber] = true
        map.chunkToTurrets[chunk.id] = (map.chunkToTurrets[chunk.id] or 0) + 1
        return true
    end
    return false
end

function ChunkPropertyUtils.removeTurretCount(map, chunk, unitNumber)
    if map.chunkToTurretIds[chunk.id] and map.chunkToTurretIds[chunk.id][unitNumber] then
        map.chunkToTurretIds[chunk.id][unitNumber] = nil
        map.chunkToTurrets[chunk.id] = map.chunkToTurrets[chunk.id] - 1
        if map.chunkToTurrets[chunk.id] == 0 then
            map.chunkToTurretIds[chunk.id] = nil
            map.chunkToTurrets[chunk.id] = nil
        end
        return true
    end
    return false
end

function ChunkPropertyUtils.addTrapCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToTrapIds[chunk.id] then
        map.chunkToTrapIds[chunk.id] = {}
    end
    if not map.chunkToTrapIds[chunk.id][unitNumber] then
        map.chunkToTrapIds[chunk.id][unitNumber] = true
        map.chunkToTraps[chunk.id] = (map.chunkToTraps[chunk.id] or 0) + 1
        return true
    end
    return false
end

function ChunkPropertyUtils.removeTrapCount(map, chunk, unitNumber)
    if map.chunkToTrapIds[chunk.id] and map.chunkToTrapIds[chunk.id][unitNumber] then
        map.chunkToTrapIds[chunk.id][unitNumber] = nil
        map.chunkToTraps[chunk.id] = map.chunkToTraps[chunk.id] - 1
        if map.chunkToTraps[chunk.id] == 0 then
            map.chunkToTrapIds[chunk.id] = nil
            map.chunkToTraps[chunk.id] = nil
        end
        return true
    end
    return false
end

function ChunkPropertyUtils.addUtilitiesCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToUtilityIds[chunk.id] then
        map.chunkToUtilityIds[chunk.id] = {}
    end
    if not map.chunkToUtilityIds[chunk.id][unitNumber] then
        map.chunkToUtilityIds[chunk.id][unitNumber] = true
        map.chunkToUtilities[chunk.id] = (map.chunkToUtilities[chunk.id] or 0) + 1
        return true
    end
    return false
end

function ChunkPropertyUtils.removeUtilitiesCount(map, chunk, unitNumber)
    if map.chunkToUtilityIds[chunk.id] and map.chunkToUtilityIds[chunk.id][unitNumber] then
        map.chunkToUtilityIds[chunk.id][unitNumber] = nil
        map.chunkToUtilities[chunk.id] = map.chunkToUtilities[chunk.id] - 1
        if map.chunkToUtilities[chunk.id] == 0 then
            map.chunkToUtilityIds[chunk.id] = nil
            map.chunkToUtilities[chunk.id] = nil
        end
        return true
    end
    return false
end

function ChunkPropertyUtils.addHiveCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToHiveIds[chunk.id] then
        map.chunkToHiveIds[chunk.id] = {}
    end
    if not map.chunkToHiveIds[chunk.id][unitNumber] then
        map.chunkToHiveIds[chunk.id][unitNumber] = true
        map.chunkToHives[chunk.id] = (map.chunkToHives[chunk.id] or 0) + 1
        return true
    end
    return false
end

function ChunkPropertyUtils.removeHiveCount(map, chunk, unitNumber)
    if map.chunkToHiveIds[chunk.id] and map.chunkToHiveIds[chunk.id][unitNumber] then
        map.chunkToHiveIds[chunk.id][unitNumber] = nil
        map.chunkToHives[chunk.id] = map.chunkToHives[chunk.id] - 1
        if map.chunkToHives[chunk.id] == 0 then
            map.chunkToHiveIds[chunk.id] = nil
            map.chunkToHives[chunk.id] = nil
        end
        return true
    end
    return false
end

function ChunkPropertyUtils.addNestCount(map, chunk, unitNumber)
    map.activeSurface = true
    local chunkId = chunk.id
    if not map.chunkToNestIds[chunkId] then
        map.chunkToNestIds[chunkId] = {}
    end
    if not map.chunkToNestIds[chunkId][unitNumber] then
        map.chunkToNestIds[chunkId][unitNumber] = true
        local cToN = Universe.chunkToNests
        local pack = cToN[chunkId]
        if not pack then
            cToN[chunkId] = {
                map = map,
                v = 0
            }
        end
        cToN[chunkId].v = cToN[chunkId].v + 1
        return true
    end
    return false
end

function ChunkPropertyUtils.removeNestCount(map, chunk, unitNumber)
    local chunkId = chunk.id
    if map.chunkToNestIds[chunkId] and map.chunkToNestIds[chunkId][unitNumber] then
        map.chunkToNestIds[chunkId][unitNumber] = nil
        local cToN = Universe.chunkToNests
        cToN[chunkId].v = cToN[chunkId].v - 1
        if cToN[chunkId].v == 0 then
            map.chunkToNestIds[chunkId] = nil
            cToN[chunkId] = nil
            if (Universe.processMigrationIterator == chunkId) then
                Universe.processMigrationIterator = nil
            end
            if (Universe.processNestIterator == chunkId) then
                Universe.processNestIterator = nil
            end
        end
        return true
    end
    return false
end

function ChunkPropertyUtils.getNestCount(chunk)
    local nestPack = Universe.chunkToNests[chunk.id]
    if not nestPack then
        return 0
    end
    return nestPack.v
end

function ChunkPropertyUtils.addBaseResourceChunk(base, chunk)
    if ChunkPropertyUtils.getResourceGenerator(base.map, chunk) > 0 then
        base.resourceChunkCount = base.resourceChunkCount + 1
        base.resourceChunks[chunk.id] = true
    end
end

function ChunkPropertyUtils.removeBaseResourceChunk(base, chunk)
    if base.resourceChunks[chunk.id] then
        base.resourceChunkCount = base.resourceChunkCount - 1
        base.resourceChunks[chunk.id] = nil
    end
end

function ChunkPropertyUtils.getChunkBase(map, chunk)
    return map.chunkToBase[chunk.id]
end

function ChunkPropertyUtils.removeChunkBase(map, chunk, base)
    if map.chunkToBase[chunk.id] then
        base.chunkCount = base.chunkCount - 1
        map.chunkToBase[chunk.id] = nil
    end
end

function ChunkPropertyUtils.setChunkBase(map, chunk, base)
    if not map.chunkToBase[chunk.id] then
        base.chunkCount = base.chunkCount + 1
        map.chunkToBase[chunk.id] = base
    end
end

function ChunkPropertyUtils.getEnemyStructureCount(map, chunk)
    local nests = 0
    local nestPack = Universe.chunkToNests[chunk.id]
    if nestPack then
        nests = nestPack.v
    end
    return nests + (map.chunkToTurrets[chunk.id] or 0) + (map.chunkToTraps[chunk.id] or 0) +
        (map.chunkToUtilities[chunk.id] or 0) + (map.chunkToHives[chunk.id] or 0)
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
    local pack = Universe.chunkToDrained[chunk.id]
    if not pack then
        return false
    end
    return (tick - pack.tick) < COOLDOWN_DRAIN
end

function ChunkPropertyUtils.setDrainedTick(map, chunk, tick)
    local chunkId = chunk.id
    local pack = Universe.chunkToDrained[chunkId]
    if not pack then
        pack = {
            map = map,
            tick = 0
        }
        Universe.chunkToDrained[chunkId] = pack
    end
    pack.tick = tick
end

function ChunkPropertyUtils.getRetreatTick(chunk)
    local pack = Universe.chunkToRetreats[chunk.id]
    if not pack then
        return 0
    end
    return pack.tick
end

function ChunkPropertyUtils.getRallyTick(chunk)
    local pack = Universe.chunkToRallys[chunk.id]
    if not pack then
        return 0
    end
    return pack.tick
end

function ChunkPropertyUtils.setRallyTick(map, chunk, tick)
    local chunkId = chunk.id
    local pack = Universe.chunkToRallys[chunkId]
    if not pack then
        pack = {
            map = map,
            tick = tick
        }
        Universe.chunkToRallys[chunkId] = pack
    end
    pack.tick = tick
end

function ChunkPropertyUtils.setRetreatTick(map, chunk, tick)
    local chunkId = chunk.id
    local pack = Universe.chunkToRetreats[chunkId]
    if not pack then
        pack = {
            map = map,
            tick = tick
        }
        Universe.chunkToRetreats[chunkId] = pack
    end
    pack.tick = tick
end

function ChunkPropertyUtils.setResourceGenerator(map, chunk, resourceGenerator)
    if (resourceGenerator <= 0) then
        map.chunkToResource[chunk.id] = nil
    else
        map.chunkToResource[chunk.id] = resourceGenerator
    end
end

function ChunkPropertyUtils.getResourceGenerator(map, chunk)
    return map.chunkToResource[chunk.id] or 0
end

function ChunkPropertyUtils.addResourceGenerator(map, chunk, delta)
    map.chunkToResource[chunk.id] = (map.chunkToResource[chunk.id] or 0) + delta
end

function ChunkPropertyUtils.getPassable(map, chunk)
    return map.chunkToPassable[chunk.id] or CHUNK_ALL_DIRECTIONS
end


function ChunkPropertyUtils.getRaidNestActiveness(chunk)
    local activeness = Universe.chunkToActiveRaidNest[chunk.id]
    if not activeness then
        return 0
    end
    return activeness.v or 0
end

function ChunkPropertyUtils.setRaidNestActiveness(map, chunk, value, base)
    if (value <= 0) then
        if Universe.chunkToActiveRaidNest[chunk.id] then
            base.activeRaidNests = base.activeRaidNests - 1
        end
        if (Universe.processActiveRaidSpawnerIterator == chunk.id) then
            Universe.processActiveRaidSpawnerIterator = nil
        end
        Universe.chunkToActiveRaidNest[chunk.id] = nil
    else
        if not Universe.chunkToActiveRaidNest[chunk.id] then
            base.activeRaidNests = base.activeRaidNests + 1
            Universe.chunkToActiveRaidNest[chunk.id] = {
                map = map,
                v = 0
            }
        end
        Universe.chunkToActiveRaidNest[chunk.id].v = value
    end
end

function ChunkPropertyUtils.getNestActiveness(chunk)
    local activeness = Universe.chunkToActiveNest[chunk.id]
    if not activeness then
        return 0
    end
    return activeness.v or 0
end

function ChunkPropertyUtils.setNestActiveness(map, chunk, value, base)
    if (value <= 0) then
        if Universe.chunkToActiveNest[chunk.id] then
            base.activeNests = base.activeNests - 1
        end
        if (Universe.processActiveSpawnerIterator == chunk.id) then
            Universe.processActiveSpawnerIterator = nil
        end
        Universe.chunkToActiveNest[chunk.id] = nil
    else
        if not Universe.chunkToActiveNest[chunk.id] then
            base.activeNests = base.activeNests + 1
            Universe.chunkToActiveNest[chunk.id] = {
                map = map,
                v = 0
            }
        end
        Universe.chunkToActiveNest[chunk.id].v = value
    end
end

function ChunkPropertyUtils.setPassable(map, chunk, value)
    if (value == CHUNK_ALL_DIRECTIONS) then
        map.chunkToPassable[chunk.id] = nil
    else
        map.chunkToPassable[chunk.id] = value
    end
end

function ChunkPropertyUtils.getPathRating(map, chunk)
    return map.chunkToPathRating[chunk.id] or 1
end

function ChunkPropertyUtils.setPathRating(map, chunk, value)
    if (value == 1) then
        map.chunkToPathRating[chunk.id] = nil
    else
        map.chunkToPathRating[chunk.id] = value
    end
end

function ChunkPropertyUtils.getDeathGeneratorRating(map, chunk)
    return 1 + (map.chunkToDeathGenerator[chunk.id] or 0)
end

function ChunkPropertyUtils.getCombinedDeathGeneratorRating(map, chunk)
    local amount = 1 + ((map.chunkToDeathGenerator[chunk.id] or 0) + (map.chunkToPermanentDeathGenerator[chunk.id] or 0))
    if (amount > 1) then
        return 1
    elseif (amount < 0) then
        return 0
    else
        return amount
    end
end

function ChunkPropertyUtils.getDeathGenerator(map, chunk)
    return map.chunkToDeathGenerator[chunk.id] or 0
end

function ChunkPropertyUtils.getPermanentDeathGeneratorRating(map, chunk)
    return 1 + (map.chunkToPermanentDeathGenerator[chunk.id] or 0)
end

function ChunkPropertyUtils.getCombinedDeathGenerator(map, chunk)
    local amount = (map.chunkToDeathGenerator[chunk.id] or 0) + (map.chunkToPermanentDeathGenerator[chunk.id] or 0)
    if (amount > 1) then
        return 1
    elseif (amount < -1) then
        return -1
    else
        return amount
    end
end

function ChunkPropertyUtils.addPermanentDeathGenerator(map, chunk, amount)
    local adjustedAmount = (amount * 0.25) + (map.chunkToPermanentDeathGenerator[chunk.id] or 0)
    if (adjustedAmount > 0.75) then
        map.chunkToPermanentDeathGenerator[chunk.id] = 0.75
    elseif (adjustedAmount < -0.75) then
        map.chunkToPermanentDeathGenerator[chunk.id] = -0.75
    else
        map.chunkToPermanentDeathGenerator[chunk.id] = adjustedAmount
    end
end

function ChunkPropertyUtils.addDeathGenerator(map, chunk, value)
    local currentAmount = (map.chunkToDeathGenerator[chunk.id] or 0) + value
    if (currentAmount > 1) then
        map.chunkToDeathGenerator[chunk.id] = 1
    elseif (currentAmount < -1) then
        map.chunkToDeathGenerator[chunk.id] = -1
    else
        map.chunkToDeathGenerator[chunk.id] = currentAmount
    end
end

function ChunkPropertyUtils.setDeathGenerator(map, chunk, value)
    if (value > 1) then
        map.chunkToDeathGenerator[chunk.id] = 1
    elseif (value < -1) then
        map.chunkToDeathGenerator[chunk.id] = -1
    else
        map.chunkToDeathGenerator[chunk.id] = value
    end
end

function ChunkPropertyUtils.addVictoryGenerator(map, chunk, value)
    local cToV = Universe.chunkToVictory
    local chunkId = chunk.id
    if not cToV[chunkId] then
        cToV[chunkId] = {
            map = map,
            v = 0
        }
    end
    cToV[chunkId].v = cToV[chunkId].v + value
end

function ChunkPropertyUtils.decayDeathGenerator(map, chunk)
    local gen = map.chunkToDeathGenerator[chunk.id]
    if gen then
        local v = gen * MOVEMENT_GENERATOR_PERSISTANCE
        if (v < 0.0001) and (v > -0.0001) then
            map.chunkToDeathGenerator[chunk.id] = nil
        else
            map.chunkToDeathGenerator[chunk.id] = v
        end
    end
end

function ChunkPropertyUtils.decayPlayerGenerator(map, chunk)
    local gen = map.chunkToPlayerGenerator[chunk.id]
    if gen then
        local v = gen * PLAYER_GENERATOR_PERSISTANCE
        if (v < 0.0001) and (v > -0.0001) then
            map.chunkToPlayerGenerator[chunk.id] = nil
        else
            map.chunkToPlayerGenerator[chunk.id] = v
        end
    end
end

function ChunkPropertyUtils.addPlayerGenerator(map, chunk, playerMaxGenerator)
    local value = map.chunkToPlayerGenerator[chunk.id]
    if value then
        map.chunkToPlayerGenerator[chunk.id] = mMin(
            value + PLAYER_PHEROMONE_GENERATOR_AMOUNT,
            playerMaxGenerator
        )
    else
        map.chunkToPlayerGenerator[chunk.id] = PLAYER_PHEROMONE_GENERATOR_AMOUNT
    end
end

function ChunkPropertyUtils.getPlayerGenerator(map, chunk)
    return map.chunkToPlayerGenerator[chunk.id] or 0
end

function ChunkPropertyUtils.getPlayerBaseGenerator(map, chunk)
    return map.chunkToPlayerBase[chunk.id] or 0
end

function ChunkPropertyUtils.addSquadToChunk(map, chunk, squad)
    local chunkToSquad = map.chunkToSquad

    if (chunk ~= -1) and ((squad.chunk == -1) or (squad.chunk.id ~= chunk.id)) then
        ChunkPropertyUtils.removeSquadFromChunk(map, squad)
        local squads = chunkToSquad[chunk.id]
        if not squads then
            squads = {}
            chunkToSquad[chunk.id] = squads
        end
        squads[squad.groupNumber] = squad
        squad.chunk = chunk
    end
end

function ChunkPropertyUtils.removeSquadFromChunk(map, squad)
    local chunkToSquad = map.chunkToSquad
    local chunk = squad.chunk
    if (chunk ~= -1) then
        local squads = chunkToSquad[chunk.id]
        if squads then
            squads[squad.groupNumber] = nil
            if (tableSize(squads) == 0) then
                chunkToSquad[chunk.id] = nil
            end
        end
    end
end

function ChunkPropertyUtils.getSquadsOnChunk(map, chunk)
    return map.chunkToSquad[chunk.id] or map.emptySquadsOnChunk
end

function ChunkPropertyUtils.setPlayerBaseGenerator(map, chunk, playerGenerator)
    if (playerGenerator <= 0) then
        map.chunkToPlayerBase[chunk.id] = nil
        return 0
    else
        map.chunkToPlayerBase[chunk.id] = playerGenerator
        return playerGenerator
    end
end

function ChunkPropertyUtils.addPlayerBaseGenerator(map, chunk, playerGenerator)
    local amount = (map.chunkToPlayerBase[chunk.id] or 0) + playerGenerator
    if amount <= 0 then
        map.chunkToPlayerBase[chunk.id] = nil
        return 0
    else
        map.chunkToPlayerBase[chunk.id] = amount
        return amount
    end
end

function ChunkPropertyUtils.findNearbyBase(map, chunk)
    local x = chunk.x
    local y = chunk.y

    local foundBase = ChunkPropertyUtils.getChunkBase(map, chunk)
    if foundBase then
        return foundBase
    end

    local closest = MAGIC_MAXIMUM_NUMBER
    for _, base in pairs(map.bases) do
        local distance = manhattenDistancePoints(base.x, base.y, x, y)
        if (distance <= base.distanceThreshold) and (distance < closest) then
            closest = distance
            foundBase = base
        end
    end

    return foundBase
end

function ChunkPropertyUtils.findNearbyBaseByPosition(map, x, y)
    local foundBase

    local closest = MAGIC_MAXIMUM_NUMBER
    for _, base in pairs(map.bases) do
        local distance = manhattenDistancePoints(base.x, base.y, x, y)
        if (distance <= base.distanceThreshold) and (distance < closest) then
            closest = distance
            foundBase = base
        end
    end

    return foundBase
end

function ChunkPropertyUtils.processNestActiveness(map, chunk)
    local nests = ChunkPropertyUtils.getNestCount(chunk)
    local base = ChunkPropertyUtils.findNearbyBase(map, chunk)
    if (nests > 0) then
        local surface = map.surface
        local activeness = ChunkPropertyUtils.getNestActiveness(chunk)
        local raidActiveness = ChunkPropertyUtils.getRaidNestActiveness(chunk)
        if Universe.attackUsePlayer and (chunk[PLAYER_PHEROMONE] > Universe.attackPlayerThreshold) then
            ChunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20), base)
        elseif (chunk[BASE_PHEROMONE] > 0) then
            if (surface.get_pollution(chunk) > 0) then
                ChunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20), base)
            else
                local x = chunk.x
                local y = chunk.y
                local position = {x=0,y=0}
                local pollutionThreshold = Universe.pollutionDiffuseMinimum
                position.x = x + 32
                position.y = y
                if (surface.get_pollution(position) > pollutionThreshold) then
                    ChunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20), base)
                else
                    position.x = x - 32
                    if (surface.get_pollution(position) > pollutionThreshold) then
                        ChunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20), base)
                    else
                        position.x = x
                        position.y = y - 32
                        if (surface.get_pollution(position) > pollutionThreshold) then
                            ChunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20), base)
                        else
                            position.y = y + 32
                            if (surface.get_pollution(position) > pollutionThreshold) then
                                ChunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20), base)
                            else
                                ChunkPropertyUtils.setNestActiveness(map, chunk, activeness - 2, base)
                                if (chunk[BASE_PHEROMONE] > RAIDING_MINIMUM_BASE_THRESHOLD) then
                                    ChunkPropertyUtils.setRaidNestActiveness(map, chunk, mMin(raidActiveness + 3, 20), base)
                                else
                                    ChunkPropertyUtils.setRaidNestActiveness(map, chunk, raidActiveness - 1, base)
                                end
                            end
                        end
                    end
                end
            end
        else
            ChunkPropertyUtils.setNestActiveness(map, chunk, activeness - 5, base)
            ChunkPropertyUtils.setRaidNestActiveness(map, chunk, raidActiveness - 5, base)
        end
    elseif base then
        ChunkPropertyUtils.setNestActiveness(map, chunk, 0, base)
        ChunkPropertyUtils.setRaidNestActiveness(map, chunk, 0, base)
    end
end

function ChunkPropertyUtils.init(universe)
    Universe = universe
end

ChunkPropertyUtilsG = ChunkPropertyUtils
return ChunkPropertyUtils
