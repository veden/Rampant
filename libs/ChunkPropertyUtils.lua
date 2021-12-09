if chunkPropertyUtilsG then
    return chunkPropertyUtilsG
end
local chunkPropertyUtils = {}

local constants = require("Constants")

-- constants

local COOLDOWN_DRAIN = constants.COOLDOWN_DRAIN

local RAIDING_MINIMUM_BASE_THRESHOLD = constants.RAIDING_MINIMUM_BASE_THRESHOLD

local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local MOVEMENT_GENERATOR_PERSISTANCE = constants.MOVEMENT_GENERATOR_PERSISTANCE
local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

-- imported functions

local mMin = math.min

-- module code

function chunkPropertyUtils.getTurretCount(map, chunk)
    return map.chunkToTurrets[chunk.id] or 0
end

function chunkPropertyUtils.getTrapCount(map, chunk)
    return map.chunkToTraps[chunk.id] or 0
end

function chunkPropertyUtils.getUtilityCount(map, chunk)
    return map.chunkToUtilities[chunk.id] or 0
end

function chunkPropertyUtils.getHiveCount(map, chunk)
    return map.chunkToHives[chunk.id] or 0
end

function chunkPropertyUtils.addTurretCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToTurretIds[chunk.id] then
        map.chunkToTurretIds[chunk.id] = {}
    end
    if not map.chunkToTurretIds[chunk.id][unitNumber] then
        map.chunkToTurretIds[chunk.id][unitNumber] = true
        map.chunkToTurrets[chunk.id] = (map.chunkToTurrets[chunk.id] or 0) + 1
    end
end

function chunkPropertyUtils.removeTurretCount(map, chunk, unitNumber)
    if map.chunkToTurretIds[chunk.id] and map.chunkToTurretIds[chunk.id][unitNumber] then
        map.chunkToTurretIds[chunk.id][unitNumber] = nil
        map.chunkToTurrets[chunk.id] = map.chunkToTurrets[chunk.id] - 1
        if map.chunkToTurrets[chunk.id] == 0 then
            map.chunkToTurretIds[chunk.id] = nil
            map.chunkToTurrets[chunk.id] = nil
        end
    end
end

function chunkPropertyUtils.addTrapCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToTrapIds[chunk.id] then
        map.chunkToTrapIds[chunk.id] = {}
    end
    if not map.chunkToTrapIds[chunk.id][unitNumber] then
        map.chunkToTrapIds[chunk.id][unitNumber] = true
        map.chunkToTraps[chunk.id] = (map.chunkToTraps[chunk.id] or 0) + 1
    end
end

function chunkPropertyUtils.removeTrapCount(map, chunk, unitNumber)
    if map.chunkToTrapIds[chunk.id] and map.chunkToTrapIds[chunk.id][unitNumber] then
        map.chunkToTrapIds[chunk.id][unitNumber] = nil
        map.chunkToTraps[chunk.id] = map.chunkToTraps[chunk.id] - 1
        if map.chunkToTraps[chunk.id] == 0 then
            map.chunkToTrapIds[chunk.id] = nil
            map.chunkToTraps[chunk.id] = nil
        end
    end
end

function chunkPropertyUtils.addUtilitiesCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToUtilityIds[chunk.id] then
        map.chunkToUtilityIds[chunk.id] = {}
    end
    if not map.chunkToUtilityIds[chunk.id][unitNumber] then
        map.chunkToUtilityIds[chunk.id][unitNumber] = true
        map.chunkToUtilities[chunk.id] = (map.chunkToUtilities[chunk.id] or 0) + 1
    end
end

function chunkPropertyUtils.removeUtilitiesCount(map, chunk, unitNumber)
    if map.chunkToUtilityIds[chunk.id] and map.chunkToUtilityIds[chunk.id][unitNumber] then
        map.chunkToUtilityIds[chunk.id][unitNumber] = nil
        map.chunkToUtilities[chunk.id] = map.chunkToUtilities[chunk.id] - 1
        if map.chunkToUtilities[chunk.id] == 0 then
            map.chunkToUtilityIds[chunk.id] = nil
            map.chunkToUtilities[chunk.id] = nil
        end
    end
end

function chunkPropertyUtils.addHiveCount(map, chunk, unitNumber)
    map.activeSurface = true
    if not map.chunkToHiveIds[chunk.id] then
        map.chunkToHiveIds[chunk.id] = {}
    end
    if not map.chunkToHiveIds[chunk.id][unitNumber] then
        map.chunkToHiveIds[chunk.id][unitNumber] = true
        map.chunkToHives[chunk.id] = (map.chunkToHives[chunk.id] or 0) + 1
    end
end

function chunkPropertyUtils.removeHiveCount(map, chunk, unitNumber)
    if map.chunkToHiveIds[chunk.id] and map.chunkToHiveIds[chunk.id][unitNumber] then
        map.chunkToHiveIds[chunk.id][unitNumber] = nil
        map.chunkToHives[chunk.id] = map.chunkToHives[chunk.id] - 1
        if map.chunkToHives[chunk.id] == 0 then
            map.chunkToHiveIds[chunk.id] = nil
            map.chunkToHives[chunk.id] = nil
        end
    end
end

function chunkPropertyUtils.addNestCount(map, chunk, unitNumber)
    map.activeSurface = true
    local chunkId = chunk.id
    if not map.chunkToNestIds[chunkId] then
        map.chunkToNestIds[chunkId] = {}
    end
    if not map.chunkToNestIds[chunkId][unitNumber] then
        map.chunkToNestIds[chunkId][unitNumber] = true
        local cToN = map.universe.chunkToNests
        local pack = cToN[chunkId]
        if not pack then
            cToN[chunkId] = {
                map = map,
                v = 0
            }
        end
        cToN[chunkId].v = cToN[chunkId].v + 1
    end
end

function chunkPropertyUtils.removeNestCount(map, chunk, unitNumber)
    local chunkId = chunk.id
    if map.chunkToNestIds[chunkId] and map.chunkToNestIds[chunkId][unitNumber] then
        map.chunkToNestIds[chunkId][unitNumber] = nil
        local cToN = map.universe.chunkToNests
        cToN[chunkId].v = cToN[chunkId].v - 1
        if cToN[chunkId].v == 0 then
            map.chunkToNestIds[chunkId] = nil
            cToN[chunkId] = nil
            if (map.universe.processMigrationIterator == chunkId) then
                map.universe.processMigrationIterator = nil
            end
            if (map.universe.processNestIterator == chunkId) then
                map.universe.processNestIterator = nil
            end
        end
    end
end

function chunkPropertyUtils.getNestCount(map, chunk)
    local nestPack = map.universe.chunkToNests[chunk.id]
    if not nestPack then
        return 0
    end
    return nestPack.v
end

function chunkPropertyUtils.getChunkBase(map, chunk)
    return map.chunkToBase[chunk.id]
end

function chunkPropertyUtils.removeChunkBase(map, chunk, base)
    if map.chunkToBase[chunk.id] then
        base.chunkCount = base.chunkCount - 1
        map.chunkToBase[chunk.id] = nil
    end
end

function chunkPropertyUtils.setChunkBase(map, chunk, base)
    if not map.chunkToBase[chunk.id] then
        base.chunkCount = base.chunkCount + 1
        map.chunkToBase[chunk.id] = base
    end
end

function chunkPropertyUtils.getEnemyStructureCount(map, chunk)
    local nests = 0
    local nestPack = map.universe.chunkToNests[chunk.id]
    if nestPack then
        nests = nestPack.v
    end
    return nests + (map.chunkToTurrets[chunk.id] or 0) + (map.chunkToTraps[chunk.id] or 0) +
        (map.chunkToUtilities[chunk.id] or 0) + (map.chunkToHives[chunk.id] or 0)
end

function chunkPropertyUtils.setDrainPylons(map, entity1, entity2)
    local pair = {entity1, entity2}
    map.drainPylons[entity1.unit_number] = pair
    map.drainPylons[entity2.unit_number] = pair
end

function chunkPropertyUtils.removeDrainPylons(map, unitNumber)
    local pair = map.drainPylons[unitNumber]
    if pair then
        local target = pair[1]
        local pole = pair[2]
        if target.unit_number == unitNumber then
            map.drainPylons[unitNumber] = nil
            if pole.valid then
                map.drainPylons[pole.unit_number] = nil
                pole.die()
            end
        elseif (pole.unit_number == unitNumber) then
            map.drainPylons[unitNumber] = nil
            if target.valid then
                map.drainPylons[target.unit_number] = nil
                target.destroy()
            end
        end
    end
end

function chunkPropertyUtils.getDrainPylonPair(map, unitNumber)
    return map.drainPylons[unitNumber]
end

function chunkPropertyUtils.isDrained(map, chunk, tick)
    local pack = map.universe.chunkToDrained[chunk.id]
    if not pack then
        return false
    end
    return (tick - pack.tick) < COOLDOWN_DRAIN
end

function chunkPropertyUtils.setDrainedTick(map, chunk, tick)
    local chunkId = chunk.id
    local pack = map.universe.chunkToDrained[chunkId]
    if not pack then
        pack = {
            map = map,
            tick = 0
        }
        map.universe.chunkToDrained[chunkId] = pack
    end
    pack.tick = tick
end

function chunkPropertyUtils.getRetreatTick(map, chunk)
    local pack = map.universe.chunkToRetreats[chunk.id]
    if not pack then
        return 0
    end
    return pack.tick
end

function chunkPropertyUtils.getRallyTick(map, chunk)
    local pack = map.universe.chunkToRallys[chunk.id]
    if not pack then
        return 0
    end
    return pack.tick
end

function chunkPropertyUtils.setRallyTick(map, chunk, tick)
    local chunkId = chunk.id
    local pack = map.universe.chunkToRallys[chunkId]
    if not pack then
        pack = {
            map = map,
            tick = tick
        }
        map.universe.chunkToRallys[chunkId] = pack
    end
    pack.tick = tick
end

function chunkPropertyUtils.setRetreatTick(map, chunk, tick)
    local chunkId = chunk.id
    local pack = map.universe.chunkToRetreats[chunkId]
    if not pack then
        pack = {
            map = map,
            tick = tick
        }
        map.universe.chunkToRetreats[chunkId] = pack
    end
    pack.tick = tick
end

function chunkPropertyUtils.setResourceGenerator(map, chunk, resourceGenerator)
    if (resourceGenerator <= 0) then
        map.chunkToResource[chunk.id] = nil
    else
        map.chunkToResource[chunk.id] = resourceGenerator
    end
end

function chunkPropertyUtils.getResourceGenerator(map, chunk)
    return map.chunkToResource[chunk.id] or 0
end

function chunkPropertyUtils.addResourceGenerator(map, chunk, delta)
    map.chunkToResource[chunk.id] = (map.chunkToResource[chunk.id] or 0) + delta
end

function chunkPropertyUtils.getDeathGenerator(map, chunk)
    return map.chunkToDeathGenerator[chunk.id] or 0
end

function chunkPropertyUtils.getPassable(map, chunk)
    return map.chunkToPassable[chunk.id] or CHUNK_ALL_DIRECTIONS
end


function chunkPropertyUtils.getRaidNestActiveness(map, chunk)
    local activeness = map.universe.chunkToActiveRaidNest[chunk.id]
    if not activeness then
        return 0
    end
    return activeness.v or 0
end

function chunkPropertyUtils.setRaidNestActiveness(map, chunk, value)
    local universe = map.universe
    if (value <= 0) then
        if universe.chunkToActiveRaidNest[chunk.id] then
            map.activeRaidNests = map.activeRaidNests - 1
        end
        if (universe.processActiveRaidSpawnerIterator == chunk.id) then
            universe.processActiveRaidSpawnerIterator = nil
        end
        universe.chunkToActiveRaidNest[chunk.id] = nil
    else
        if not universe.chunkToActiveRaidNest[chunk.id] then
            map.activeRaidNests = map.activeRaidNests + 1
            universe.chunkToActiveRaidNest[chunk.id] = {
                map = map,
                v = 0
            }
        end
        universe.chunkToActiveRaidNest[chunk.id].v = value
    end
end

function chunkPropertyUtils.getNestActiveness(map, chunk)
    local activeness = map.universe.chunkToActiveNest[chunk.id]
    if not activeness then
        return 0
    end
    return activeness.v or 0
end

function chunkPropertyUtils.setNestActiveness(map, chunk, value)
    local universe = map.universe
    if (value <= 0) then
        if universe.chunkToActiveNest[chunk.id] then
            map.activeNests = map.activeNests - 1
        end
        if (universe.processActiveSpawnerIterator == chunk.id) then
            universe.processActiveSpawnerIterator = nil
        end
        universe.chunkToActiveNest[chunk.id] = nil
    else
        if not universe.chunkToActiveNest[chunk.id] then
            map.activeNests = map.activeNests + 1
            universe.chunkToActiveNest[chunk.id] = {
                map = map,
                v = 0
            }
        end
        universe.chunkToActiveNest[chunk.id].v = value
    end
end

function chunkPropertyUtils.setPassable(map, chunk, value)
    if (value == CHUNK_ALL_DIRECTIONS) then
        map.chunkToPassable[chunk.id] = nil
    else
        map.chunkToPassable[chunk.id] = value
    end
end

function chunkPropertyUtils.getPathRating(map, chunk)
    return map.chunkToPathRating[chunk.id] or 1
end

function chunkPropertyUtils.setPathRating(map, chunk, value)
    if (value == 1) then
        map.chunkToPathRating[chunk.id] = nil
    else
        map.chunkToPathRating[chunk.id] = value
    end
end

function chunkPropertyUtils.addDeathGenerator(map, chunk, value)
    map.chunkToDeathGenerator[chunk.id] = (map.chunkToDeathGenerator[chunk.id] or 0) + value
end

function chunkPropertyUtils.addVictoryGenerator(map, chunk, value)
    local cToV = map.universe.chunkToVictory
    local chunkId = chunk.id
    if not cToV[chunkId] then
        cToV[chunkId] = {
            map = map,
            v = 0
        }
    end
    cToV[chunkId].v = cToV[chunkId].v + value
end

function chunkPropertyUtils.decayDeathGenerator(map, chunk)
    local gen = map.chunkToDeathGenerator[chunk.id]
    if gen then
        gen = gen * MOVEMENT_GENERATOR_PERSISTANCE

        if (gen >= -2) and (gen <= 2) then
            map.chunkToDeathGenerator[chunk.id] = nil
        else
            map.chunkToDeathGenerator[chunk.id] = gen
        end
    end
end

function chunkPropertyUtils.addPlayerToChunk(map, chunk, name)
    local playerChunks = map.playerToChunk
    local playerCountChunks = map.chunkToPlayerCount
    local playerChunk = playerChunks[name]
    if not playerChunk then
        playerChunks[name] = chunk
        local playerCount = playerCountChunks[chunk.id]
        if not playerCount then
            playerCountChunks[chunk.id] = 1
        else
            playerCountChunks[chunk.id] = playerCount + 1
        end
    elseif (playerChunk.id ~= chunk.id) then
        playerChunks[name] = chunk
        local playerCount = playerCountChunks[playerChunk.id]
        chunkPropertyUtils.setPlayersOnChunk(map, playerChunk, playerCount - 1)
        playerCount = playerCountChunks[chunk.id]
        if not playerCount then
            playerCountChunks[chunk.id] = 1
        else
            playerCountChunks[chunk.id] = playerCount + 1
        end
    end
end

function chunkPropertyUtils.setPlayersOnChunk(map, chunk, value)
    if (value <= 0) then
        map.chunkToPlayerCount[chunk.id] = nil
    else
        map.chunkToPlayerCount[chunk.id] = value
    end
end

function chunkPropertyUtils.getPlayersOnChunk(map, chunk)
    return map.chunkToPlayerCount[chunk.id] or 0
end

function chunkPropertyUtils.getPlayerBaseGenerator(map, chunk)
    return map.chunkToPlayerBase[chunk.id] or 0
end

function chunkPropertyUtils.addSquadToChunk(map, chunk, squad)
    local chunkToSquad = map.chunkToSquad

    if (chunk ~= -1) and ((squad.chunk == -1) or (squad.chunk.id ~= chunk.id)) then
        chunkPropertyUtils.removeSquadFromChunk(map, squad)
        local squads = chunkToSquad[chunk.id]
        if not squads then
            squads = {}
            chunkToSquad[chunk.id] = squads
        end
        squads[squad.groupNumber] = squad
        squad.chunk = chunk
    end
end

function chunkPropertyUtils.removeSquadFromChunk(map, squad)
    local chunkToSquad = map.chunkToSquad
    local chunk = squad.chunk
    if (chunk ~= -1) then
        local squads = chunkToSquad[chunk.id]
        if squads then
            squads[squad.groupNumber] = nil
            if (table_size(squads) == 0) then
                chunkToSquad[chunk.id] = nil
            end
        end
    end
end

function chunkPropertyUtils.getSquadsOnChunk(map, chunk)
    return map.chunkToSquad[chunk.id] or map.emptySquadsOnChunk
end

function chunkPropertyUtils.setPlayerBaseGenerator(map, chunk, playerGenerator)
    if (playerGenerator <= 0) then
        map.chunkToPlayerBase[chunk.id] = nil
    else
        map.chunkToPlayerBase[chunk.id] = playerGenerator
    end
end

function chunkPropertyUtils.addPlayerBaseGenerator(map, chunk, playerGenerator)
    map.chunkToPlayerBase[chunk.id] = (map.chunkToPlayerBase[chunk.id] or 0) + playerGenerator
end

function chunkPropertyUtils.processNestActiveness(map, chunk)
    local nests = chunkPropertyUtils.getNestCount(map, chunk)
    if (nests > 0) then
        local surface = map.surface
        local activeness = chunkPropertyUtils.getNestActiveness(map, chunk)
        local universe = map.universe
        local raidActiveness = chunkPropertyUtils.getRaidNestActiveness(map, chunk)
        if universe.attackUsePlayer and (chunk[PLAYER_PHEROMONE] > universe.attackPlayerThreshold) then
            chunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20))
        elseif (chunk[BASE_PHEROMONE] > 0) then
            if (surface.get_pollution(chunk) > 0) then
                chunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20))
            else
                local x = chunk.x
                local y = chunk.y
                local position = {x=0,y=0}
                position.x = x + 32
                position.y = y
                if (surface.get_pollution(position) > 0) then
                    chunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20))
                else
                    position.x = x - 32
                    if (surface.get_pollution(position) > 0) then
                        chunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20))
                    else
                        position.x = x
                        position.y = y - 32
                        if (surface.get_pollution(position) > 0) then
                            chunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20))
                        else
                            position.y = y + 32
                            if (surface.get_pollution(position) > 0) then
                                chunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20))
                            else
                                chunkPropertyUtils.setNestActiveness(map, chunk, activeness - 2)
                                if (chunk[BASE_PHEROMONE] > RAIDING_MINIMUM_BASE_THRESHOLD) then
                                    chunkPropertyUtils.setRaidNestActiveness(map, chunk, mMin(raidActiveness + 3, 20))
                                else
                                    chunkPropertyUtils.setRaidNestActiveness(map, chunk, raidActiveness - 1)
                                end
                            end
                        end
                    end
                end
            end
        else
            chunkPropertyUtils.setNestActiveness(map, chunk, activeness - 5)
            chunkPropertyUtils.setRaidNestActiveness(map, chunk, raidActiveness - 5)
        end
    else
        chunkPropertyUtils.setNestActiveness(map, chunk, 0)
        chunkPropertyUtils.setRaidNestActiveness(map, chunk, 0)
    end
end


chunkPropertyUtilsG = chunkPropertyUtils
return chunkPropertyUtils
