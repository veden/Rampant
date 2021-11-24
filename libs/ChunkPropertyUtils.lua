if chunkPropertyUtilsG then
    return chunkPropertyUtilsG
end
local chunkPropertyUtils = {}

local constants = require("Constants")

-- constants

local RAIDING_MINIMUM_BASE_THRESHOLD = constants.RAIDING_MINIMUM_BASE_THRESHOLD

local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local MOVEMENT_GENERATOR_PERSISTANCE = constants.MOVEMENT_GENERATOR_PERSISTANCE
local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

-- imported functions

local mMin = math.min

-- module code

function chunkPropertyUtils.getNestCount(map, chunk)
    return map.chunkToNests[chunk] or 0
end

function chunkPropertyUtils.getTurretCount(map, chunk)
    return map.chunkToTurrets[chunk] or 0
end

function chunkPropertyUtils.getTrapCount(map, chunk)
    return map.chunkToTraps[chunk] or 0
end

function chunkPropertyUtils.getUtilityCount(map, chunk)
    return map.chunkToUtilities[chunk] or 0
end

function chunkPropertyUtils.getHiveCount(map, chunk)
    return map.chunkToHives[chunk] or 0
end

function chunkPropertyUtils.setTurretCount(map, chunk, count)
    if (count <= 0) then
        map.chunkToTurrets[chunk] = nil
    else
        map.chunkToTurrets[chunk] = count
    end
end

function chunkPropertyUtils.setHiveCount(map, chunk, count)
    if (count <= 0) then
        map.chunkToHives[chunk] = nil
    else
        map.chunkToHives[chunk] = count
    end
end

function chunkPropertyUtils.setTrapCount(map, chunk, count)
    if (count <= 0) then
        map.chunkToTraps[chunk] = nil
    else
        map.chunkToTraps[chunk] = count
    end
end

function chunkPropertyUtils.setUtilityCount(map, chunk, count)
    if (count <= 0) then
        map.chunkToUtilities[chunk] = nil
    else
        map.chunkToUtilities[chunk] = count
    end
end

function chunkPropertyUtils.setNestCount(map, chunk, count)
    if (count <= 0) then
        map.chunkToNests[chunk] = nil
        if (map.processMigrationIterator == chunk) then
            map.processMigrationIterator = nil
        end
        if (map.processNestIterator == chunk) then
            map.processNestIterator = nil
        end
    else
        map.chunkToNests[chunk] = count
    end
end

function chunkPropertyUtils.getNestCount(map, chunk)
    return map.chunkToNests[chunk] or 0
end

function chunkPropertyUtils.getChunkBase(map, chunk)
    return map.chunkToBase[chunk]
end

function chunkPropertyUtils.setChunkBase(map, chunk, base)
    map.chunkToBase[chunk] = base
end

function chunkPropertyUtils.getEnemyStructureCount(map, chunk)
    return (map.chunkToNests[chunk] or 0) + (map.chunkToTurrets[chunk] or 0) + (map.chunkToTraps[chunk] or 0) +
        (map.chunkToUtilities[chunk] or 0) + (map.chunkToHives[chunk] or 0)
end

function chunkPropertyUtils.getRetreatTick(map, chunk)
    return map.chunkToRetreats[chunk] or 0
end

function chunkPropertyUtils.getRallyTick(map, chunk)
    return map.chunkToRallys[chunk] or 0
end

function chunkPropertyUtils.setRallyTick(map, chunk, tick)
    map.chunkToRallys[chunk] = tick
end

function chunkPropertyUtils.setRetreatTick(map, chunk, tick)
    map.chunkToRetreats[chunk] = tick
end

function chunkPropertyUtils.setResourceGenerator(map, chunk, resourceGenerator)
    if (resourceGenerator <= 0) then
        map.chunkToResource[chunk] = nil
    else
        map.chunkToResource[chunk] = resourceGenerator
    end
end

function chunkPropertyUtils.getResourceGenerator(map, chunk)
    return map.chunkToResource[chunk] or 0
end

function chunkPropertyUtils.addResourceGenerator(map, chunk, delta)
    map.chunkToResource[chunk] = (map.chunkToResource[chunk] or 0) + delta
end

function chunkPropertyUtils.getDeathGenerator(map, chunk)
    return map.chunkToDeathGenerator[chunk] or 0
end

function chunkPropertyUtils.getPassable(map, chunk)
    return map.chunkToPassable[chunk] or CHUNK_ALL_DIRECTIONS
end


function chunkPropertyUtils.getRaidNestActiveness(map, chunk)
    return map.chunkToActiveRaidNest[chunk] or 0
end

function chunkPropertyUtils.setRaidNestActiveness(map, chunk, value)
    if (value <= 0) then
        if (map.chunkToActiveRaidNest[chunk] ~= nil) then
            map.activeRaidNests = map.activeRaidNests - 1
        end
        if (map.processActiveRaidSpawnerIterator == chunk) then
            map.processActiveRaidSpawnerIterator = nil
        end
        map.chunkToActiveRaidNest[chunk] = nil
    else
        if (map.chunkToActiveRaidNest[chunk] == nil) then
            map.activeRaidNests = map.activeRaidNests + 1
        end
        map.chunkToActiveRaidNest[chunk] = value
    end
end

function chunkPropertyUtils.getNestActiveTick(map, chunk)
    return map.tickActiveNest[chunk] or 0
end

function chunkPropertyUtils.setNestActiveTick(map, chunk, tick)
    if (tick == 0) then
        map.tickActiveNest[chunk] = nil
    else
        map.tickActiveNest[chunk] = tick
    end
end

function chunkPropertyUtils.getNestActiveness(map, chunk)
    return map.chunkToActiveNest[chunk] or 0
end

function chunkPropertyUtils.setNestActiveness(map, chunk, value)
    if (value <= 0) then
        if (map.chunkToActiveNest[chunk] ~= nil) then
            map.activeNests = map.activeNests - 1
        end
        if (map.processActiveSpawnerIterator == chunk) then
            map.processActiveSpawnerIterator = nil
        end
        map.chunkToActiveNest[chunk] = nil
    else
        if (map.chunkToActiveNest[chunk] == nil) then
            map.activeNests = map.activeNests + 1
        end
        map.chunkToActiveNest[chunk] = value
    end
end

function chunkPropertyUtils.setPassable(map, chunk, value)
    if (value == CHUNK_ALL_DIRECTIONS) then
        map.chunkToPassable[chunk] = nil
    else
        map.chunkToPassable[chunk] = value
    end
end

function chunkPropertyUtils.getPathRating(map, chunk)
    return map.chunkToPathRating[chunk] or 1
end

function chunkPropertyUtils.setPathRating(map, chunk, value)
    if (value == 1) then
        map.chunkToPathRating[chunk] = nil
    else
        map.chunkToPathRating[chunk] = value
    end
end

function chunkPropertyUtils.addDeathGenerator(map, chunk, value)
    map.chunkToDeathGenerator[chunk] = (map.chunkToDeathGenerator[chunk] or 0) + value
end

function chunkPropertyUtils.addVictoryGenerator(map, chunk, value)
    map.chunkToVictory[chunk] = (map.chunkToVictory[chunk] or 0) + value
end

function chunkPropertyUtils.decayDeathGenerator(map, chunk)
    local gen = map.chunkToDeathGenerator[chunk]
    if gen then
        gen = gen * MOVEMENT_GENERATOR_PERSISTANCE

        if (gen >= -2) and (gen <= 2) then
            map.chunkToDeathGenerator[chunk] = nil
        else
            map.chunkToDeathGenerator[chunk] = gen
        end
    end
end

function chunkPropertyUtils.addPlayerToChunk(map, chunk, name)
    local playerChunks = map.playerToChunk
    local playerCountChunks = map.chunkToPlayerCount
    local playerChunk = playerChunks[name]
    if not playerChunk then
        playerChunks[name] = chunk
        local playerCount = playerCountChunks[chunk]
        if not playerCount then
            playerCountChunks[chunk] = 1
        else
            playerCountChunks[chunk] = playerCount + 1
        end
    elseif (playerChunk ~= chunk) then
        playerChunks[name] = chunk
        local playerCount = playerCountChunks[playerChunk]
        chunkPropertyUtils.setPlayersOnChunk(map, playerChunk, playerCount - 1)
        playerCount = playerCountChunks[chunk]
        if not playerCount then
            playerCountChunks[chunk] = 1
        else
            playerCountChunks[chunk] = playerCount + 1
        end
    end
end

function chunkPropertyUtils.setPlayersOnChunk(map, chunk, value)
    if (value <= 0) then
        map.chunkToPlayerCount[chunk] = nil
    else
        map.chunkToPlayerCount[chunk] = value
    end
end

function chunkPropertyUtils.getPlayersOnChunk(map, chunk)
    return map.chunkToPlayerCount[chunk] or 0
end

function chunkPropertyUtils.getPlayerBaseGenerator(map, chunk)
    return map.chunkToPlayerBase[chunk] or 0
end

function chunkPropertyUtils.addSquadToChunk(map, chunk, squad)
    local chunkToSquad = map.chunkToSquad

    if (chunk ~= -1) and (squad.chunk ~= chunk) then
        chunkPropertyUtils.removeSquadFromChunk(map, squad)
        local squads = chunkToSquad[chunk]
        if not squads then
            squads = {}
            chunkToSquad[chunk] = squads
        end
        squads[squad.groupNumber] = squad
        squad.chunk = chunk
    end
end

function chunkPropertyUtils.removeSquadFromChunk(map, squad)
    local chunkToSquad = map.chunkToSquad
    local chunk = squad.chunk
    local squads = chunkToSquad[chunk]
    if squads then
        squads[squad.groupNumber] = nil
        if (table_size(squads) == 0) then
            chunkToSquad[chunk] = nil
        end
    end
end

function chunkPropertyUtils.getSquadsOnChunk(map, chunk)
    return map.chunkToSquad[chunk] or map.emptySquadsOnChunk
end

function chunkPropertyUtils.setPlayerBaseGenerator(map, chunk, playerGenerator)
    if (playerGenerator <= 0) then
        map.chunkToPlayerBase[chunk] = nil
    else
        map.chunkToPlayerBase[chunk] = playerGenerator
    end
end

function chunkPropertyUtils.addPlayerBaseGenerator(map, chunk, playerGenerator)
    map.chunkToPlayerBase[chunk] = (map.chunkToPlayerBase[chunk] or 0) + playerGenerator
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
            local position = universe.position
            if (surface.get_pollution(chunk) > 0) then
                chunkPropertyUtils.setNestActiveness(map, chunk, mMin(activeness + 5, 20))
            else
                local x = chunk.x
                local y = chunk.y
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
