local chunkPropertyUtils = {}

local constants = require("Constants")

-- imported functions

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT
local DEATH_PHEROMONE_DECAY_AMOUNT = constants.DEATH_PHEROMONE_DECAY_AMOUNT
local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

-- module code

function chunkPropertyUtils.getNestCount(map, chunk)
    return map.chunkToNests[chunk] or 0
end

function chunkPropertyUtils.getWormCount(map, chunk)
    return map.chunkToWorms[chunk] or 0
end

function chunkPropertyUtils.setWormCount(map, chunk, count)
    if (count == 0) then
	map.chunkToWorms[chunk] = nil
    else
	map.chunkToWorms[chunk] = count
    end
end

function chunkPropertyUtils.setNestCount(map, chunk, count)
    if (count == 0) then
	map.chunkToNests[chunk] = nil
    else
	map.chunkToNests[chunk] = count
    end
end

function chunkPropertyUtils.getChunkSettlerTick(map, chunk)
    return map.chunkToSettler[chunk] or 0
end

function chunkPropertyUtils.setChunkSettlerTick(map, chunk, tick)
    if (tick == 0) then
	map.chunkToSettler[chunk] = nil
    else
	map.chunkToSettler[chunk] = tick
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

function chunkPropertyUtils.getWormCount(map, chunk)
    return map.chunkToWorms[chunk] or 0
end

function chunkPropertyUtils.getEnemyStructureCount(map, chunk)
    return (map.chunkToNests[chunk] or 0) + (map.chunkToWorms[chunk] or 0)
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
    if (resourceGenerator == 0) then
	map.chunkToResource[chunk] = nil
    else
	map.chunkToResource[chunk] = resourceGenerator
    end
end

function chunkPropertyUtils.setChunkSpawnerEggTick(map, chunk, tick)
    map.chunkToSpawner[chunk] = tick
end

function chunkPropertyUtils.getChunkSpawnerEggTick(map, chunk)
    return map.chunkToSpawner[chunk] or 0
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

function chunkPropertyUtils.addDeathGenerator(map, chunk)
    map.chunkToDeathGenerator[chunk] = (map.chunkToDeathGenerator[chunk] or 0) + DEATH_PHEROMONE_GENERATOR_AMOUNT
end

function chunkPropertyUtils.decayDeathGenerator(map, chunk)
    local gen = map.chunkToDeathGenerator[chunk]
    if gen and (gen > 0) then
	gen = gen - DEATH_PHEROMONE_DECAY_AMOUNT
    end
    if (gen == 0) then
	map.chunkToDeathGenerator[chunk] = nil
    else
	map.chunkToDeathGenerator[chunk] = gen
    end
end

function chunkPropertyUtils.getPlayerBaseGenerator(map, chunk)
    return map.chunkToPlayerBase[chunk] or 0
end

function chunkPropertyUtils.addSquadToChunk(map, chunk, squad)
    local chunkToSquad = map.chunkToSquad

    if squad.chunk ~= chunk then
	chunkPropertyUtils.removeSquadFromChunk(map, squad)
    end
    
    if not chunkToSquad[chunk] then
	chunkToSquad[chunk] = {}
    end
    chunkToSquad[chunk][squad] = squad
    
    squad.chunk = chunk
end

function chunkPropertyUtils.removeSquadFromChunk(map, squad)
    local chunkToSquad = map.chunkToSquad
    local chunk = squad.chunk
    if chunk then
	local squads = chunkToSquad[chunk]
	if squads then
	    squads[squad] = nil
	    local i = 0
	    for _,_ in pairs(squads) do
		i = i + 1
		break
	    end
	    if (i == 0) then
	    	chunkToSquad[chunk] = nil
	    end
	end
    end
end

function chunkPropertyUtils.getSquadsOnChunk(map, chunk)
    return map.chunkToSquad[chunk] or {}
end

function chunkPropertyUtils.setPlayerBaseGenerator(map, chunk, playerGenerator)
    if (playerGenerator == 0) then
	map.chunkToPlayerBase[chunk] = nil
    else
	map.chunkToPlayerBase[chunk] = playerGenerator
    end
end

function chunkPropertyUtils.addPlayerBaseGenerator(map, chunk, playerGenerator)
    map.chunkToPlayerBase[chunk] = (map.chunkToPlayerBase[chunk] or 0) + playerGenerator
end

return chunkPropertyUtils
