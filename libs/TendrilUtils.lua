local tendrilUtils = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local baseRegisterUtils = require("BaseRegisterUtils")
local neighborsUtils = require("NeighborUtils")

-- constants

local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local NEST_COUNT = constants.NEST_COUNT

-- imported functions

local scoreNeighborsWithDirection = neighborsUtils.scoreNeighborsWithDirection

local getNeighborChunks = mapUtils.getNeighborChunks

local getChunkByPosition = mapUtils.getChunkByPosition

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local registerEnemyBaseStructure = baseRegisterUtils.registerEnemyBaseStructure

local canMoveChunkDirection = mapUtils.canMoveChunkDirection

-- module code

local function scoreTendrilChunk(squad, chunk, surface)
    return chunk[RESOURCE_PHEROMONE]
end

local function validTendrilChunk(x, chunk, neighborChunk)
    return canMoveChunkDirection(x, chunk, neighborChunk)
end

local function buildTendrilPath(regionMap, tendril, surface, base, tempNeighbors)
    local chunk = getChunkByPosition(regionMap, tendril.x, tendril.y)
    if chunk then
	local tendrilPath,tendrilDirection = scoreNeighborsWithDirection(chunk,
									 getNeighborChunks(regionMap,
											   chunk.cX,
											   chunk.cY,
											   tempNeighbors),
									 validTendrilChunk,
									 scoreTendrilChunk,
									 nil,
									 surface,
									 true)
	if (tendrilDirection == -1) then
	    tendril.path[#tendril.path] = chunk
	    local biterSpawner = {name="spitter-spawner", position={x=chunk.x,y=chunk.y}}
	    local hive = surface.create_entity(biterSpawner)
	    registerEnemyBaseStructure(regionMap, hive, base)
	    for i=1,#base.tendrils do
		if (base.tendrils[i] == tendril) then
		    table.remove(base.tendrils,i)
		    break
		end
	    end
	    tendrilUtils.buildTendril(regionMap, nil, base, surface, tick)
	    return
	elseif tendrilPath then
  	    positionFromDirectionAndChunk(tendrilDirection, chunk, tendril, 0.95)
	    tendril.path[#tendril.path] = chunk
	    local biterSpawner = {name="spitter-spawner", position={x=tendril.x,y=tendril.y}}
	    local hive = surface.create_entity(biterSpawner)
	    registerEnemyBaseStructure(regionMap, hive, base)
	end
    end
end

function tendrilUtils.advanceTendrils(regionMap, base, surface, tempNeighbors)
    for i=1, #base.tendrils do
	buildTendrilPath(regionMap, base.tendrils[i], surface, base, tempNeighbors)
    end
end

function tendrilUtils.createTendril(base)
    local tendril = {
	x = base.x,
	y = base.y,
	path = {}
    }
    return tendril
end

function tendrilUtils.buildTendril(regionMap, natives, base, surface, tick)
    -- local chunk = getChunkByPosition(regionMap, base.x, base.y)
    -- if chunk then
    -- 	local tempNeighbors = {nil, nil, nil, nil, nil, nil, nil, nil}
    -- 	buildTendrilPath(regionMap, chunk, surface, base, tempNeighbors)
    base.tendrils[#base.tendrils+1] = tendrilUtils.createTendril(base)
    -- end
end



return tendrilUtils
