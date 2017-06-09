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

local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

-- module code

local function scoreTendrilChunk(squad, chunk, surface)
    return chunk[RESOURCE_PHEROMONE]
end

local function validTendrilChunk(x, chunk, neighborChunk)
    return canMoveChunkDirection(x, chunk, neighborChunk)
end

-- local function colorChunk(x, y, tileType, surface)
--     local tiles = {}
--     for xi=x+5, x + 27 do
-- 	for yi=y+5, y + 27 do
-- 	    tiles[#tiles+1] = {name=tileType, position={xi, yi}}
-- 	end
--     end
--     surface.set_tiles(tiles, false)
-- end

local function removeTendril(base, tendril)
    for i=1,#base.tendrils do
	if (base.tendrils[i] == tendril) then
	    table.remove(base.tendrils,i)
	    break
	end
    end    
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
	    removeTendril(base, tendril)
	    tendrilUtils.buildTendril(regionMap, nil, base, surface, tick)
	    -- colorChunk(chunk.x, chunk.y, "hazard-concrete-left", surface)
	    return
	elseif tendrilPath then
  	    positionFromDirectionAndChunk(tendrilDirection, tendril, tendril, 0.5)
	    mapUtils.distortPosition(tendril)
	    tendril.path[#tendril.path] = chunk
	    local position = surface.find_non_colliding_position("spitter-spawner",
								 {x=tendril.x,y=tendril.y},
								 32,
								 4)
	    if position and (tendrilPath[NEST_COUNT] <= 3)then
		tendril.x = position.x
		tendril.y = position.y
		local biterSpawner = {name="spitter-spawner", position=position}
		local hive = surface.create_entity(biterSpawner)
		registerEnemyBaseStructure(regionMap, hive, base)
	    elseif not position then
		removeTendril(base, tendril)
	    end
	end
	-- colorChunk(chunk.x, chunk.y, "concrete", surface)
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
