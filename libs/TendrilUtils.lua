local tendrilUtils = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local baseRegisterUtils = require("BaseRegisterUtils")
local neighborsUtils = require("NeighborUtils")
local mathUtils = require("MathUtils")

local buildUtils = require("BuildUtils")
local mathUtils = require("MathUtils")

-- constants

local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local NEST_COUNT = constants.NEST_COUNT

-- imported functions

local scoreNeighborsForAttack = neighborsUtils.scoreNeighborsForAttack

local getNeighborChunks = mapUtils.getNeighborChunks

local getChunkByPosition = mapUtils.getChunkByPosition

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local registerEnemyBaseStructure = baseRegisterUtils.registerEnemyBaseStructure

local buildOutpost = buildUtils.buildOutpost

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

-- module code

local function scoreTendrilChunk(squad, chunk)
    return chunk[RESOURCE_PHEROMONE]
end

local function colorChunk(x, y, tileType, surface)
    local tiles = {}
    for xi=x+5, x + 27 do
	for yi=y+5, y + 27 do
	    tiles[#tiles+1] = {name=tileType, position={xi, yi}}
	end
    end
    surface.set_tiles(tiles, false)
end

local function removeTendril(base, tendril)
    for i=1,#base.tendrils do
	if (base.tendrils[i] == tendril) then
	    table.remove(base.tendrils,i)
	    break
	end
    end    
end

local function buildTendrilPath(regionMap, tendril, surface, base, tick, natives)
    local tendrilUnit = tendril.unit
    if not tendrilUnit.valid then
	return
    end
    if (tendril.cycles > 0) then
	tendril.cycles = tendril.cycles - 1
	return
    end
    local tendrilPosition = tendrilUnit.position
    local chunk = getChunkByPosition(regionMap, tendrilPosition.x, tendrilPosition.y)
    if chunk then
	local tendrilPath,tendrilDirection = scoreNeighborsForAttack(chunk,
								     getNeighborChunks(regionMap,
										       chunk.cX,
										       chunk.cY),
								     scoreTendrilChunk,
								     nil)
	if (tendrilDirection == -1) then
	    -- removeTendril(base, tendril)
	    -- buildOutpost(regionMap, natives, base, surface, tendril)
	    -- tendrilUtils.buildTendril(regionMap, natives, base, surface, tick)
	    colorChunk(chunk.x, chunk.y, "hazard-concrete-left", surface)
	    return
	elseif tendrilPath then
  	    positionFromDirectionAndChunk(tendrilDirection, tendrilPosition, tendrilPosition, 0.5)
	    mathUtils.distortPosition(tendrilPosition)
	    -- tendril.path[#tendril.path] = chunk
	    local position = surface.find_non_colliding_position("spitter-spawner",
								 tendrilPosition,
								 32,
								 2)
	    if position then
		tendril.target = position
		tendrilUnit.set_command({ type = defines.command.go_to_location,
					  destination = position,
					  distraction = defines.distraction.by_damage })
		
		-- needs to check to make sure unit still exists as well.
		--tendril.cycles = 2
		-- tendril.x = position.x
		-- tendril.y = position.y
		-- local biterSpawner = {name="spitter-spawner", position=position}
		-- local hive = surface.create_entity(biterSpawner)
		-- registerEnemyBaseStructure(regionMap, hive, base)
	    -- elseif not position then
		-- 	removeTendril(base, tendril)
	    end
	end
	colorChunk(chunk.x, chunk.y, "concrete", surface)
    end
end

function tendrilUtils.advanceTendrils(regionMap, base, surface, tick, natives)
    for i=1, #base.tendrils do
    	buildTendrilPath(regionMap, base.tendrils[i], surface, base, tick, natives)
    end
end

function tendrilUtils.createTendril(base, surface)
    local position = surface.find_non_colliding_position("small-tendril-biter-rampant",
							 {x=base.x,y=base.y},
							 32,
							 4)
    if not position then
	return nil
    end

    local entity = surface.create_entity({name="small-tendril-biter-rampant", position=position})
    
    local tendril = {
	unit = entity,
	penalties = {},
	target = entity.position,
	cycles = 0,
	path = {}
    }
    
    return tendril
end

function tendrilUtils.buildTendril(regionMap, natives, base, surface, tick)
    -- local chunk = getChunkByPosition(regionMap, base.x, base.y)
    -- if chunk then
    -- 	local tempNeighbors = {nil, nil, nil, nil, nil, nil, nil, nil}
    -- 	buildTendrilPath(regionMap, chunk, surface, base, tempNeighbors)
    local tendril = tendrilUtils.createTendril(base, surface)
    if tendril then
	base.tendrils[#base.tendrils+1] = tendril
    end
    -- end
end

return tendrilUtils
