local tendrilUtils = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local nestUtils = require("NestUtils")
local movementUtils = require("MovementUtils")

-- constants

local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local scoreNeighborsForResource = movementUtils.scoreNeighborsForResource

local getNeighborChunks = mapUtils.getNeighborChunks

local getChunkByPosition = mapUtils.getChunkByPosition

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local buildNest = nestUtils.buildNest

local buildOutpost = nestUtils.buildOutpost

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
	    if tendril.unit.valid then
		tendril.unit.destroy()
	    end
	    table.remove(base.tendrils,i)
	    break
	end
    end    
end

local function buildTendrilPath(regionMap, tendril, surface, base, tick, natives)
    local tendrilUnit = tendril.unit
    if not tendrilUnit.valid then
	removeTendril(base, tendril)
	tendrilUtils.buildTendril(regionMap, natives, base, surface, tick)
	return
    end
    if (tendril.cycles > 0) then
	tendril.cycles = tendril.cycles - 1
	return
    end
    local tendrilPosition = tendrilUnit.position
    local chunk = getChunkByPosition(regionMap, tendrilPosition.x, tendrilPosition.y)
    if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	local tendrilPath,tendrilDirection = scoreNeighborsForResource(chunk,
								       getNeighborChunks(regionMap, chunk.x, chunk.y),
								       scoreTendrilChunk,
								       nil)
	if (tendrilDirection == -1) then
	    if (chunk[RESOURCE_GENERATOR] ~= 0) then
		buildOutpost(regionMap, natives, base, surface, tendril)
		removeTendril(base, tendril)
		tendrilUtils.buildTendril(regionMap, natives, base, surface, tick)
		colorChunk(chunk.x, chunk.y, "hazard-concrete-left", surface)
	    end
	    return
	elseif tendrilPath then
  	    positionFromDirectionAndChunk(tendrilDirection, tendrilPosition, tendrilPosition, 0.5)
	    -- mathUtils.distortPosition(tendrilPosition)
	    -- tendril.path[#tendril.path] = chunk
	    local position = surface.find_non_colliding_position("spitter-spawner",
								 tendrilPosition,
								 32,
								 2)
	    if position then
		buildNest(regionMap, base, surface, tendril.unit.position, "spitter-spawner")
		-- tendril.cycles = 3
		tendrilUnit.set_command({ type = defines.command.go_to_location,
					  destination = position,
					  distraction = defines.distraction.by_none })		
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
