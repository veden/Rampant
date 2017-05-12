local aiBuilding = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local neighborUtils = require("NeighborUtils")
package.path = "../?.lua;" .. package.path
local config = require("config")

-- constants

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local AI_MAX_SQUAD_COUNT = constants.AI_MAX_SQUAD_COUNT

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
local CHUNK_SIZE = constants.CHUNK_SIZE
local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE
local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE

local RALLY_CRY_DISTANCE = 3

local CHUNK_BASE = constants.CHUNK_BASE

-- imported functions

local getNeighborChunks = mapUtils.getNeighborChunks
local getChunkByIndex = mapUtils.getChunkByIndex
local scoreNeighbors = neighborUtils.scoreNeighbors
local createSquad = unitGroupUtils.createSquad
local attackWaveScaling = config.attackWaveScaling

local mMax = math.max

-- module code

local function attackWaveValidCandidate(chunk, natives, surface, evolutionFactor)
    local total = 0;

    if natives.attackUsePlayer then
	local playerPheromone = chunk[PLAYER_PHEROMONE]
	if (playerPheromone > natives.attackPlayerThreshold) then
	    total = total + chunk[PLAYER_PHEROMONE]
	end
    end
    if natives.attackUsePollution then
	total = total + surface.get_pollution({chunk.pX, chunk.pY})
    end

    local threshold = natives.attackThresholdRange
    local delta = threshold * evolutionFactor
    
    if (total > ((threshold - delta) + natives.attackThresholdMin)) then
	return true
    else 
	return false
    end
end

local function scoreUnitGroupLocation(position, squad, neighborChunk, surface)
    return surface.get_pollution(position) + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[MOVEMENT_PHEROMONE] + neighborChunk[BASE_PHEROMONE]
end

local function validUnitGroupLocation(x, chunk, neighborChunk)
    return neighborChunk[NORTH_SOUTH_PASSABLE] and neighborChunk[EAST_WEST_PASSABLE] and neighborChunk[CHUNK_BASE] ~= nil
end

function aiBuilding.rallyUnits(chunk, regionMap, surface, natives, evolutionFactor)
    local cX = chunk.cX
    local cY = chunk.cY
    for x=cX - RALLY_CRY_DISTANCE, cX + RALLY_CRY_DISTANCE do
	for y=cY - RALLY_CRY_DISTANCE, cY + RALLY_CRY_DISTANCE do
	    local rallyChunk = getChunkByIndex(regionMap, x, y)
	    if (rallyChunk ~= nil) and (x ~= cX) and (y ~= cY) and (rallyChunk[CHUNK_BASE] ~= nil) then
		aiBuilding.formSquads(regionMap, surface, natives, rallyChunk, evolutionFactor, AI_VENGENCE_SQUAD_COST)
	    end
	end
    end
end

function aiBuilding.formSquads(regionMap, surface, natives, chunk, evolution_factor, cost)
    if (natives.points > cost) and (chunk[CHUNK_BASE] ~= nil) and (#natives.squads < (AI_MAX_SQUAD_COUNT * evolution_factor)) then
	local valid = false
	if not surface.peaceful_mode then
	    if (cost == AI_VENGENCE_SQUAD_COST) then
		valid = true
	    elseif (cost == AI_SQUAD_COST) then
		valid = attackWaveValidCandidate(chunk, natives, surface, evolution_factor)
	    end
	end 
	if valid and (math.random() < mMax((0.25 * evolution_factor), 0.10)) then
	    local squadPosition = {x=0, y=0}
	    local squadPath, _ = scoreNeighbors(chunk,
						getNeighborChunks(regionMap, chunk.cX, chunk.cY),
						validUnitGroupLocation,
						scoreUnitGroupLocation,
						nil,
						surface,
						squadPosition,
						false)
	    if (squadPath ~= nil) then
		squadPosition.x = squadPath.pX + HALF_CHUNK_SIZE
		squadPosition.y = squadPath.pY + HALF_CHUNK_SIZE
		
		local squad = createSquad(squadPosition, surface, natives)
		
		if (math.random() < 0.03) then
		    squad.rabid = true
		end

		local scaledWaveSize = attackWaveScaling(evolution_factor, natives)
		local foundUnits = surface.set_multi_command({ command = { type = defines.command.group,
									   group = squad.group,
									   distraction = defines.distraction.none },
							       unit_count = scaledWaveSize,
							       unit_search_distance = (CHUNK_SIZE * 3)})
		if (foundUnits > 0) then
		    natives.points = natives.points - cost
		end
	    end
	end
    end
end

return aiBuilding
