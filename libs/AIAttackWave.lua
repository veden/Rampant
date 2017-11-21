local aiAttackWave = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkUtils = require("ChunkUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
package.path = "../?.lua;" .. package.path
local config = require("config")

-- constants

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC

local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE
local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local CHUNK_SIZE = constants.CHUNK_SIZE

local RALLY_CRY_DISTANCE = constants.RALLY_CRY_DISTANCE

local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_DISTRACTION_NONE  = defines.distraction.none

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

local PASSABLE = constants.PASSABLE

-- imported functions

local mRandom = math.random

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local getNestCount = chunkUtils.getNestCount
local getRallyTick = chunkUtils.getRallyTick
local setRallyTick = chunkUtils.setRallyTick

local getNeighborChunks = mapUtils.getNeighborChunks
local getChunkByPosition = mapUtils.getChunkByPosition
local scoreNeighborsForFormation = movementUtils.scoreNeighborsForFormation
local createSquad = unitGroupUtils.createSquad
local attackWaveScaling = config.attackWaveScaling

-- module code

local function attackWaveValidCandidate(chunk, natives, surface)
    local total = 0;

    local hasPlayerPheromone = false
    if natives.attackUsePlayer then
	local playerPheromone = chunk[PLAYER_PHEROMONE]
	if (playerPheromone > natives.attackPlayerThreshold) then
	    total = total + chunk[PLAYER_PHEROMONE]
	    hasPlayerPheromone = true
	elseif (playerPheromone > 0) then
	    hasPlayerPheromone = true
	end
    end
    local hasBasePheromone = false
    if (chunk[BASE_PHEROMONE] > 0) then
	hasBasePheromone = true
    end
    if natives.attackUsePollution then
	total = total + surface.get_pollution(chunk)
    end
    
    return (total > natives.attackWaveThreshold) and (hasBasePheromone or hasPlayerPheromone)
end

local function scoreUnitGroupLocation(neighborChunk)
    return neighborChunk[PLAYER_PHEROMONE] + neighborChunk[MOVEMENT_PHEROMONE] + neighborChunk[BASE_PHEROMONE]
end

local function validUnitGroupLocation(regionMap, neighborChunk)
    return neighborChunk[PASSABLE] == CHUNK_ALL_DIRECTIONS and (getNestCount(regionMap, neighborChunk) == 0)
end

function aiAttackWave.rallyUnits(chunk, regionMap, surface, natives, tick)
    if ((tick - getRallyTick(regionMap, chunk) > INTERVAL_LOGIC) and (natives.points >= AI_VENGENCE_SQUAD_COST) and
	(#natives.squads < natives.maxSquads)) then
	setRallyTick(regionMap, chunk, tick)
	local cX = chunk.x
	local cY = chunk.y
	for x=cX - RALLY_CRY_DISTANCE, cX + RALLY_CRY_DISTANCE, 32 do
	    for y=cY - RALLY_CRY_DISTANCE, cY + RALLY_CRY_DISTANCE, 32 do
		if (x ~= cX) and (y ~= cY) then
		    local rallyChunk = getChunkByPosition(regionMap, x, y)
		    if (rallyChunk ~= SENTINEL_IMPASSABLE_CHUNK) and (getNestCount(regionMap, rallyChunk) > 0) then
			aiAttackWave.formSquads(regionMap, surface, natives, rallyChunk, AI_VENGENCE_SQUAD_COST)
			if (natives.points < AI_VENGENCE_SQUAD_COST) and (#natives.squads < natives.maxSquads) then
			    return
			end
		    end
		end
	    end
	end
    end
end

function aiAttackWave.formSquads(regionMap, surface, natives, chunk, cost)
    local valid = (cost == AI_VENGENCE_SQUAD_COST) or ((cost == AI_SQUAD_COST) and attackWaveValidCandidate(chunk, natives, surface))

    if valid and (mRandom() < natives.formSquadThreshold) then
	
	local squadPath, squadDirection = scoreNeighborsForFormation(getNeighborChunks(regionMap, chunk.x, chunk.y),
								     validUnitGroupLocation,
								     scoreUnitGroupLocation,
								     regionMap)
	if (squadPath ~= SENTINEL_IMPASSABLE_CHUNK) then
	    local squadPosition = surface.find_non_colliding_position("biter-spawner-hive-rampant",
								      positionFromDirectionAndChunk(squadDirection,
												    chunk,
												    regionMap.position,
												    0.98),
								      CHUNK_SIZE,
								      4)
	    if squadPosition then
		local squad = createSquad(squadPosition, surface, natives)
		
		squad.rabid = mRandom() < 0.03

		local scaledWaveSize = attackWaveScaling(natives)
		local foundUnits = surface.set_multi_command({ command = { type = DEFINES_COMMAND_GROUP,
									   group = squad.group,
									   distraction = DEFINES_DISTRACTION_NONE },
							       unit_count = scaledWaveSize,
							       unit_search_distance = TRIPLE_CHUNK_SIZE })
		if (foundUnits > 0) then
		    natives.points = natives.points - cost
		end
	    end
	end
    end
end

return aiAttackWave
