local aiAttackWave = {}

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

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST

local RALLY_TRIGGERED = constants.RALLY_TRIGGERED
local INTERVAL_LOGIC = constants.INTERVAL_LOGIC

local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE
local PASSABLE = constants.PASSABLE
local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local RALLY_CRY_DISTANCE = constants.RALLY_CRY_DISTANCE

local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_DISTRACTION_NONE  = defines.distraction.none

local NEST_COUNT = constants.NEST_COUNT

-- imported functions

local mRandom = math.random

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local getNeighborChunks = mapUtils.getNeighborChunks
local getChunkByIndex = mapUtils.getChunkByIndex
local scoreNeighborsForFormation = neighborUtils.scoreNeighborsForFormation
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

local function validUnitGroupLocation(neighborChunk)
    return (neighborChunk[PASSABLE] == CHUNK_ALL_DIRECTIONS) and (neighborChunk[NEST_COUNT] == 0)
end

function aiAttackWave.rallyUnits(chunk, regionMap, surface, natives, tick)
    if (tick - chunk[RALLY_TRIGGERED] > INTERVAL_LOGIC) and (natives.points >= AI_VENGENCE_SQUAD_COST) then
	chunk[RALLY_TRIGGERED] = tick
	local cX = chunk.cX
	local cY = chunk.cY
	for x=cX - RALLY_CRY_DISTANCE, cX + RALLY_CRY_DISTANCE do
	    for y=cY - RALLY_CRY_DISTANCE, cY + RALLY_CRY_DISTANCE do
		if (x ~= cX) and (y ~= cY) then
		    local rallyChunk = getChunkByIndex(regionMap, x, y)
		    if rallyChunk and (rallyChunk[NEST_COUNT] ~= 0) then
			aiAttackWave.formSquads(regionMap, surface, natives, rallyChunk, AI_VENGENCE_SQUAD_COST)
			if (natives.points < AI_VENGENCE_SQUAD_COST) then
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
	
	local squadPath, squadDirection = scoreNeighborsForFormation(getNeighborChunks(regionMap, chunk.cX, chunk.cY),
								     validUnitGroupLocation,
								     scoreUnitGroupLocation)
	if squadPath then
	    local squadPosition = surface.find_non_colliding_position("biter-spawner-hive",
								      positionFromDirectionAndChunk(squadDirection,
												    chunk,
												    regionMap.position,
												    0.98),
								      32,
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
