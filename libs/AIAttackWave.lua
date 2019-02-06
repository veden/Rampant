local aiAttackWave = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local mathUtils = require("MathUtils")
package.path = "../?.lua;" .. package.path
local config = require("config")

-- constants

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_MAX_SQUAD_COUNT = constants.AI_MAX_SQUAD_COUNT
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST

local INTERVAL_RALLY = constants.INTERVAL_RALLY

local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE
local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local CHUNK_SIZE = constants.CHUNK_SIZE

local RALLY_CRY_DISTANCE = constants.RALLY_CRY_DISTANCE
local SETTLER_DISTANCE = constants.SETTLER_DISTANCE

local RESOURCE_MINIMUM_FORMATION_DELTA = constants.RESOURCE_MINIMUM_FORMATION_DELTA

local AI_STATE_SIEGE = constants.AI_STATE_SIEGE

local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_DISTRACTION_NONE  = defines.distraction.none

local RAIDING_MINIMUM_BASE_THRESHOLD = constants.RAIDING_MINIMUM_BASE_THRESHOLD

local AI_STATE_RAIDING = constants.AI_STATE_RAIDING

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- local PASSABLE = constants.PASSABLE

-- imported functions

local mRandom = math.random

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local getPassable = chunkPropertyUtils.getPassable
local getNestCount = chunkPropertyUtils.getNestCount
local getChunkSettlerTick = chunkPropertyUtils.getChunkSettlerTick
local setChunkSettlerTick = chunkPropertyUtils.setChunkSettlerTick
local getRallyTick = chunkPropertyUtils.getRallyTick
local setRallyTick = chunkPropertyUtils.setRallyTick

local gaussianRandomRange = mathUtils.gaussianRandomRange

local getNeighborChunks = mapUtils.getNeighborChunks
local getChunkByXY = mapUtils.getChunkByXY
local scoreNeighborsForFormation = movementUtils.scoreNeighborsForFormation
local scoreNeighborsForResource = movementUtils.scoreNeighborsForResource
local createSquad = unitGroupUtils.createSquad
local attackWaveScaling = config.attackWaveScaling
local settlerWaveScaling = config.settlerWaveScaling

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
    local basePheromone = chunk[BASE_PHEROMONE]
    if (basePheromone > 0) then
	hasBasePheromone = true
	if (natives.state == AI_STATE_RAIDING) then
	    if (basePheromone > RAIDING_MINIMUM_BASE_THRESHOLD) then
		total = total + basePheromone
	    end
	end
    end
    if natives.attackUsePollution then
	total = total + surface.get_pollution(chunk)
    end

    return (total > natives.attackWaveThreshold) and (hasBasePheromone or hasPlayerPheromone)
end

local function scoreSettlerLocation(neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE] + -neighborChunk[MOVEMENT_PHEROMONE] + -neighborChunk[PLAYER_PHEROMONE]
end

local function scoreSiegeSettlerLocation(neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE] + neighborChunk[BASE_PHEROMONE] + -neighborChunk[MOVEMENT_PHEROMONE] + -neighborChunk[PLAYER_PHEROMONE]
end

local function validSiegeSettlerLocation(map, neighborChunk)
    return (getPassable(map, neighborChunk) == CHUNK_ALL_DIRECTIONS) and (getNestCount(map, neighborChunk) == 0)
end


local function validSettlerLocation(map, chunk, neighborChunk)
    local chunkResource = chunk[RESOURCE_PHEROMONE]
    return (getPassable(map, neighborChunk) == CHUNK_ALL_DIRECTIONS) and (getNestCount(map, neighborChunk) == 0) and (neighborChunk[RESOURCE_PHEROMONE] >= (chunkResource * RESOURCE_MINIMUM_FORMATION_DELTA))
end

local function scoreUnitGroupLocation(neighborChunk)
    return neighborChunk[PLAYER_PHEROMONE] + neighborChunk[MOVEMENT_PHEROMONE] + neighborChunk[BASE_PHEROMONE]
end

local function validUnitGroupLocation(map, neighborChunk)
    return getPassable(map, neighborChunk) == CHUNK_ALL_DIRECTIONS and (getNestCount(map, neighborChunk) == 0)
end

function aiAttackWave.rallyUnits(chunk, map, surface, natives, tick)
    if ((tick - getRallyTick(map, chunk) > INTERVAL_RALLY) and (natives.points >= AI_VENGENCE_SQUAD_COST)) then
	setRallyTick(map, chunk, tick)
	local cX = chunk.x
	local cY = chunk.y
	for x=cX - RALLY_CRY_DISTANCE, cX + RALLY_CRY_DISTANCE, 32 do
	    for y=cY - RALLY_CRY_DISTANCE, cY + RALLY_CRY_DISTANCE, 32 do
		if (x ~= cX) and (y ~= cY) then
		    local rallyChunk = getChunkByXY(map, x, y)
		    if (rallyChunk ~= SENTINEL_IMPASSABLE_CHUNK) and (getNestCount(map, rallyChunk) > 0) then
			if not aiAttackWave.formSquads(map, surface, natives, rallyChunk, AI_VENGENCE_SQUAD_COST) then
			    return
			end
		    end
		end
	    end
	end
    end
end

local function noNearbySettlers(map, chunk, tick)
    local cX = chunk.x
    local cY = chunk.y
    for x=cX - SETTLER_DISTANCE, cX + SETTLER_DISTANCE, 32 do
	for y=cY - SETTLER_DISTANCE, cY + SETTLER_DISTANCE, 32 do
	    if (x ~= cX) and (y ~= cY) then
		local c = getChunkByXY(map, x, y)
		if (c ~= SENTINEL_IMPASSABLE_CHUNK) and ((tick - getChunkSettlerTick(map, c)) < 0) then
		    return false
		end
	    end
	end
    end
    return true
end

function aiAttackWave.formSettlers(map, surface, natives, chunk, cost, tick)

    if (mRandom() < natives.formSquadThreshold) and (#natives.squads < AI_MAX_SQUAD_COUNT) then

        local squadPath, squadDirection
        if (natives.state == AI_STATE_SIEGE) then
            squadPath, squadDirection = scoreNeighborsForFormation(getNeighborChunks(map, chunk.x, chunk.y),
                                                                   validSiegeSettlerLocation,
                                                                   scoreSiegeSettlerLocation,
                                                                   map)
        else
            squadPath, squadDirection = scoreNeighborsForResource(chunk,
                                                                  getNeighborChunks(map, chunk.x, chunk.y),
                                                                  validSettlerLocation,
                                                                  scoreSettlerLocation,
                                                                  map)
        end

	if (squadPath ~= SENTINEL_IMPASSABLE_CHUNK) and noNearbySettlers(map, chunk, tick) then
	    local squadPosition = surface.find_non_colliding_position("chunk-scanner-squad-rampant",
								      positionFromDirectionAndChunk(squadDirection,
												    chunk,
												    map.position,
												    0.98),
								      CHUNK_SIZE,
								      4)
	    if squadPosition then
		local squad = createSquad(squadPosition, surface, natives)

		squad.settlers = true
		squad.maxDistance = gaussianRandomRange(natives.expansionMaxDistance,
							natives.expansionMaxDistanceDerivation,
							CHUNK_SIZE * 1,
							natives.expansionMaxDistance)
		squad.originPosition.x = squadPosition.x
		squad.originPosition.y = squadPosition.y

		local scaledWaveSize = settlerWaveScaling(natives)
		local foundUnits = surface.set_multi_command({ command = { type = DEFINES_COMMAND_GROUP,
									   group = squad.group,
									   distraction = DEFINES_DISTRACTION_NONE },
							       unit_count = scaledWaveSize,
							       unit_search_distance = TRIPLE_CHUNK_SIZE })
		if (foundUnits > 0) then
		    setChunkSettlerTick(map, squadPath, tick + natives.settlerCooldown)
		    natives.points = natives.points - cost
		end
	    end
	end
    end

    return (natives.points - cost) > 0
end

function aiAttackWave.formSquads(map, surface, natives, chunk, cost)
    local valid = (cost == AI_VENGENCE_SQUAD_COST) or ((cost == AI_SQUAD_COST) and attackWaveValidCandidate(chunk, natives, surface))

    if valid and (mRandom() < natives.formSquadThreshold) and (#natives.squads < AI_MAX_SQUAD_COUNT) then

	local squadPath, squadDirection = scoreNeighborsForFormation(getNeighborChunks(map, chunk.x, chunk.y),
								     validUnitGroupLocation,
								     scoreUnitGroupLocation,
								     map)
	if (squadPath ~= SENTINEL_IMPASSABLE_CHUNK) then
	    local squadPosition = surface.find_non_colliding_position("chunk-scanner-squad-rampant",
								      positionFromDirectionAndChunk(squadDirection,
												    chunk,
												    map.position,
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

    return (natives.points - cost) > 0
end

return aiAttackWave
