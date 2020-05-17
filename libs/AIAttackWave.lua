if (aiAttackWaveG) then
    return aiAttackWaveG
end
local aiAttackWave = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local mathUtils = require("MathUtils")
local baseUtils = require("BaseUtils")
local config = require("__Rampant__/config")

-- constants

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION = constants.AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION
local AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION = constants.AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION


local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_SETTLER_COST = constants.AI_SETTLER_COST
local AI_MAX_SQUAD_COUNT = constants.AI_MAX_SQUAD_COUNT
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE

local INTERVAL_RALLY = constants.INTERVAL_RALLY

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local CHUNK_SIZE = constants.CHUNK_SIZE

local RALLY_CRY_DISTANCE = constants.RALLY_CRY_DISTANCE
local SETTLER_DISTANCE = constants.SETTLER_DISTANCE

local RESOURCE_MINIMUM_FORMATION_DELTA = constants.RESOURCE_MINIMUM_FORMATION_DELTA

local AI_STATE_SIEGE = constants.AI_STATE_SIEGE

local AI_STATE_RAIDING = constants.AI_STATE_RAIDING

-- imported functions

local randomTickEvent = mathUtils.randomTickEvent

local mRandom = math.random

local createSpawnerProxies = baseUtils.createSpawnerProxies

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local getPassable = chunkPropertyUtils.getPassable
local getNestCount = chunkPropertyUtils.getNestCount
local getChunkSettlerTick = chunkPropertyUtils.getChunkSettlerTick
local getRaidNestActiveness = chunkPropertyUtils.getRaidNestActiveness
local getNestActiveness = chunkPropertyUtils.getNestActiveness
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

local function attackWaveValidCandidate(chunk, natives, map)
    local isValid = getNestActiveness(map, chunk)
    if natives.state == AI_STATE_RAIDING then
        isValid = isValid + getRaidNestActiveness(map, chunk)
    end
    return (isValid > 0)
end

local function scoreSettlerLocation(neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE] +
        neighborChunk[MOVEMENT_PHEROMONE] +
        -neighborChunk[PLAYER_PHEROMONE]
end

local function scoreSiegeSettlerLocation(neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE] +
        neighborChunk[BASE_PHEROMONE] +
        neighborChunk[MOVEMENT_PHEROMONE] +
        -neighborChunk[PLAYER_PHEROMONE]
end

local function scoreUnitGroupLocation(neighborChunk)
    return neighborChunk[PLAYER_PHEROMONE] +
        neighborChunk[MOVEMENT_PHEROMONE] +
        neighborChunk[BASE_PHEROMONE]
end

local function validSiegeSettlerLocation(map, neighborChunk)
    return (getPassable(map, neighborChunk) == CHUNK_ALL_DIRECTIONS) and
        (getNestCount(map, neighborChunk) == 0)
end

local function validSettlerLocation(map, chunk, neighborChunk)
    local chunkResource = chunk[RESOURCE_PHEROMONE]
    return (getPassable(map, neighborChunk) == CHUNK_ALL_DIRECTIONS) and
        (getNestCount(map, neighborChunk) == 0) and
        (neighborChunk[RESOURCE_PHEROMONE] >= (chunkResource * RESOURCE_MINIMUM_FORMATION_DELTA))
end

local function validUnitGroupLocation(map, neighborChunk)
    return getPassable(map, neighborChunk) == CHUNK_ALL_DIRECTIONS and
        (getNestCount(map, neighborChunk) == 0)
end

function aiAttackWave.rallyUnits(chunk, map, surface, tick)
    if ((tick - getRallyTick(map, chunk) > INTERVAL_RALLY) and (map.natives.points >= AI_VENGENCE_SQUAD_COST)) then
        setRallyTick(map, chunk, tick)
        local cX = chunk.x
        local cY = chunk.y
        for x=cX - RALLY_CRY_DISTANCE, cX + RALLY_CRY_DISTANCE, 32 do
            for y=cY - RALLY_CRY_DISTANCE, cY + RALLY_CRY_DISTANCE, 32 do
                if (x ~= cX) and (y ~= cY) then
                    local rallyChunk = getChunkByXY(map, x, y)
                    if (rallyChunk ~= -1) and (getNestCount(map, rallyChunk) > 0) then
                        if not aiAttackWave.formVengenceSquad(map, surface, rallyChunk) then
                            return false
                        end
                    end
                end
            end
        end
        return true
    end
end

function aiAttackWave.formAttackWave(chunk, map, surface, tick)
    if (map.natives.points >= AI_SQUAD_COST) then
        local cX = chunk.x
        local cY = chunk.y
        for x=cX - RALLY_CRY_DISTANCE, cX + RALLY_CRY_DISTANCE, 32 do
            for y=cY - RALLY_CRY_DISTANCE, cY + RALLY_CRY_DISTANCE, 32 do
                if (x ~= cX) and (y ~= cY) then
                    local rallyChunk = getChunkByXY(map, x, y)
                    if (rallyChunk ~= -1) and (getNestCount(map, rallyChunk) > 0) then
                        if not aiAttackWave.formSquads(map, surface, rallyChunk, tick) then
                            return false
                        end
                    end
                end
            end
        end
        return true
    end
    return false
end

local function noNearbySettlers(map, chunk, tick)
    local cX = chunk.x
    local cY = chunk.y
    for x=cX - SETTLER_DISTANCE, cX + SETTLER_DISTANCE, 32 do
        for y=cY - SETTLER_DISTANCE, cY + SETTLER_DISTANCE, 32 do
            if (x ~= cX) and (y ~= cY) then
                local c = getChunkByXY(map, x, y)
                if (c ~= -1) and ((tick - getChunkSettlerTick(map, c)) < 0) then
                    return false
                end
            end
        end
    end
    return true
end

function aiAttackWave.formSettlers(map, surface, chunk, tick)
    local natives = map.natives
    if (mRandom() < natives.formSquadThreshold) and (natives.remainingSquads > 0) then

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

        if (squadPath ~= -1) and noNearbySettlers(map, chunk, tick) then
            local squadPosition = surface.find_non_colliding_position("chunk-scanner-squad-rampant",
                                                                      positionFromDirectionAndChunk(squadDirection,
                                                                                                    chunk,
                                                                                                    map.position,
                                                                                                    0.98),
                                                                      CHUNK_SIZE,
                                                                      4,
                                                                      true)
            if squadPosition then
                local squad = createSquad(squadPosition, surface, nil, true)

                squad.maxDistance = gaussianRandomRange(natives.expansionMaxDistance * 0.5,
                                                        natives.expansionMaxDistanceDerivation,
                                                        10,
                                                        natives.expansionMaxDistance)

                local scaledWaveSize = settlerWaveScaling(natives)
                map.formGroupCommand.group = squad.group
                map.formCommand.unit_count = scaledWaveSize
                local foundUnits = surface.set_multi_command(map.formCommand)
                if (foundUnits > 0) then
                    createSpawnerProxies(map, surface, chunk, foundUnits)
                    setChunkSettlerTick(map, squadPath, tick + natives.settlerCooldown)
                    natives.remainingSquads = natives.remainingSquads - 1
                    natives.points = natives.points - AI_SETTLER_COST
                    natives.groupNumberToSquad[squad.groupNumber] = squad
                else
                    if (squad.group.valid) then
                        squad.group.destroy()
                    end
                end
            end
        end
    end

    return ((natives.points - AI_SETTLER_COST) > 0) and (natives.remainingSquads > 0)
end

function aiAttackWave.formVengenceSquad(map, surface, chunk)
    local natives = map.natives
    if (mRandom() < natives.formSquadThreshold) then
        local squadPath, squadDirection = scoreNeighborsForFormation(getNeighborChunks(map, chunk.x, chunk.y),
                                                                     validUnitGroupLocation,
                                                                     scoreUnitGroupLocation,
                                                                     map)
        if (squadPath ~= -1) then
            local squadPosition = surface.find_non_colliding_position("chunk-scanner-squad-rampant",
                                                                      positionFromDirectionAndChunk(squadDirection,
                                                                                                    chunk,
                                                                                                    map.position,
                                                                                                    0.98),
                                                                      CHUNK_SIZE,
                                                                      4,
                                                                      true)
            if squadPosition then
                local squad = createSquad(squadPosition, surface)

                squad.rabid = mRandom() < 0.03

                local scaledWaveSize = attackWaveScaling(natives)
                map.formGroupCommand.group = squad.group
                map.formCommand.unit_count = scaledWaveSize
                local foundUnits = surface.set_multi_command(map.formCommand)
                if (foundUnits > 0) then
                    createSpawnerProxies(map, surface, chunk, foundUnits)
                    natives.groupNumberToSquad[squad.groupNumber] = squad
                    natives.points = natives.points - AI_VENGENCE_SQUAD_COST
                else
                    if (squad.group.valid) then
                        squad.group.destroy()
                    end
                end
            end
        end
    end

    return (natives.points - AI_VENGENCE_SQUAD_COST) > 0
end

function aiAttackWave.formSquads(map, surface, chunk, tick)
    local natives = map.natives
    if attackWaveValidCandidate(chunk, natives, map) and
        (mRandom() < natives.formSquadThreshold) and
        (natives.remainingSquads > 0)
    then
        local squadPath, squadDirection = scoreNeighborsForFormation(getNeighborChunks(map, chunk.x, chunk.y),
                                                                     validUnitGroupLocation,
                                                                     scoreUnitGroupLocation,
                                                                     map)
        if (squadPath ~= -1) then
            local squadPosition = surface.find_non_colliding_position("chunk-scanner-squad-rampant",
                                                                      positionFromDirectionAndChunk(squadDirection,
                                                                                                    chunk,
                                                                                                    map.position,
                                                                                                    0.98),
                                                                      CHUNK_SIZE,
                                                                      4,
                                                                      true)
            if squadPosition then
                local squad = createSquad(squadPosition, surface)

                squad.rabid = mRandom() < 0.03

                local scaledWaveSize = attackWaveScaling(natives)
                map.formGroupCommand.group = squad.group
                map.formCommand.unit_count = scaledWaveSize
                local foundUnits = surface.set_multi_command(map.formCommand)
                if (foundUnits > 0) then
                    createSpawnerProxies(map, surface, chunk, foundUnits)
                    natives.points = natives.points - AI_SQUAD_COST
                    natives.remainingSquads = natives.remainingSquads - 1
                    natives.groupNumberToSquad[squad.groupNumber] = squad
                    if tick and (natives.state == AI_STATE_AGGRESSIVE) then
                        natives.canAttackTick = randomTickEvent(tick,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                        return false
                    end
                else
                    if (squad.group.valid) then
                        squad.group.destroy()
                    end
                end
            end
        end
    end

    return ((natives.points - AI_SQUAD_COST) > 0) and (natives.remainingSquads > 0)
end


aiAttackWaveG = aiAttackWave
return aiAttackWave
