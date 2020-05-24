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
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

local AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION = constants.AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION
local AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION = constants.AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION

local AI_MAX_BUILDER_COUNT = constants.AI_MAX_BUILDER_COUNT

local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_SETTLER_COST = constants.AI_SETTLER_COST
local AI_MAX_SQUAD_COUNT = constants.AI_MAX_SQUAD_COUNT
local AI_VENGENCE_SQUAD_COST = constants.AI_VENGENCE_SQUAD_COST
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE

local COOLDOWN_RALLY = constants.COOLDOWN_RALLY

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS

local CHUNK_SIZE = constants.CHUNK_SIZE

local RALLY_CRY_DISTANCE = constants.RALLY_CRY_DISTANCE
local SETTLER_DISTANCE = constants.SETTLER_DISTANCE

local RESOURCE_MINIMUM_FORMATION_DELTA = constants.RESOURCE_MINIMUM_FORMATION_DELTA

local AI_STATE_SIEGE = constants.AI_STATE_SIEGE

local AI_STATE_RAIDING = constants.AI_STATE_RAIDING

-- imported functions

local randomTickEvent = mathUtils.randomTickEvent

local calculateKamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold

local mRandom = math.random

local positionFromDirectionAndChunk = mapUtils.positionFromDirectionAndChunk

local getPassable = chunkPropertyUtils.getPassable
local getNestCount = chunkPropertyUtils.getNestCount
local getRaidNestActiveness = chunkPropertyUtils.getRaidNestActiveness
local getNestActiveness = chunkPropertyUtils.getNestActiveness
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
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator

-- module code

local function attackWaveValidCandidate(chunk, natives, map)
    local isValid = getNestActiveness(map, chunk)
    if natives.state == AI_STATE_RAIDING then
        isValid = isValid + getRaidNestActiveness(map, chunk)
    end
    return (isValid > 0)
end

local function scoreSettlerLocation(map, neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE] +
        -getDeathGenerator(map, neighborChunk) +
        -neighborChunk[PLAYER_PHEROMONE]
end

local function scoreSiegeSettlerLocation(map, neighborChunk)
    return neighborChunk[RESOURCE_PHEROMONE] +
        neighborChunk[BASE_PHEROMONE] +
        -getDeathGenerator(map, neighborChunk) +
        -neighborChunk[PLAYER_PHEROMONE]
end

local function scoreUnitGroupLocation(map, neighborChunk)
    return neighborChunk[PLAYER_PHEROMONE] +
        -getDeathGenerator(map, neighborChunk) +
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

local function visitPattern(o, cX, cY, distance)
    local startX
    local endX
    local stepX
    local startY
    local endY
    local stepY
    if (o == 0) then
        startX = cX - RALLY_CRY_DISTANCE
        endX = cX + RALLY_CRY_DISTANCE
        stepX = 32
        startY = cY - RALLY_CRY_DISTANCE
        endY = cY + RALLY_CRY_DISTANCE
        stepY = 32
    elseif (o == 1) then
        startX = cX + RALLY_CRY_DISTANCE
        endX = cX - RALLY_CRY_DISTANCE
        stepX = -32
        startY = cY + RALLY_CRY_DISTANCE
        endY = cY - RALLY_CRY_DISTANCE
        stepY = -32
    elseif (o == 2) then
        startX = cX - RALLY_CRY_DISTANCE
        endX = cX + RALLY_CRY_DISTANCE
        stepX = 32
        startY = cY + RALLY_CRY_DISTANCE
        endY = cY - RALLY_CRY_DISTANCE
        stepY = -32
    elseif (o == 3) then
        startX = cX + RALLY_CRY_DISTANCE
        endX = cX - RALLY_CRY_DISTANCE
        stepX = -32
        startY = cY - RALLY_CRY_DISTANCE
        endY = cY + RALLY_CRY_DISTANCE
        stepY = 32
    end
    return startX, endX, stepX, startY, endY, stepY
end

function aiAttackWave.rallyUnits(chunk, map, surface, tick)
    if ((tick - getRallyTick(map, chunk) > COOLDOWN_RALLY) and (map.natives.points >= AI_VENGENCE_SQUAD_COST)) then
        setRallyTick(map, chunk, tick)
        local cX = chunk.x
        local cY = chunk.y
        local startX, endX, stepX, startY, endY, stepY = visitPattern(tick % 4, cX, cY, RALLY_CRY_DISTANCE)
        local vengenceQueue = map.natives.vengenceQueue
        for x=startX, endX, stepX do
            for y=startY, endY, stepY do
                if (x ~= cX) and (y ~= cY) then
                    local rallyChunk = getChunkByXY(map, x, y)
                    if (rallyChunk ~= -1) and (getNestCount(map, rallyChunk) > 0) then
                        local count = vengenceQueue[rallyChunk]
                        if not count then
                            count = 0
                            vengenceQueue[rallyChunk] = count
                        end
                        vengenceQueue[rallyChunk] = count + 1
                    end
                end
            end
        end

        return true
    end
end

function aiAttackWave.formSettlers(map, surface, chunk, tick)

    local natives = map.natives
    if (natives.builderCount < AI_MAX_BUILDER_COUNT) and
        (mRandom() < natives.formSquadThreshold) and
        ((natives.points - AI_SETTLER_COST) > 0)
    then
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
                local squad = createSquad(squadPosition, surface, nil, true)

                squad.maxDistance = gaussianRandomRange(natives.expansionMaxDistance * 0.5,
                                                        natives.expansionMaxDistanceDerivation,
                                                        10,
                                                        natives.expansionMaxDistance)


                local scaledWaveSize = settlerWaveScaling(natives)
                map.formGroupCommand.group = squad.group
                local group = squad.group
                map.formCommand.unit_count = scaledWaveSize
                local foundUnits = surface.set_multi_command(map.formCommand)
                if (foundUnits > 0) then
                    squad.kamikaze = mRandom() < calculateKamikazeThreshold(foundUnits, natives)                    
                    natives.builderCount = natives.builderCount + 1
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
end

function aiAttackWave.formVengenceSquad(map, surface, chunk)
    local natives = map.natives
    if (natives.squadCount < AI_MAX_SQUAD_COUNT) and
        (mRandom() < natives.formSquadThreshold) and
        ((natives.points - AI_VENGENCE_SQUAD_COST) > 0)
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
                local group = squad.group
                map.formCommand.unit_count = scaledWaveSize
                local foundUnits = surface.set_multi_command(map.formCommand)
                if (foundUnits > 0) then
                    squad.kamikaze = mRandom() < calculateKamikazeThreshold(foundUnits, natives)                    
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
end

function aiAttackWave.formSquads(map, surface, chunk, tick)
    local natives = map.natives
    if (natives.squadCount < AI_MAX_SQUAD_COUNT) and
        attackWaveValidCandidate(chunk, natives, map) and
        (mRandom() < natives.formSquadThreshold) and
        ((natives.points - AI_SQUAD_COST) > 0)
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
                local group = squad.group
                map.formCommand.unit_count = scaledWaveSize
                local foundUnits = surface.set_multi_command(map.formCommand)
                if (foundUnits > 0) then
                    squad.kamikaze = mRandom() < calculateKamikazeThreshold(foundUnits, natives)
                    natives.points = natives.points - AI_SQUAD_COST
                    natives.squadCount = natives.squadCount + 1
                    natives.groupNumberToSquad[squad.groupNumber] = squad
                    if tick and (natives.state == AI_STATE_AGGRESSIVE) then
                        natives.canAttackTick = randomTickEvent(tick,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION,
                                                                AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION)
                    end
                else
                    if (squad.group.valid) then
                        squad.group.destroy()
                    end
                end
            end
        end
    end
end


aiAttackWaveG = aiAttackWave
return aiAttackWave
