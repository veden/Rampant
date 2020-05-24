if aiDefenseG then
    return aiDefenseG
end
local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local chunkUtils = require("ChunkUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local SQUAD_RETREATING = constants.SQUAD_RETREATING

local COOLDOWN_RETREAT = constants.COOLDOWN_RETREAT

-- imported functions

local mRandom = math.random

local addSquadToChunk = chunkPropertyUtils.addSquadToChunk

local calculateKamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold

local positionFromDirectionAndFlat = mapUtils.positionFromDirectionAndFlat
local getNeighborChunks = mapUtils.getNeighborChunks
local findNearbyRetreatingSquad = unitGroupUtils.findNearbyRetreatingSquad
local addMovementPenalty = movementUtils.addMovementPenalty
local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad
local scoreNeighborsForRetreat = movementUtils.scoreNeighborsForRetreat
local findMovementPosition = movementUtils.findMovementPosition

local getSquadsOnChunk = chunkPropertyUtils.getSquadsOnChunk
local getRetreatTick = chunkPropertyUtils.getRetreatTick
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local setRetreatTick = chunkPropertyUtils.setRetreatTick
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount

-- module code

local function scoreRetreatLocation(map, neighborChunk)
    return (-neighborChunk[BASE_PHEROMONE] +
                -getDeathGenerator(map, neighborChunk) +
                -(neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER) +
                -(getPlayerBaseGenerator(map, neighborChunk) * 1000))
end

function aiDefense.retreatUnits(chunk, cause, map, surface, tick, radius)
    if (tick - getRetreatTick(map, chunk) > COOLDOWN_RETREAT) and (getEnemyStructureCount(map, chunk) == 0) then

        setRetreatTick(map, chunk, tick)
        local exitPath,exitDirection,nextExitPath,nextExitDirection  = scoreNeighborsForRetreat(chunk,
                                                                                                getNeighborChunks(map,
                                                                                                                  chunk.x,
                                                                                                                  chunk.y),
                                                                                                scoreRetreatLocation,
                                                                                                map)
        local position = map.position
        local targetPosition2 = map.position2
        local retreatPosition
        position.x = chunk.x + 16
        position.y = chunk.y + 16
        if (exitPath == -1) then
            return
        elseif (nextExitPath ~= -1) then
            positionFromDirectionAndFlat(exitDirection, position, targetPosition2)
            positionFromDirectionAndFlat(nextExitDirection, targetPosition2, position)
            retreatPosition = findMovementPosition(surface, position)
            exitPath = nextExitPath
        else
            positionFromDirectionAndFlat(exitDirection, position, targetPosition2)
            retreatPosition = findMovementPosition(surface, targetPosition2)
        end

        if retreatPosition then
            position.x = retreatPosition.x
            position.y = retreatPosition.y
        else
            return
        end
        
        local newSquad = findNearbyRetreatingSquad(map, exitPath)
        local created = false

        if not newSquad then
            created = true
            newSquad = createSquad(position, surface)
        end

        map.fleeCommand.from = cause
        map.retreatCommand.group = newSquad.group

        map.formRetreatCommand.unit_search_distance = radius

        local foundUnits = surface.set_multi_command(map.formRetreatCommand)
        
        if (foundUnits == 0) then
            if created then
                newSquad.group.destroy()
            end
            return
        end

        if created then
            local natives = map.natives
            natives.groupNumberToSquad[newSquad.groupNumber] = newSquad
            natives.squadCount = natives.squadCount + 1
        end

        newSquad.status = SQUAD_RETREATING

        addSquadToChunk(map, chunk, newSquad)

        newSquad.frenzy = true
        local squadPosition = newSquad.group.position
        newSquad.frenzyPosition.x = squadPosition.x
        newSquad.frenzyPosition.y = squadPosition.y
    end
end

aiDefenseG = aiDefense
return aiDefense
