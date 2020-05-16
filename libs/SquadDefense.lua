if aiDefenseG then
    return aiDefenseG
end
local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local movementUtils = require("MovementUtils")
local chunkPropetyUtils = require("ChunkPropertyUtils")

-- constants

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = constants.BASE_PHEROMONE

local PLAYER_PHEROMONE_MULTIPLER = constants.PLAYER_PHEROMONE_MULTIPLER

local SQUAD_RETREATING = constants.SQUAD_RETREATING

local INTERVAL_RETREAT = constants.INTERVAL_RETREAT

-- imported functions

local mRandom = math.random

local addSquadToChunk = chunkPropetyUtils.addSquadToChunk

local calculateKamikazeThreshold = unitGroupUtils.calculateKamikazeThreshold

local positionFromDirectionAndFlat = mapUtils.positionFromDirectionAndFlat
local getNeighborChunks = mapUtils.getNeighborChunks
local findNearbyRetreatingSquad = unitGroupUtils.findNearbyRetreatingSquad
local addMovementPenalty = movementUtils.addMovementPenalty
local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad
local scoreNeighborsForRetreat = movementUtils.scoreNeighborsForRetreat
local findMovementPosition = movementUtils.findMovementPosition

local getRetreatTick = chunkPropetyUtils.getRetreatTick
local getPlayerBaseGenerator = chunkPropetyUtils.getPlayerBaseGenerator
local setRetreatTick = chunkPropetyUtils.setRetreatTick
local getEnemyStructureCount = chunkPropetyUtils.getEnemyStructureCount

-- module code

local function scoreRetreatLocation(map, neighborChunk)
    return (-neighborChunk[BASE_PHEROMONE] +
                neighborChunk[MOVEMENT_PHEROMONE] +
                -(neighborChunk[PLAYER_PHEROMONE] * PLAYER_PHEROMONE_MULTIPLER) +
                -(getPlayerBaseGenerator(map, neighborChunk) * 1000))
end

function aiDefense.retreatUnits(chunk, position, squad, map, surface, tick, radius, artilleryBlast)
    if (tick - getRetreatTick(map, chunk) > INTERVAL_RETREAT) and
        ((getEnemyStructureCount(map, chunk) == 0) or artilleryBlast)
    then
        local performRetreat = false
        local enemiesToSquad = nil

        if not squad then
            enemiesToSquad = map.enemiesToSquad
            local unitCount = 0
            local units = surface.find_enemy_units(position, radius)
            for i=1,#units do
                local unit = units[i]
                if not unit.unit_group then
                    unitCount = unitCount + 1
                    enemiesToSquad[unitCount] = unit
                end
            end
            enemiesToSquad.len = unitCount
            if (mRandom() < calculateKamikazeThreshold(unitCount, map.natives)) then
                setRetreatTick(map, chunk, tick)
                return
            end
            performRetreat = unitCount > 6
        elseif squad.group and squad.group.valid and (squad.status ~= SQUAD_RETREATING) and not squad.kamikaze then
            performRetreat = #squad.group.members > 6
        end

        if performRetreat then
            setRetreatTick(map, chunk, tick)
            local exitPath,exitDirection,nextExitPath,nextExitDirection  = scoreNeighborsForRetreat(chunk,
                                                                                                    getNeighborChunks(map,
                                                                                                                      chunk.x,
                                                                                                                      chunk.y),
                                                                                                    scoreRetreatLocation,
                                                                                                    map)
            if (exitPath ~= -1) then
                local targetPosition = map.position
                local targetPosition2 = map.position2

                positionFromDirectionAndFlat(exitDirection, position, targetPosition)

                local retreatPosition = findMovementPosition(surface, targetPosition)

                if not retreatPosition then
                    return
                end

                if (nextExitPath ~= -1) then
                    positionFromDirectionAndFlat(nextExitDirection, retreatPosition, targetPosition2)

                    local retreatPosition2 = findMovementPosition(surface, targetPosition2)

                    if retreatPosition2 then
                        retreatPosition.x = retreatPosition2.x
                        retreatPosition.y = retreatPosition2.y
                    end
                end

                -- in order for units in a group attacking to retreat, we have to create a new group and give the command to join
                -- to each unit, this is the only way I have found to have snappy mid battle retreats even after 0.14.4

                local newSquad = findNearbyRetreatingSquad(map, exitPath)

                if not newSquad then
                    newSquad = createSquad(retreatPosition, surface)
                    local squads = map.natives.squads
                    squads.len = squads.len+1
                    squads[squads.len] = newSquad
                end

                if newSquad then
                    newSquad.status = SQUAD_RETREATING
                    newSquad.cycles = 13

                    local cmd = map.retreatCommand
                    cmd.group = newSquad.group
                    if enemiesToSquad then
                        membersToSquad(cmd, enemiesToSquad.len, enemiesToSquad, artilleryBlast)
                    else
                        membersToSquad(cmd, #squad.group.members, squad.group.members, true)
                        if squad.rabid then
                            newSquad.rabid = true
                        end
                    end

                    if not newSquad.rabid then
                        newSquad.frenzy = true
                        local squadPosition = newSquad.group.position
                        newSquad.frenzyPosition.x = squadPosition.x
                        newSquad.frenzyPosition.y = squadPosition.y
                    end
                    addMovementPenalty(newSquad, chunk)
                    addSquadToChunk(map, chunk, newSquad)
                end
            end
        end
    end
end

aiDefenseG = aiDefense
return aiDefense
