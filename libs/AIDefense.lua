local aiDefense = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")

-- constants

local DISTRACTION_NONE = defines.distraction.none

local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE

local DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT

local RETREAT_DEATH_PHEROMONE_LEVEL = constants.RETREAT_DEATH_PHEROMONE_LEVEL

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_SUICIDE_HUNT = constants.SQUAD_SUICIDE_HUNT
local SQUAD_SUICIDE_RAID = constants.SQUAD_SUICIDE_RAID

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

-- imported functions

local getChunkByPosition = mapUtils.getChunkByPosition
local getCardinalChunks = mapUtils.getCardinalChunks
local positionDirectionToChunkCornerCardinal = mapUtils.positionDirectionToChunkCornerCardinal
local getChunkByIndex = mapUtils.getChunkByIndex
local findNearBySquad = unitGroupUtils.findNearBySquad
local createSquad = unitGroupUtils.createSquad
local membersToSquad = unitGroupUtils.membersToSquad

-- premade tables

local retreatFilter = {[SQUAD_RETREATING] = true}

-- module code

function aiDefense.retreatUnits(position, squad, regionMap, surface, natives)
    local chunk = getChunkByPosition(regionMap, position.x, position.y)
    if (chunk ~= nil) and (chunk[DEATH_PHEROMONE] > (game.evolution_factor * RETREAT_DEATH_PHEROMONE_LEVEL)) then -- TODO sliding scale of death based on evolution
        local performRetreat = false
        local enemiesToSquad
    
        if (squad == nil) then
            enemiesToSquad = surface.find_enemy_units(position, 15)
            if (#enemiesToSquad > 0) then
                performRetreat = true
            end
        elseif squad.group.valid and (squad.status ~= SQUAD_RETREATING) and (squad.status ~= SQUAD_SUICIDE_HUNT) and (squad.status ~= SQUAD_SUICIDE_RAID) then
            if (#squad.group.members ~= 0) then
                performRetreat = true
            end
        end
                
        if performRetreat then
            local retreatNeighbors = getCardinalChunks(regionMap, 
                                                       chunk.cX, 
                                                       chunk.cY)
            local exitPath
            local exitScore = -MAGIC_MAXIMUM_NUMBER
            local exitDirection
            local retreatPosition = {x=0, y=0}
            for i=1, #retreatNeighbors do
                local neighborChunk = retreatNeighbors[i]
                if (neighborChunk ~= nil) then
                    retreatPosition.x = neighborChunk.pX
                    retreatPosition.y = neighborChunk.pY
                    
                    local dangerScore = neighborChunk[DEATH_PHEROMONE] + surface.get_pollution(retreatPosition) + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[PLAYER_DEFENSE_PHEROMONE] + (neighborChunk[ENEMY_BASE_GENERATOR] * 10)
                    local avoidScore = neighborChunk[ENEMY_BASE_PHEROMONE] 
                    local score = avoidScore - dangerScore
                    if (exitScore < score) then
                        exitScore = score
                        exitPath = neighborChunk
                        exitDirection = i
                    end
                end
            end
            
            retreatPosition = positionDirectionToChunkCornerCardinal(exitDirection, exitPath)
            -- in order for units in a group attacking to retreat, we have to create a new group and give the command to join
            -- to each unit
            -- retreatPosition.x = exitPath.pX + constants.HALF_CHUNK_SIZE
            -- retreatPosition.y = exitPath.pY + constants.HALF_CHUNK_SIZE

            if (squad ~= nil) and (squad.cX ~= nil) then
                local chunk = getChunkByIndex(regionMap, squad.cX, squad.cY)
                chunk[DEATH_PHEROMONE] = chunk[DEATH_PHEROMONE] + DEATH_PHEROMONE_GENERATOR_AMOUNT
            end
            
            local newSquad = findNearBySquad(natives, 
                                             retreatPosition,
                                             HALF_CHUNK_SIZE,
                                             retreatFilter)
            
            if (newSquad == nil) then
                newSquad = createSquad(retreatPosition, surface, natives)
                newSquad.status = SQUAD_RETREATING
                newSquad.cycles = 4
            end
            if (enemiesToSquad ~= nil) then
                membersToSquad(newSquad, enemiesToSquad, false, DISTRACTION_NONE)
            else
                -- newSquad.penalties = squad.penalties
                membersToSquad(newSquad, squad.group.members, true, DISTRACTION_NONE)
            end
        end
    end
end

return aiDefense