local aiDefense = {}

local factorio_defined = defines
local constants = require("Constants")
local mapUtils = require("MapUtils")
local utils = require("Utils")
local unitGroupUtils = require("UnitGroupUtils")

local retreatFilter = {[constants.SQUAD_RETREATING] = true}
local retreatNeighbors = {1,2,3,4,5,6,7,8} -- used to minimize garbage generation
local retreatPosition = {x=0, y=0} -- used to minimize garbage generation

function aiDefense.retreatUnits(position, squad, regionMap, surface, natives)
    local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
    
    local chunk = mapUtils.getChunkByPosition(regionMap, position.x, position.y)
    if (chunk ~= nil) and (chunk[DEATH_PHEROMONE] > (game.evolution_factor * constants.RETREAT_LEVEL_DEATH_PHEROMONE)) then -- TODO sliding scale of death based on evolution
        local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
        local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
        local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
        local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE
    
        local performRetreat = false
        local enemiesToSquad
    
        if (squad == nil) then
            enemiesToSquad = surface.find_enemy_units(position, 15)
            if (#enemiesToSquad > 0) then
                performRetreat = true
            end
        elseif squad.group.valid and (squad.status ~= constants.SQUAD_RETREATING) and (squad.status ~= constants.SQUAD_SUICIDE_HUNT) and (squad.status ~= constants.SQUAD_SUICIDE_RAID) then
            if (#squad.group.members ~= 0) then
                performRetreat = true
            end
        end
                
        if performRetreat then
            mapUtils.getNeighborChunks(regionMap, 
                                       chunk.cX, 
                                       chunk.cY, 
                                       retreatNeighbors)
            local exitPath
            local exitScore = constants.MAGIC_MAXIMUM_NUMBER
            local exitDirection
            for i=1, 4 do
                local neighborChunk = retreatNeighbors[i]
                if (neighborChunk ~= nil) then
                    retreatPosition.x = neighborChunk.pX
                    retreatPosition.y = neighborChunk.pY
                    
                    local dangerScore = neighborChunk[DEATH_PHEROMONE] + surface.get_pollution(retreatPosition) + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[PLAYER_DEFENSE_PHEROMONE] - neighborChunk[ENEMY_BASE_PHEROMONE] + (neighborChunk[ENEMY_BASE_GENERATOR] * 5)
                    if (dangerScore < exitScore) then
                        exitScore = dangerScore
                        exitPath = neighborChunk
                        exitDirection = i
                    end
                end
            end
            
            utils.positionDirectionToChunkCorner(exitDirection, exitPath, retreatPosition)
            -- in order for units in a group attacking to retreat, we have to create a new group and give the command to join
            -- to each unit
            -- retreatPosition.x = exitPath.pX + constants.HALF_CHUNK_SIZE
            -- retreatPosition.y = exitPath.pY + constants.HALF_CHUNK_SIZE

            if (squad ~= nil) and (squad.cX ~= nil) then
                local chunk = mapUtils.getChunkByIndex(regionMap, squad.cX, squad.cY)
                chunk[constants.DEATH_PHEROMONE] = chunk[constants.DEATH_PHEROMONE] + constants.DEATH_PHEROMONE_GENERATOR_AMOUNT
            end
            
            local newSquad = unitGroupUtils.findNearBySquad(natives, 
                                                            retreatPosition,
                                                            constants.HALF_CHUNK_SIZE,
                                                            retreatFilter)
            
            if (newSquad == nil) then
                newSquad = unitGroupUtils.createSquad(retreatPosition, surface, natives)
                newSquad.status = constants.SQUAD_RETREATING
                newSquad.cycles = 4
            end
            if (enemiesToSquad ~= nil) then
                unitGroupUtils.membersToSquad(newSquad, enemiesToSquad, false, factorio_defined.distraction.none)
            else
                unitGroupUtils.membersToSquad(newSquad, squad.group.members, true, factorio_defined.distraction.none)
            end
        end
    end
end

return aiDefense