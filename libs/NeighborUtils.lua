local neighborUtils = {}

-- imports

local mapUtils = require("MapUtils")

local constants = require("Constants")

-- constants

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE

-- imported functions

local canMoveChunkDirection = mapUtils.canMoveChunkDirection

-- module code


--[[
    Expects all neighbors adjacent to a chunk
--]]
function neighborUtils.scoreNeighborsForAttack(chunk, neighborDirectionChunks, scoreFunction, squad) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection    
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if neighborChunk and canMoveChunkDirection(x, chunk, neighborChunk) then
            local score = scoreFunction(squad, neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    if scoreFunction(squad, chunk) > highestScore then
	return nil, -1
    end
    
    return highestChunk, highestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function neighborUtils.scoreNeighborsForResource(chunk, neighborDirectionChunks, scoreFunction, squad, threshold) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection    
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if neighborChunk and canMoveChunkDirection(x, chunk, neighborChunk) and (neighborChunk[RESOURCE_PHEROMONE] > threshold) then
            local score = scoreFunction(squad, neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    if scoreFunction(squad, chunk) > highestScore then
	return nil, -1
    end
    
    return highestChunk, highestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function neighborUtils.scoreNeighborsForRetreat(chunk, neighborDirectionChunks, scoreFunction) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection    
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if neighborChunk and canMoveChunkDirection(x, chunk, neighborChunk) then
            local score = scoreFunction(neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end
    
    return highestChunk, highestDirection
end


--[[
    Expects all neighbors adjacent to a chunk
--]]
function neighborUtils.scoreNeighborsForFormation(neighborChunks, validFunction, scoreFunction) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborChunks[x]
        if neighborChunk and validFunction(neighborChunk) then
            local score = scoreFunction(neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
		highestDirection = x
            end
        end
    end

    return highestChunk, highestDirection
end

return neighborUtils
