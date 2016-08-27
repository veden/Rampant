local neighborUtils = {}

-- imports

local constants = require("Constants")

-- constants

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

-- module code

function neighborUtils.scoreNeighborsWithDirection(chunk, neighborDirectionChunks, validFunction, scoreFunction, squad, surface) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection    
    local position = {x=0, y=0}
    for x=1,#neighborDirectionChunks do
        local neighborDirectionChunk = neighborDirectionChunks[x]
        local neighborChunk = neighborDirectionChunk.c
        if (neighborChunk ~= nil) and validFunction(x, chunk, neighborChunk) then
            position.x = neighborChunk.pX
            position.y = neighborChunk.pY
            local score = scoreFunction(position, squad, neighborChunk, surface)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = neighborDirectionChunk.d
            end
        end
    end
    
    return highestChunk, highestDirection
end

function neighborUtils.scoreNeighbors(chunk, neighborChunks, validFunction, scoreFunction, squad, surface) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local position = {x=0, y=0}
    for x=1,#neighborChunks do
        local neighborChunk = neighborChunks[x]
        if (neighborChunk ~= nil) and validFunction(x, chunk, neighborChunk) then
            position.x = neighborChunk.pX
            position.y = neighborChunk.pY
            local score = scoreFunction(position, squad, neighborChunk, surface)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
            end
        end
    end
    
    return highestChunk, highestScore
end

return neighborUtils