local neighborUtils = {}

-- imports

local constants = require("Constants")

-- constants

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

-- module code


--[[
    Expects all neighbors adjacent to a chunk
--]]
function neighborUtils.scoreNeighborsWithDirection(chunk, neighborDirectionChunks, validFunction, scoreFunction, squad, surface, position, scoreSelf) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection    
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]

        if neighborChunk and validFunction(x, chunk, neighborChunk) then
            position.x = neighborChunk.pX
            position.y = neighborChunk.pY
            local score = scoreFunction(position, squad, neighborChunk, surface)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    if scoreSelf and scoreFunction(position, squad, chunk, surface) > highestScore then
	return nil, -1
    end
    
    return highestChunk, highestDirection
end


--[[
    Expects all neighbors adjacent to a chunk
--]]
function neighborUtils.scoreNeighbors(chunk, neighborChunks, validFunction, scoreFunction, squad, surface, position, scoreSelf) 
    local highestChunk
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    for x=1,8 do
        local neighborChunk = neighborChunks[x]
        if neighborChunk and validFunction(x, chunk, neighborChunk) then
            position.x = neighborChunk.pX
            position.y = neighborChunk.pY
            local score = scoreFunction(position, squad, neighborChunk, surface)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
            end
        end
    end

    if scoreSelf and scoreFunction(position, squad, chunk, surface) > highestScore then
	return nil, -1
    end
    
    return highestChunk, highestScore
end

return neighborUtils
