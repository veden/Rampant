local utils = {}

local constants = require("Constants")

function utils.euclideanDistanceNamed(p1, p2)
    local xs = p1.x - p2.x
    local ys = p1.y - p2.y
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function utils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function utils.positionDirectionToChunkCorner(direction, chunk, position)
    -- local position = {}
    if (direction == 1) then
        position.x = chunk.pX + constants.QUARTER_CHUNK_SIZE 
        position.y = chunk.pY + constants.QUARTER_CHUNK_SIZE
    elseif (direction == 2) then
        position.x = chunk.pX + constants.HALF_CHUNK_SIZE
        position.y = chunk.pY + constants.QUARTER_CHUNK_SIZE
    elseif (direction == 3) then
        position.x = chunk.pX + constants.HALF_CHUNK_SIZE + constants.QUARTER_CHUNK_SIZE
        position.y = chunk.pY + constants.QUARTER_CHUNK_SIZE
    elseif (direction == 4) then
        position.x = chunk.pX + constants.QUARTER_CHUNK_SIZE
        position.y = chunk.pY + constants.HALF_CHUNK_SIZE
    elseif (direction == 5) then
        position.x = chunk.pX + constants.HALF_CHUNK_SIZE + constants.QUARTER_CHUNK_SIZE
        position.y = chunk.pY + constants.HALF_CHUNK_SIZE
    elseif (direction == 6) then
        position.x = chunk.pX + constants.QUARTER_CHUNK_SIZE
        position.y = chunk.pY + constants.HALF_CHUNK_SIZE + constants.QUARTER_CHUNK_SIZE
    elseif (direction == 7) then
        position.x = chunk.pX + constants.HALF_CHUNK_SIZE
        position.y = chunk.pY + constants.HALF_CHUNK_SIZE + constants.QUARTER_CHUNK_SIZE
    elseif (direction == 8) then
        position.x = chunk.pX + constants.HALF_CHUNK_SIZE + constants.QUARTER_CHUNK_SIZE
        position.y = chunk.pY + constants.HALF_CHUNK_SIZE + constants.QUARTER_CHUNK_SIZE
    end
    return position
end

return utils