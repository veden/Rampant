if movementUtilsG then
    return movementUtilsG
end
local movementUtils = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local mathUtils = require("MathUtils")

-- constants

local MOVEMENT_PENALTY_AMOUNT = constants.MOVEMENT_PENALTY_AMOUNT

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local canMoveChunkDirection = mapUtils.canMoveChunkDirection

-- local recycleBiters = unitGroupUtils.recycleBiters

local tableRemove = table.remove
local tableInsert = table.insert

local distortPosition = mathUtils.distortPosition

-- module code

function movementUtils.findMovementPosition(surface, position, distort)
    local pos = position
    if not surface.can_place_entity({name="chunk-scanner-squad-movement-rampant", position=position}) then
	pos = surface.find_non_colliding_position("chunk-scanner-squad-movement-rampant", position, 15, 2, true)
    end
    return (distort and distortPosition(pos)) or pos
end

function movementUtils.addMovementPenalty(units, chunk)
    local penalties = units.penalties
    for i=1,#penalties do
        local penalty = penalties[i]
        if (penalty.c == chunk) then
            penalty.v = (2 * penalty.v) + MOVEMENT_PENALTY_AMOUNT
            return
        end
    end
    if (#penalties == 7) then
        tableRemove(penalties, 7)
    end
    tableInsert(penalties,
		1,
		{ v = MOVEMENT_PENALTY_AMOUNT,
		  c = chunk })
end

function movementUtils.lookupMovementPenalty(squad, chunk)
    local penalties = squad.penalties
    for i=1,#penalties do
        local penalty = penalties[i]
        if (penalty.c == chunk) then
            return penalty.v
        end
    end
    return 0
end


--[[
    Expects all neighbors adjacent to a chunk
--]]
function movementUtils.scoreNeighborsForAttack(map, natives, chunk, neighborDirectionChunks, scoreFunction, squad)
    local highestChunk = SENTINEL_IMPASSABLE_CHUNK
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]

        if (neighborChunk ~= SENTINEL_IMPASSABLE_CHUNK) and canMoveChunkDirection(map, x, chunk, neighborChunk) then
            local score = scoreFunction(natives, squad, neighborChunk)
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
function movementUtils.scoreNeighborsForSettling(map, chunk, neighborDirectionChunks, scoreFunction, squad)
    local highestChunk = SENTINEL_IMPASSABLE_CHUNK
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= SENTINEL_IMPASSABLE_CHUNK) and canMoveChunkDirection(map, x, chunk, neighborChunk) then
            local score = scoreFunction(squad, neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) and (scoreFunction(squad, chunk) > highestScore) then
	return chunk, -1
    end

    return highestChunk, highestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function movementUtils.scoreNeighborsForResource(chunk, neighborDirectionChunks, validFunction, scoreFunction, map)
    local highestChunk = SENTINEL_IMPASSABLE_CHUNK
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= SENTINEL_IMPASSABLE_CHUNK) and canMoveChunkDirection(map, x, chunk, neighborChunk) and validFunction(map, chunk, neighborChunk) then
            local score = scoreFunction(neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) and (scoreFunction(chunk) > highestScore) then
	return SENTINEL_IMPASSABLE_CHUNK, -1
    end

    return highestChunk, highestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function movementUtils.scoreNeighborsForRetreat(chunk, neighborDirectionChunks, scoreFunction, map)
    local highestChunk = SENTINEL_IMPASSABLE_CHUNK
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= SENTINEL_IMPASSABLE_CHUNK) and canMoveChunkDirection(map, x, chunk, neighborChunk) then
            local score = scoreFunction(map, neighborChunk)
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
function movementUtils.scoreNeighborsForFormation(neighborChunks, validFunction, scoreFunction, map)
    local highestChunk = SENTINEL_IMPASSABLE_CHUNK
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborChunks[x]
        if (neighborChunk ~= SENTINEL_IMPASSABLE_CHUNK) and validFunction(map, neighborChunk) then
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

movementUtilsG = movementUtils
return movementUtils
