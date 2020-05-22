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

-- imported functions

local canMoveChunkDirection = mapUtils.canMoveChunkDirection
local getNeighborChunks = mapUtils.getNeighborChunks

-- local recycleBiters = unitGroupUtils.recycleBiters

local tableRemove = table.remove
local tableInsert = table.insert

local distortPosition = mathUtils.distortPosition

-- module code

function movementUtils.findMovementPosition(surface, position)
    local pos = position
    -- if not surface.can_place_entity({name="chunk-scanner-squad-movement-rampant", position=pos}) then
        pos = surface.find_non_colliding_position("chunk-scanner-squad-movement-rampant", pos, 10, 2, false)
    -- end
    return pos
end

function movementUtils.findMovementPositionEntity(entityName, surface, position)
    local pos = position
    -- if not surface.can_place_entity({name=entityName, position=pos}) then
        pos = surface.find_non_colliding_position(entityName, pos, 5, 4, true)
    -- end
    return pos
end

function movementUtils.findMovementPositionDistort(surface, position)
    local pos = position
    -- if not surface.can_place_entity({name="chunk-scanner-squad-movement-rampant", position=pos}) then
        pos = surface.find_non_colliding_position("chunk-scanner-squad-movement-rampant", pos, 10, 2, false)
    -- end
    return distortPosition(pos, 8)
end

function movementUtils.addMovementPenalty(squad, chunk)
    if (chunk == -1) then
        return
    end
    local penalties = squad.penalties
    for i=1,#penalties do
        local penalty = penalties[i]
        if (penalty.c == chunk) then
            penalty.v = (2 * penalty.v) + MOVEMENT_PENALTY_AMOUNT
            if (penalty.v >= MOVEMENT_PENALTY_AMOUNT * 5) then
                squad.kamikaze = true
            end
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
function movementUtils.scoreNeighborsForAttack(map, chunk, neighborDirectionChunks, scoreFunction, squad)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection

    local nextHighestChunk = -1
    local nextHighestScore = -MAGIC_MAXIMUM_NUMBER
    local nextHighestDirection

    local natives = map.natives

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]

        if (neighborChunk ~= -1) then
            if canMoveChunkDirection(map, x, chunk, neighborChunk) or (chunk == -1) then
                local score = scoreFunction(natives, squad, neighborChunk)
                if (score > highestScore) then
                    highestScore = score
                    highestChunk = neighborChunk
                    highestDirection = x
                end
            end
        end
    end

    if (highestChunk ~= -1) then
        neighborDirectionChunks = getNeighborChunks(map, highestChunk.x, highestChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]

            if ((neighborChunk ~= -1) and (neighborChunk ~= chunk) and
                canMoveChunkDirection(map, x, highestChunk, neighborChunk)) then
                local score = scoreFunction(natives, squad, neighborChunk)
                if (score > nextHighestScore) then
                    nextHighestScore = score
                    nextHighestChunk = neighborChunk
                    nextHighestDirection = x
                end
            end
        end
    end

    return highestChunk, highestDirection, nextHighestChunk, nextHighestDirection
end


--[[
    Expects all neighbors adjacent to a chunk
--]]
function movementUtils.scoreNeighborsForSettling(map, chunk, neighborDirectionChunks, scoreFunction, squad)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection = 0

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) then
            if canMoveChunkDirection(map, x, chunk, neighborChunk) or (chunk == -1) then
                local score = scoreFunction(squad, neighborChunk)
                if (score > highestScore) then
                    highestScore = score
                    highestChunk = neighborChunk
                    highestDirection = x
                end
            end
        end
    end

    if (chunk ~= -1) and (scoreFunction(squad, chunk) > highestScore) then
        return chunk, 0, -1, 0
    end

    local nextHighestChunk = -1
    local nextHighestScore = -MAGIC_MAXIMUM_NUMBER
    local nextHighestDirection = 0

    if (highestChunk ~= -1) then
        neighborDirectionChunks = getNeighborChunks(map, highestChunk.x, highestChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]
            if ((neighborChunk ~= -1) and (neighborChunk ~= chunk) and
                canMoveChunkDirection(map, x, highestChunk, neighborChunk)) then
                local score = scoreFunction(squad, neighborChunk)
                if (score > nextHighestScore) then
                    nextHighestScore = score
                    nextHighestChunk = neighborChunk
                    nextHighestDirection = x
                end
            end
        end
    end

    return highestChunk, highestDirection, nextHighestChunk, nextHighestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function movementUtils.scoreNeighborsForResource(chunk, neighborDirectionChunks, validFunction, scoreFunction, map)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) and canMoveChunkDirection(map, x, chunk, neighborChunk) and validFunction(map, chunk, neighborChunk) then
            local score = scoreFunction(neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    if (chunk ~= -1) and (scoreFunction(chunk) > highestScore) then
        return -1, -1
    end

    return highestChunk, highestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function movementUtils.scoreNeighborsForRetreat(chunk, neighborDirectionChunks, scoreFunction, map)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection

    local nextHighestChunk = -1
    local nextHighestScore = -MAGIC_MAXIMUM_NUMBER
    local nextHighestDirection

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) then
            if canMoveChunkDirection(map, x, chunk, neighborChunk) or (chunk == -1) then
                local score = scoreFunction(map, neighborChunk)
                if (score > highestScore) then
                    highestScore = score
                    highestChunk = neighborChunk
                    highestDirection = x
                end
            end
        end
    end

    if (highestChunk ~= -1) then
        neighborDirectionChunks = getNeighborChunks(map, highestChunk.x, highestChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]

            if ((neighborChunk ~= -1) and (neighborChunk ~= chunk) and
                canMoveChunkDirection(map, x, highestChunk, neighborChunk)) then
                local score = scoreFunction(map, neighborChunk)
                if (score > nextHighestScore) then
                    nextHighestScore = score
                    nextHighestChunk = neighborChunk
                    nextHighestDirection = x
                end
            end
        end
    end

    if (nextHighestChunk == nil) then
        nextHighestChunk = -1
    end

    return highestChunk, highestDirection, nextHighestChunk, nextHighestDirection
end


--[[
    Expects all neighbors adjacent to a chunk
--]]
function movementUtils.scoreNeighborsForFormation(neighborChunks, validFunction, scoreFunction, map)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborChunks[x]
        if (neighborChunk ~= -1) and validFunction(map, neighborChunk) then
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
