-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if MovementUtilsG then
    return MovementUtilsG
end
local MovementUtils = {}

--

local Universe

-- imports

local Constants = require("Constants")
local MapUtils = require("MapUtils")
local MathUtils = require("MathUtils")
local UnitGroupUtils = require("UnitGroupUtils")

-- Constants

local MAGIC_MAXIMUM_NUMBER = Constants.MAGIC_MAXIMUM_NUMBER

local SQUAD_SETTLING = Constants.SQUAD_SETTLING

-- imported functions

local calculateSettlerMaxDistance = UnitGroupUtils.calculateSettlerMaxDistance

local canMoveChunkDirection = MapUtils.canMoveChunkDirection
local getNeighborChunks = MapUtils.getNeighborChunks

local tableRemove = table.remove
local tableInsert = table.insert

local distortPosition = MathUtils.distortPosition

-- module code

function MovementUtils.findMovementPosition(surface, position)
    local pos = position
    pos = surface.find_non_colliding_position("behemoth-biter", pos, 10, 2, false)
    return pos
end

function MovementUtils.findMovementPositionEntity(entityName, surface, position)
    local pos = position
    pos = surface.find_non_colliding_position(entityName, pos, 5, 4, true)
    return pos
end

function MovementUtils.findMovementPositionDistort(surface, position)
    local pos = position
    pos = surface.find_non_colliding_position("behemoth-biter", pos, 10, 2, false)
    return distortPosition(pos, 8)
end

function MovementUtils.addMovementPenalty(squad, chunk)
    if (chunk == -1) then
        return
    end
    local penalties = squad.penalties
    local penaltyCount = #penalties
    for i=1,penaltyCount do
        local penalty = penalties[i]
        if (penalty.c.id == chunk.id) then
            penalty.v = penalty.v + 1
            if penalty.v >= 15 then
                if Universe.enabledMigration and
                    (Universe.builderCount < Universe.AI_MAX_BUILDER_COUNT) then
                    squad.settler = true
                    squad.originPosition.x = squad.group.position.x
                    squad.originPosition.y = squad.group.position.y
                    squad.maxDistance = calculateSettlerMaxDistance()

                    squad.status = SQUAD_SETTLING
                else
                    squad.group.destroy()
                end
            end
            return
        end
    end
    if (penaltyCount == 10) then
        tableRemove(penalties, 10)
    end
    tableInsert(penalties,
                1,
                { v = 1,
                  c = chunk })
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function MovementUtils.scoreNeighborsForAttack(map, chunk, neighborDirectionChunks, scoreFunction)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) then
            if (chunk == -1) or canMoveChunkDirection(map, x, chunk, neighborChunk) then
                local score = scoreFunction(map, neighborChunk)
                if (score > highestScore) then
                    highestScore = score
                    highestChunk = neighborChunk
                    highestDirection = x
                end
            end
        end
    end

    local nextHighestChunk = -1
    local nextHighestScore = highestScore
    local nextHighestDirection

    if (highestChunk ~= -1) then
        neighborDirectionChunks = getNeighborChunks(map, highestChunk.x, highestChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]
            if ((neighborChunk ~= -1) and ((chunk == -1) or (neighborChunk.id ~= chunk.id)) and
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

    return highestChunk, highestDirection, nextHighestChunk, nextHighestDirection
end


--[[
    Expects all neighbors adjacent to a chunk
--]]
function MovementUtils.scoreNeighborsForSettling(map, chunk, neighborDirectionChunks, scoreFunction)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection = 0

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) then
            if (chunk == -1) or canMoveChunkDirection(map, x, chunk, neighborChunk) then
                local score = scoreFunction(map, neighborChunk)
                if (score > highestScore) then
                    highestScore = score
                    highestChunk = neighborChunk
                    highestDirection = x
                end
            end
        end
    end

    if (chunk ~= -1) and (scoreFunction(map, chunk) > highestScore) then
        return chunk, 0, -1, 0
    end

    local nextHighestChunk = -1
    local nextHighestScore = highestScore
    local nextHighestDirection = 0

    if (highestChunk ~= -1) then
        neighborDirectionChunks = getNeighborChunks(map, highestChunk.x, highestChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]
            if ((neighborChunk ~= -1) and ((chunk == -1) or (neighborChunk.id ~= chunk.id)) and
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

    return highestChunk, highestDirection, nextHighestChunk, nextHighestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function MovementUtils.scoreNeighborsForResource(chunk, neighborDirectionChunks, validFunction, scoreFunction, map)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) and
            canMoveChunkDirection(map, x, chunk, neighborChunk) and
            validFunction(map, chunk, neighborChunk)
        then
            local score = scoreFunction(map, neighborChunk)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    if (chunk ~= -1) and (scoreFunction(map, chunk) > highestScore) then
        return -1, -1
    end

    return highestChunk, highestDirection
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
function MovementUtils.scoreNeighborsForRetreat(chunk, neighborDirectionChunks, scoreFunction, map)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) then
            if (chunk == -1) or canMoveChunkDirection(map, x, chunk, neighborChunk) then
                local score = scoreFunction(map, neighborChunk)
                if (score > highestScore) then
                    highestScore = score
                    highestChunk = neighborChunk
                    highestDirection = x
                end
            end
        end
    end

    local nextHighestChunk = -1
    local nextHighestScore = highestScore
    local nextHighestDirection

    if (highestChunk ~= -1) then
        neighborDirectionChunks = getNeighborChunks(map, highestChunk.x, highestChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]

            if ((neighborChunk ~= -1) and ((chunk == -1) or (neighborChunk.id ~= chunk.id)) and
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
function MovementUtils.scoreNeighborsForFormation(neighborChunks, validFunction, scoreFunction, map)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    for x=1,8 do
        local neighborChunk = neighborChunks[x]
        if (neighborChunk ~= -1) and validFunction(map, neighborChunk) then
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

function MovementUtils.init(universe)
    Universe = universe
end

MovementUtilsG = MovementUtils
return MovementUtils
