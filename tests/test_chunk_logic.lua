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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>

--[[
    ChunkLogic.canMove equivalence: the lookup table vs the original chain.

    The oracle below is the ORIGINAL MapUtils.canMoveChunkDirection branch
    chain, transcribed verbatim (including the final `else` — an IMPASSABLE
    start can move anywhere the end is not impassable). It is an INDEPENDENT
    transcription from the table in libs/chunk_logic.lua: two independent
    readings of the same original code must agree over the entire input domain.

    The test is EXHAUSTIVE: 4 start classes x 8 directions x 4 end classes
    = 128 cases, every combination the function can be called with. There is no
    sampling — a wrong table entry or a wrong lookup key fails here.
]]

local TestCollection = require("framework")
local Constants = require("Constants")
local ChunkLogic = require("chunk_logic")

local CHUNK_IMPASSABLE = Constants.CHUNK_IMPASSABLE
local CHUNK_NORTH_SOUTH = Constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = Constants.CHUNK_EAST_WEST
local CHUNK_ALL_DIRECTIONS = Constants.CHUNK_ALL_DIRECTIONS

-- ORACLE: the original MapUtils.canMoveChunkDirection, verbatim.
local function oracleCanMove(direction, startPassable, endPassable)
    local canMove = false
    if (startPassable == CHUNK_ALL_DIRECTIONS) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 2) or (direction == 7) then
            canMove = ((endPassable == CHUNK_NORTH_SOUTH) or (endPassable == CHUNK_ALL_DIRECTIONS))
        elseif (direction == 4) or (direction == 5) then
            canMove = ((endPassable == CHUNK_EAST_WEST) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    elseif (startPassable == CHUNK_NORTH_SOUTH) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 2) or (direction == 7) then
            canMove = ((endPassable == CHUNK_NORTH_SOUTH) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    elseif (startPassable == CHUNK_EAST_WEST) then
        if ((direction == 1) or (direction == 3) or (direction == 6) or (direction == 8)) then
            canMove = (endPassable == CHUNK_ALL_DIRECTIONS)
        elseif (direction == 4) or (direction == 5) then
            canMove = ((endPassable == CHUNK_EAST_WEST) or (endPassable == CHUNK_ALL_DIRECTIONS))
        end
    else
        canMove = (endPassable ~= CHUNK_IMPASSABLE)
    end
    return canMove
end

local CLASSES = {
    CHUNK_IMPASSABLE,
    CHUNK_NORTH_SOUTH,
    CHUNK_EAST_WEST,
    CHUNK_ALL_DIRECTIONS,
}

local my_collection = TestCollection:new()

my_collection:add('canMove_exhaustive_equivalence', function()
    local checked = 0
    for _, start in ipairs(CLASSES) do
        for direction = 1, 8 do
            for _, finish in ipairs(CLASSES) do
                local expected = oracleCanMove(direction, start, finish)
                local actual = ChunkLogic.canMove(start, direction, finish)
                assert(actual == expected,
                    "canMove(" .. start .. "," .. direction .. "," .. finish ..
                    ") = " .. tostring(actual) ..
                    " but the oracle says " .. tostring(expected))
                checked = checked + 1
            end
        end
    end
    assert(checked == 128, "expected the full input domain (128), checked " .. checked)
end)

my_collection:add('canMove_table_covers_every_input', function()
    -- Every start class and direction must have an entry (a missing key would
    -- silently return false and diverge from the oracle for some end class).
    for _, start in ipairs(CLASSES) do
        for direction = 1, 8 do
            local row = ChunkLogic.CAN_MOVE[start]
            assert(row, "no table row for start class " .. start)
            local byDir = row[direction]
            local anyTrue = false
            if byDir then
                for _, finish in ipairs(CLASSES) do
                    if byDir[finish] then anyTrue = true end
                end
            end
            local oracleAnyTrue = false
            for _, finish in ipairs(CLASSES) do
                if oracleCanMove(direction, start, finish) then oracleAnyTrue = true end
            end
            assert(anyTrue == oracleAnyTrue,
                "table row presence mismatch at start=" .. start ..
                " dir=" .. direction)
        end
    end
end)

return my_collection
