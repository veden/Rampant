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
    Chunk Logic - pure chunk-movement decisions.

    Pure module. Zero Factorio dependencies.

    canMove is a LOOKUP TABLE transcription of the original branch chain
    (MapUtils.canMoveChunkDirection). The chain was ~12 comparisons per call,
    called 8x per chunk in the pheromone loop — the hottest path in the
    end-game profile.

    Correctness: the table is verified EXHAUSTIVELY against an independent
    oracle transcription of the original chain over the entire input domain
    (4 start classes x 8 directions x 4 end classes = 128 cases) in
    tests/test_chunk_logic.lua. The oracle is the original code, verbatim;
    the table is data. Any wrong entry or wrong key fails the exhaustive test.

    Direction numbering (the mod's convention, from
    MapUtils.positionFromScaledDirections):
      1 = NW, 2 = N, 3 = NE, 4 = W, 5 = E, 6 = SW, 7 = S, 8 = SE
    Diagonals: 1, 3, 6, 8.  Axis: 2/7 = N/S, 4/5 = E/W.

    The table's semantics (transcribed from the chain):
      - start ALL/N/S/E/W: a diagonal move needs the end to be ALL; an axis
        move needs the end to allow that axis (N/S needs N/S or ALL; E/W needs
        E/W or ALL); a cross-axis move is false.
      - start IMPASSABLE: the chain's final `else` — can move anywhere the end
        is not impassable. Counterintuitive but preserved (the oracle tests it).
]]

local Constants = require("Constants")

local CHUNK_IMPASSABLE = Constants.CHUNK_IMPASSABLE
local CHUNK_NORTH_SOUTH = Constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = Constants.CHUNK_EAST_WEST
local CHUNK_ALL_DIRECTIONS = Constants.CHUNK_ALL_DIRECTIONS

local ChunkLogic = {}

-- CAN_MOVE[startPassable][direction][endPassable] = true when movement is
-- allowed. Nested tables: absent keys are false, so the lookup needs no default.
local CAN_MOVE = {
    [CHUNK_ALL_DIRECTIONS] = {
        [1] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- NW
        [2] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_ALL_DIRECTIONS] = true }, -- N
        [3] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- NE
        [4] = { [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },   -- W
        [5] = { [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },   -- E
        [6] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- SW
        [7] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_ALL_DIRECTIONS] = true }, -- S
        [8] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- SE
    },
    [CHUNK_NORTH_SOUTH] = {
        [1] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- NW
        [2] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_ALL_DIRECTIONS] = true }, -- N
        [3] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- NE
        -- W/E: false (no entry)
        [6] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- SW
        [7] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_ALL_DIRECTIONS] = true }, -- S
        [8] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- SE
    },
    [CHUNK_EAST_WEST] = {
        [1] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- NW
        -- N/S: false (no entry)
        [3] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- NE
        [4] = { [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },   -- W
        [5] = { [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },   -- E
        [6] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- SW
        [8] = { [CHUNK_ALL_DIRECTIONS] = true },                       -- SE
    },
    [CHUNK_IMPASSABLE] = {
        -- the chain's final else: end ~= IMPASSABLE, any direction
        [1] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
        [2] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
        [3] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
        [4] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
        [5] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
        [6] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
        [7] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
        [8] = { [CHUNK_NORTH_SOUTH] = true, [CHUNK_EAST_WEST] = true, [CHUNK_ALL_DIRECTIONS] = true },
    },
}

--- PURE: can a unit move from a chunk with passability `startPassable` in
-- `direction` to a chunk with passability `endPassable`?
-- @return boolean
function ChunkLogic.canMove(startPassable, direction, endPassable)
    local byDirection = CAN_MOVE[startPassable]
    if not byDirection then
        return false
    end
    local allowedEnds = byDirection[direction]
    if not allowedEnds then
        return false
    end
    return allowedEnds[endPassable] == true
end

ChunkLogic.CAN_MOVE = CAN_MOVE

return ChunkLogic
