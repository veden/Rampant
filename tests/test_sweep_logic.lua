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
    SweepLogic coverage: the round-robin must visit every entry EXACTLY once per
    cycle -- no gaps, no duplicates. That is the invariant the sweep's scheduling
    exists to satisfy, and it is pure arithmetic, so it is proven exhaustively here
    rather than debugged in the engine. Also covers the cycle-length formula and the
    dynamic-source predicate.
]]

local TestCollection = require("framework")
local SweepLogic = require("sweep_logic")

local my_collection = TestCollection:new()

my_collection:add('sweep_covers_every_entry_exactly_once', function()
    local totals = { 1, 2, 39, 40, 41, 100, 392, 2183, 10739 }
    local rates = { 1, 3, 8, 20, 40, 200 }
    for _, total in ipairs(totals) do
        for _, rate in ipairs(rates) do
            local cycleTicks = SweepLogic.cycleTicks(total, rate)
            local covered = {}
            local visits = 0
            for t = 0, cycleTicks - 1 do
                local first, last = SweepLogic.slice(t, cycleTicks, rate, total)
                for i = first, last do
                    assert(covered[i] == nil,
                        "duplicate entry " .. i .. " (total=" .. total ..
                        " rate=" .. rate .. " tick=" .. t .. ")")
                    covered[i] = true
                    visits = visits + 1
                end
            end
            assert(visits == total,
                "coverage gap: visited " .. visits .. " of " .. total ..
                " (rate=" .. rate .. ")")
        end
    end
end)

my_collection:add('sweep_never_exceeds_the_list', function()
    local first, last = SweepLogic.slice(37, 11, 40, 392)
    assert(last <= 392, "slice end past the list")
    assert(first >= 1 and first <= last, "bad slice bounds")
end)

my_collection:add('sweep_empty_list_is_noop', function()
    local first, last = SweepLogic.slice(5, 1, 40, 0)
    assert(first == 0 and last == 0, "empty list must yield no slice")
end)

my_collection:add('sweep_cycle_index_is_stable_within_a_cycle', function()
    -- the bug the review flagged: a cycle length recomputed from a flapping
    -- list length must not be recomputed inside a cycle. This tests the pure
    -- index function only; the caller must FREEZE cycleTicks per cycle.
    assert(SweepLogic.cycleIndex(0, 10) == 0)
    assert(SweepLogic.cycleIndex(9, 10) == 0)
    assert(SweepLogic.cycleIndex(10, 10) == 1)
    assert(SweepLogic.cycleIndex(19, 10) == 1)
    assert(SweepLogic.cycleIndex(20, 10) == 2)
end)

my_collection:add('sweep_dynamic_source_predicate', function()
    -- static-only: ore
    assert(SweepLogic.isDynamicSource({ resourceGenerator = 100 }) == false,
        "ore-only chunk is static")
    -- dynamic: each of the others
    assert(SweepLogic.isDynamicSource({ resourceGenerator = 100, nestCount = 1 }) == true)
    assert(SweepLogic.isDynamicSource({ playerBaseGenerator = 50 }) == true)
    assert(SweepLogic.isDynamicSource({ playerGenerator = 10 }) == true)
    assert(SweepLogic.isDynamicSource({ turretCount = 1 }) == true)
    assert(SweepLogic.isDynamicSource({ utilityCount = 1 }) == true)
    assert(SweepLogic.isDynamicSource({ hiveCount = 1 }) == true)
    assert(SweepLogic.isDynamicSource({ deathGenerator = 0.5 }) == true)
    -- empty
    assert(SweepLogic.isDynamicSource({}) == false, "empty chunk is static")
end)

return my_collection
