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
    Sweep Logic - pure round-robin scheduling for the background sweep.

    Pure module. Zero Factorio dependencies. The sweep's SCHEDULING is the part
    with the subtle invariants (full coverage exactly once per cycle, no gaps, no
    duplicates, stable cycle length), and it is pure arithmetic -- so it can be
    proven exhaustively in the unit harness rather than debugged in the engine.
    The impure shell (iterating storage, marking chunks dirty) stays in Processor.
]]

local SweepLogic = {}

--- Cycle length (ticks) for a list of `total` entries at `rate` per tick.
function SweepLogic.cycleTicks(total, rate)
    if rate <= 0 or total <= 0 then return 1 end
    local c = math.ceil(total / rate)
    if c < 1 then c = 1 end
    return c
end

--- The half-open index range [first, last] this tick sweeps, 1-based inclusive.
-- Returns 0, 0 when there is nothing to sweep this tick.
function SweepLogic.slice(tick, cycleTicks, rate, total)
    if total <= 0 or rate <= 0 or cycleTicks <= 0 then return 0, 0 end
    local offset = (tick % cycleTicks) * rate
    local last = offset + rate
    if last > total then last = total end
    if offset + 1 > last then return 0, 0 end
    return offset + 1, last
end

--- Should the sweep list be rebuilt this tick?
function SweepLogic.shouldRebuild(cycleIndex, storedCycleIndex)
    return cycleIndex ~= storedCycleIndex
end

--- Cycle index for a tick at a cycle length.
function SweepLogic.cycleIndex(tick, cycleTicks)
    if not cycleTicks or cycleTicks <= 0 then return 0 end
    return math.floor(tick / cycleTicks)
end

--- Is this chunk a DYNAMIC source (needs periodic renewal)?
-- Static-only chunks (ore and nothing else) do not: their field is a fixed point.
function SweepLogic.isDynamicSource(g)
    return (g.playerBaseGenerator or 0) > 0
        or (g.playerGenerator or 0) > 0
        or (g.nestCount or 0) > 0
        or (g.turretCount or 0) > 0
        or (g.utilityCount or 0) > 0
        or (g.hiveCount or 0) > 0
        or (g.deathGenerator or 0) ~= 0
end

return SweepLogic
