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

-- Derived state that does NOT belong in the save file.
--
-- Everything here is recomputable: the source-versioned fields are solved from
-- their sources, and the neighbour-id cache is derived from chunk coordinates.
-- Keeping them in `storage` meant ~14MB of the session heap was written to the
-- save and read back on load, for data the mod rebuilds anyway. Module-level
-- tables are re-created when the control stage is re-executed, so they live in
-- RAM only.
--
-- This is a deliberate exception to the "globals must be constant" rule: these
-- tables are mutable, but they are pure caches with no authoritative state. If
-- they are empty (first load, or after a save/load), the solver rebuilds them
-- lazily, exactly as it does for a field that has never been read.

if RamStateG then
    return RamStateG
end

local RamState = {
    -- Per field: chunkId -> value / top source / second best / second source.
    fields = {},
    top = {},
    second = {},
    secondSource = {},
    -- Per field: chunkIds whose value may still need to propagate.
    frontier = {},
    -- Per field: solve bookkeeping.
    solved = {},
    solvedAt = {},
    -- Leftover heap for a budgeted source removal.
    removal = {},
    -- chunkId -> list of neighbour chunk ids (derived from coordinates).
    neighborIds = {},
}

RamStateG = RamState
return RamState
