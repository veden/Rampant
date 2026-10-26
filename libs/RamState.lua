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

-- Derived state that is safe to keep OUTSIDE storage.
--
-- Multiplayer constraint: Factorio transfers `storage` (per-mod global) and
-- re-executes module code locally, so anything mutable outside storage must be a
-- PURE derivation of stored data - every peer must compute the same value from
-- the same storage, and the value must not depend on which events a peer saw.
--
-- The neighbour-id cache qualifies: ids are a function of the chunk's stored
-- coordinates and the stored chunk records, so a late joiner gets identical ids.
--
-- The source-versioned FIELDS do not qualify and stayed in storage: the
-- incremental solver does not reproduce a full solve exactly (measured 21
-- mismatches, 606 live-only chunk values), so a client re-deriving them on load
-- would diverge from the host.
--
-- This is a deliberate exception to the "globals must be constant" rule: these
-- tables are mutable, but they are pure caches with no authoritative state. If
-- they are empty (first load, or after a save/load), the solver rebuilds them
-- lazily, exactly as it does for a field that has never been read.

if RamStateG then
    return RamStateG
end

local RamState = {
}

RamStateG = RamState
return RamState
