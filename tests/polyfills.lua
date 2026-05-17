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
    Lua Polyfills for Testing

    Factorio ships with an extended Lua runtime (LuaJIT-based) that includes
    functions not present in standard PUC-Rio Lua. This module provides
    polyfills so tests can run on system Lua without modifying production code.

    Load this file BEFORE requiring any production libs.
]]

local Polyfills = {}

--- Apply all polyfills to the global environment.
-- Idempotent: only sets a value if it's currently nil.
function Polyfills.apply()
    if not math.log10 then
        math.log10 = function(x)
            return math.log(x, 10)
        end
    end
end

Polyfills.apply()

return Polyfills
