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

if DebugLogging then
    return DebugLogging
end

local DebugLogging = {}

local LOG_FILE = "/tmp/rampant-debug.log"

function DebugLogging.log(message)
    local tick = game and game.tick or 0
    local timestamp = os.date("%Y-%m-%d %H:%M:%S")
    local line = "[" .. timestamp .. " tick:" .. tick .. "] " .. message
    
    -- Write to log file
    local file = io.open(LOG_FILE, "a")
    if file then
        file:write(line .. "\n")
        file:close()
    end
    
    -- Also print to console
    print(line)
end

function DebugLogging.clear()
    local file = io.open(LOG_FILE, "w")
    if file then
        file:write("")
        file:close()
    end
end

function DebugLogging.getLogs()
    local file = io.open(LOG_FILE, "r")
    if file then
        local content = file:read("*a")
        file:close()
        return content
    end
    return ""
end

-- Register remote interface for RCON access
if remote then
    remote.add_interface("RampantDebug", {
        getLogs = function()
            return DebugLogging.getLogs()
        end,
        clear = function()
            DebugLogging.clear()
        end,
    })
end

return DebugLogging