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
    Squad Creation - Evaluate whether to adopt a vanilla unit group.

    Zero Factorio imports. Takes gathered data, returns commands.
]]

local Command = require("command")
local BaseLogic = require("base_logic")

local SquadCreation = {}

--- Evaluate whether to adopt a vanilla-spawned unit group.
-- @param data table with:
--   forceName: string ("enemy", "neutral", etc.)
--   isScriptDriven: bool
--   universeAwake: bool
--   hasMap: bool
--   baseId: number or nil
--   canMigrate: bool (from base_logic.canMigrate)
--   builderCount: number
--   maxBuilderCount: number
--   squadCount: number
--   maxSquadCount: number
--   hasChunk: bool (chunk ~= -1)
--   basePheromone: number
--   playerPheromone: number
--   aiNocturnalMode: bool
--   darkness: number (0-1)
--   randomRoll: number (0-1)
-- @return table of commands (empty = destroy/ignore)
function SquadCreation.evaluate(data)
    -- Not enemy force → ignore
    if data.forceName ~= "enemy" then
        return {}
    end

    -- Script-driven → ignore (Factorio manages it)
    if data.isScriptDriven then
        return {}
    end

    -- Universe not awake → destroy
    if not data.universeAwake then
        return { Command.destroyGroup("universe_not_awake") }
    end

    -- No map → ignore
    if not data.hasMap then
        return {}
    end

    -- No base found → destroy
    if not data.baseId then
        return { Command.destroyGroup("no_base_found") }
    end

    -- Nocturnal mode: check darkness
    if data.aiNocturnalMode and data.darkness <= 0.65 then
        return { Command.destroyGroup("nocturnal_too_bright") }
    end

    -- Determine if settler
    local settler = BaseLogic.canMigrate(data.migrateParams) and
        (data.builderCount < data.maxBuilderCount) and
        (data.randomRoll < 0.25)

    -- If not settler, check squad viability
    if not settler then
        if data.squadCount >= data.maxSquadCount then
            return { Command.destroyGroup("squad_limit_reached") }
        end
        if not data.hasChunk then
            return { Command.destroyGroup("outside_chunk_grid") }
        end
        if data.basePheromone < 0.0001 and data.playerPheromone < 0.0001 then
            return { Command.destroyGroup("no_pheromone_interest") }
        end
    end

    return { Command.createSquad(nil, data.baseId, settler, "vanilla_spawn") }
end

return SquadCreation
