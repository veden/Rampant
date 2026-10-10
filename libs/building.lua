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
    Building - Evaluate enemy structure construction.

    Zero Factorio imports. Takes gathered data, returns commands.
]]

local Command = require("command")

local Building = {}

--- Evaluate an enemy structure being built.
-- @param data table with:
--   entityName: string
--   entityType: string
--   hasMap: bool
--   hasChunk: bool (chunk ~= -1)
--   chunkId: number or nil
--   chunkX: number
--   chunkY: number
--   baseId: number or nil (existing base found nearby)
--   isNewBase: bool (no base existed, needs creation)
--   newEnemies: bool (Universe.NEW_ENEMIES setting)
--   isVanillaEntity: bool (in VANILLA_ENTITY_TYPE_LOOKUP)
-- @return table of commands
function Building.evaluateEnemy(data)
    local commands = {}

    if not data.hasMap then
        return commands
    end

    if data.hasChunk then
        -- Within chunk grid
        if data.isNewBase then
            table.insert(commands, Command.createBase(data.chunkId, data.chunkX, data.chunkY))
        end


        if data.newEnemies then
            if data.isVanillaEntity then
                -- Vanilla entity → queue for upgrade to Rampant type
                table.insert(commands, Command.queueUpgrade(
                    data.entityName, data.baseId, true))
            else
                -- Rampant entity → register directly
                table.insert(commands, Command.registerStructure(
                    data.chunkId, data.baseId, data.entityName))
            end
        else
            -- Legacy mode → always register
            table.insert(commands, Command.registerStructure(
                data.chunkId, data.baseId, data.entityName))
        end
    else
        -- Outside chunk grid → trigger chunk generation
        table.insert(commands, Command.triggerChunkGeneration(
            data.chunkX, data.chunkY))
    end

    return commands
end

return Building
