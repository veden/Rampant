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
    Death - Pure death event evaluation logic.

    Zero Factorio dependencies. Takes event data, returns commands.
]]

local Command = require("command")
local Constants = require("Constants")

local UNIT_DEATH_POINT_COST = Constants.UNIT_DEATH_POINT_COST

local Death = {}

--- Evaluate an enemy entity death. Returns table of commands.
-- @param data table with fields:
--   entityType: "unit" | "unit-spawner" | "turret" | other
--   entityName: string
--   chunkId: number (or -1 if no chunk)
--   baseId: number (or nil)
--   squadId: number (or nil)
--   damageType: string (or nil)
--   isArtilleryCause: bool
--   deathGeneratorRating: number
--   retreatThreshold: number
--   causeValid: bool (cause entity exists and has health)
--   rallyThreshold: number (0-1 roll threshold)
--   randomRoll: number (0-1 for rally chance)
--   lostEnemyUnits: number (current count before this death)
--   isDrainPylon: bool
function Death.evaluateEnemy(data)
    local commands = {}

    if data.isDrainPylon then
        return commands  -- drain pylons handled separately
    end

    if data.entityType == "unit" then
        -- Decompress squad state
        if data.squadId then
            table.insert(commands, Command.decompressSquad(data.squadId))
        end

        if data.chunkId ~= -1 then
            -- Mark chunk dirty for pheromone recalculation

            if data.baseId then
                local base = data.base
                base.lostEnemyUnits = (base.lostEnemyUnits or 0) + 1

                if data.damageType then
                    base.damagedBy = base.damagedBy or {}
                    base.damagedBy[data.damageType] = (base.damagedBy[data.damageType] or 0) + 0.01
                    base.deathEvents = (base.deathEvents or 0) + 1
                end

                -- Point cost every 20 units lost
                local lostCount = base.lostEnemyUnits
                if lostCount % 20 == 0 then
                    table.insert(commands, Command.modifyUnitPoints(
                        data.baseId, -(20 * UNIT_DEATH_POINT_COST), "20 Units Lost"))
                end

                -- Rally check
                if data.randomRoll < data.rallyThreshold then
                    table.insert(commands, Command.rallySquads(
                        data.chunkId, "death_cry"))
                end

                if data.isArtilleryCause then
                    base.artilleryBlasts = (base.artilleryBlasts or 0) + 1
                end
            end

            -- Retreat check
            if data.causeValid and
               data.deathGeneratorRating < data.retreatThreshold then
                local radius = data.isArtilleryCause and 80 or 40
                table.insert(commands, Command.retreatUnits(
                    data.chunkId, "player_damage", radius))
            end
        end

    else
        -- Structure death (spawner, turret, building)
        if data.chunkId ~= -1 then
            table.insert(commands, Command.unregisterStructure(
                data.chunkId, data.entityName, data.damageType))

            if data.baseId then
                if data.entityType == "unit-spawner" then
                    table.insert(commands, Command.modifyUnitPoints(
                        data.baseId, -500, "Nest Lost"))
                elseif data.entityType == "turret" then
                    table.insert(commands, Command.modifyUnitPoints(
                        data.baseId, -300, "Worm Lost"))
                end
                table.insert(commands, Command.rallySquads(
                    data.chunkId, "structure_loss"))
                if data.isArtilleryCause then
                    local base = data.base
                    base.artilleryBlasts = (base.artilleryBlasts or 0) + 1
                end
            end

            if data.causeValid then
                table.insert(commands, Command.retreatUnits(
                    data.chunkId, "structure_destroyed", 80))
            end
        end
    end

    return commands
end

return Death
