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
    Squad - Pure squad decision logic.

    Zero Factorio dependencies. Takes data tables, returns commands.
]]

local Command = require("command")
local Constants = require("Constants")

local SQUAD_RAIDING    = Constants.SQUAD_RAIDING
local SQUAD_SETTLING   = Constants.SQUAD_SETTLING
local SQUAD_GUARDING   = Constants.SQUAD_GUARDING
local SQUAD_RETREATING = Constants.SQUAD_RETREATING

local Squad = {}

--- Evaluate what a squad should do. Returns a table of commands.
-- State transitions: RETREATING/GUARDING → SETTLING or RAIDING.
-- RAIDING/SETTLING/BUILDING continue without transition.
-- @param squad table with: { id, status, settlers }
-- @return table of commands
function Squad.evaluate(squad)
    local commands = {}
    local status = squad.status

    if status == SQUAD_RETREATING then
        if squad.settlers then
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_SETTLING, nil, "retreat_to_settle"))
        else
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_RAIDING, nil, "retreat_end"))
        end

    elseif status == SQUAD_GUARDING then
        if squad.settlers then
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_SETTLING, nil, "guard_to_settle"))
        else
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_RAIDING, nil, "guard_end"))
        end
        -- SQUAD_BUILDING, SQUAD_RAIDING, SQUAD_SETTLING: no transition
    end

    return commands
end

-- Threshold calculations

function Squad.kamikazeSquadThreshold(memberCount, attackWaveMaxSize, evolutionLevel)
    return (memberCount / attackWaveMaxSize) * 0.2 + (evolutionLevel * 0.2)
end

function Squad.kamikazeSettlerThreshold(memberCount, expansionMaxSize, evolutionLevel)
    return (memberCount / expansionMaxSize) * 0.2 + (evolutionLevel * 0.2)
end

-- Location scoring (takes pheromone values directly)

function Squad.scoreSettlerLocation(resourcePheromone, playerPheromone)
    return resourcePheromone + (-playerPheromone)
end

function Squad.scoreSiegeSettlerLocation(resourcePheromone, basePheromone, playerPheromone)
    return resourcePheromone + basePheromone + (-playerPheromone)
end

function Squad.scoreUnitGroupLocation(enemyPheromone, playerPheromone, kamikazePheromone)
    return enemyPheromone + kamikazePheromone + (-playerPheromone)
end

function Squad.validUnitGroupLocation(kamikazePheromone)
    return kamikazePheromone > 0
end

return Squad
