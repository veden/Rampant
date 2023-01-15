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


local vanillaUpdates = {}

function vanillaUpdates.addWallAcidResistance()
    local walls = data.raw["wall"]

    for _,wall in pairs(walls) do
        local foundAcid = false
        if wall.resistances then
            for _,resistance in pairs(wall.resistances) do
                if resistance.type == "acid" then
                    if resistance.percent < 60 then
                        resistance.percent = 60
                    end
                    foundAcid = true
                    break
                end
            end
            if not foundAcid then
                wall.resistances[#wall.resistances+1] = {type="acid",percent=60}
            end
        end
    end

    walls = data.raw["gate"]
    for _,wall in pairs(walls) do
        local foundAcid = false
        if wall.resistances then
            for _,resistance in pairs(wall.resistances) do
                if resistance.type == "acid" then
                    if resistance.percent < 60 then
                        resistance.percent = 60
                    end
                    foundAcid = true
                    break
                end
            end
            if not foundAcid then
                wall.resistances[#wall.resistances+1] = {type="acid",percent=60}
            end
        end
    end
end

return vanillaUpdates
