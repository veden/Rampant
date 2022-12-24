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


-- module code

function generateMigration()
    local factions = {"neutral", "acid", "physical", "electric", "suicide", "nuclear", "fire", "inferno", "troll", "laser", "fast", "wasp", "spawner", "energy-thief", "poison"}

    for fi = 1, #factions do
        local faction = factions[fi]
        for t = 1, 10 do
            local adjT = t
            -- if t > 5 then
            --     adjT = t - 5
            -- end
            for v = 2, 20 do
                print("        [\"" .. faction .. "-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\", \"" .. faction .. "-biter-spawner-v1-t" .. adjT .. "-rampant\"],")
                print("        [\"" .. faction .. "-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\", \"" .. faction .. "-spitter-spawner-v1-t" .. adjT .. "-rampant\"],")
                print("        [\"" .. faction .. "-worm-v" .. v .. "-t" .. t .. "-rampant\", \"" .. faction .. "-worm-v1-t" .. adjT .. "-rampant\"],")
                print("        [\"" .. faction .. "-hive-v" .. v .. "-t" .. t .. "-rampant\", \"" .. faction .. "-hive-v1-t" .. adjT .. "-rampant\"],")
                print("        [\"" .. faction .. "-biter-v" .. v .. "-t" .. t .. "-rampant\", \"" .. faction .. "-biter-v1-t" .. adjT .. "-rampant\"],")
                print("        [\"" .. faction .. "-spitter-v" .. v .. "-t" .. t .. "-rampant\", \"" .. faction .. "-spitter-v1-t" .. adjT .. "-rampant\"],")
            end
        end
    end
end
