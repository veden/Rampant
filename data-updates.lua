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


local vanillaUpdates = require("prototypes/utils/UpdatesVanilla")
local attackBall = require("prototypes/utils/AttackBall")

if settings.startup["rampant--useDumbProjectiles"].value or settings.startup["rampant--newEnemies"].value then
    attackBall.generateVanilla()
    vanillaUpdates.useDumbProjectiles()
end

for _, robot in pairs(data.raw["logistic-robot"]) do
    if (settings.startup["rampant--unkillableLogisticRobots"].value) then
        robot.resistances = {}
        for damageType, _ in pairs(data.raw["damage-type"]) do
            robot.resistances[damageType] = {
                type = damageType,
                percent = 100
            }
        end
    end
end

for _, robot in pairs(data.raw["construction-robot"]) do
    if (settings.startup["rampant--unkillableConstructionRobots"].value) then
        robot.resistances = {}
        for damageType, _ in pairs(data.raw["damage-type"]) do
            robot.resistances[damageType] = {
                type = damageType,
                percent = 100
            }
        end
    end
end

