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


-- imports

local smokeUtils = require("utils/SmokeUtils")

-- constants

local poison = {}

-- imported functions

local makeCloud = smokeUtils.makeCloud

function poison.addFactionAddon()

    for i=1,10 do
        makeCloud(
            {
                name = "poison-cloud-v" .. i,
                scale = 0.80 + (i * 0.15),
                wind = true,
                slowdown = -1.3,
                duration = 10 * (i * 5),
                cooldown = 5
            },
            {
                type = "direct",
                action_delivery =
                    {
                        type = "instant",
                        target_effects =
                            {
                                type = "nested-result",
                                action =
                                    {
                                        {
                                            type = "area",
                                            radius = 2 + (i * 0.5),
                                            force = "ally",
                                            entity_flags = {"placeable-enemy"},
                                            action_delivery =
                                                {
                                                    type = "instant",
                                                    target_effects =
                                                        {
                                                            type = "damage",
                                                            damage = { amount = -2 * i, type = "healing"}
                                                        }
                                                }
                                        },
                                        {
                                            type = "area",
                                            radius = 2 + (i * 0.5),
                                            force = "enemy",
                                            action_delivery =
                                                {
                                                    type = "instant",
                                                    target_effects =
                                                        {
                                                            type = "damage",
                                                            damage = { amount = 0.9 * i, type = "poison"}
                                                        }
                                                }
                                        }
                                    }
                            }
                    }
            }
        )
    end

end

return poison
