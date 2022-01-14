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


local bombUtils = {}

function bombUtils.makeAtomicBlast(attributes)
    local name = attributes.name .. "-atomic-blast-rampant"
    data:extend({{
                type = "projectile",
                name = name,
                flags = {"not-on-map"},
                acceleration = 0,
                action =
                    {
                        {
                            type = "direct",
                            action_delivery =
                                {
                                    type = "instant",
                                    target_effects =
                                        {
                                            {
                                                type = "create-entity",
                                                entity_name = "explosion"
                                            }
                                        }
                                }
                        },
                        {
                            type = "area",
                            radius = 3,
                            action_delivery =
                                {
                                    type = "instant",
                                    target_effects =
                                        {
                                            type = "damage",
                                            damage = {amount = (attributes.damage * 2) or 400,
                                                      type = attributes.damageType or "explosion"}
                                        }
                                }
                        }
                    },
                animation =
                    {
                        filename = "__core__/graphics/empty.png",
                        frame_count = 1,
                        width = 1,
                        height = 1,
                        priority = "high"
                    },
                shadow =
                    {
                        filename = "__core__/graphics/empty.png",
                        frame_count = 1,
                        width = 1,
                        height = 1,
                        priority = "high"
                    }
    }})
    return name
end

return bombUtils
