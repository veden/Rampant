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

local unitSpawnerUtils = {}

function unitSpawnerUtils.spawner_integration(scale)
    return
        {
            filename = "__base__/graphics/entity/spawner/spawner-idle.png",
            variation_count = 4,
            width = 258,
            height = 188,
            shift = util.by_pixel(2, -2),
            frame_count = 1,
            scale = scale * 1.01,
            line_length = 1
        }
end

function unitSpawnerUtils.spawner_idle_animation(variation, tint, scale, tint2)
    return
        {
            layers =
                {
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-idle.png",
                        line_length = 4,
                        width = 248,
                        height = 180,
                        frame_count = 8,
                        animation_speed = 0.18,
                        direction_count = 1,
                        scale = scale,
                        run_mode = "forward-then-backward",
                        shift = util.by_pixel(2, -4),
                        tint = tint,
                        y = variation * 180 * 2
                    },
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-idle-mask.png",
                        flags = { "mask" },
                        width = 140,
                        height = 118,
                        frame_count = 8,
                        animation_speed = 0.18,
                        run_mode = "forward-then-backward",
                        shift = util.by_pixel(-1.5 + (-0.5 * scale), -11 + (-3 * scale)),
                        line_length = 4,
                        tint = tint2,
                        scale = scale,
                        y = variation * 118 * 2
                    }
                }
        }
end

function unitSpawnerUtils.spawner_die_animation(variation, tint, scale, tint2)
    return
        {
            layers =
                {
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-death-v" .. (variation + 1) .. ".png",
                        line_length = 8,
                        width = 248,
                        height = 178,
                        frame_count = 8,
                        direction_count = 1,
                        shift = util.by_pixel(2, -2),
                        tint = tint,
                        scale = scale
                    },
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-death-mask-v" .. (variation + 1) .. ".png",
                        flags = { "mask" },
                        width = 140,
                        height = 118,
                        frame_count = 8,
                        direction_count = 1,
                        shift = util.by_pixel(-2, -14),
                        scale = scale,
                        line_length = 8,
                        tint = tint2
                    }
                }
        }
end

return unitSpawnerUtils
