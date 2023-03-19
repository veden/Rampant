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


-- import

local colorUtils = require("prototypes/utils/ColorUtils")
local smokeUtils = require("prototypes/utils/SmokeUtils")
local swarmUtils = require("prototypes/SwarmUtils")
local collision_mask_util = require("collision-mask-util")

-- imported functions

local makeSmokeSoft = smokeUtils.makeSmokeSoft
local makeSmokeWithGlow = smokeUtils.makeSmokeWithGlow
local makeSmokeWithoutGlow = smokeUtils.makeSmokeWithoutGlow
local makeSmokeAddingFuel = smokeUtils.makeSmokeAddingFuel

local makeColor = colorUtils.makeColor

-- module code

makeSmokeSoft({name="the", softSmokeTint=makeColor(0.3, 0.75, 0.3, 0.1)})
makeSmokeWithGlow({name="the", smokeWithGlowTint=makeColor(0.3, 0.75, 0.3, 0.1)})
makeSmokeWithoutGlow({name="the", smokeWithoutGlowTint=makeColor(0.3, 0.75, 0.3, 0.1)})
makeSmokeAddingFuel({name="the"})

require("prototypes/buildings/ChunkScanner")

RampantGlobalVariables = {}
RampantGlobalVariables.projectileCollisionLayer = collision_mask_util.get_first_unused_layer()

if not data.raw["corpse"]["acid-splash-purple"] then
    local attributes = {}

    data:extend({
            {
                type = "corpse",
                name = "acid-splash-purple",
                flags = {"not-on-map"},
                time_before_removed = 60 * 30,
                final_render_layer = "corpse",
                splash =
                    {
                        {
                            filename = "__base__/graphics/entity/acid-splash/acid-splash-1.png",
                            line_length = 8,
                            direction_count = 1,
                            width = 106,
                            height = 116,
                            frame_count = 26,
                            shift = util.mul_shift(util.by_pixel(-12, -10), attributes.scale or 1),
                            tint = attributes.tint,
                            scale = (attributes.scale or 1),
                            hr_version = {
                                filename = "__base__/graphics/entity/acid-splash/hr-acid-splash-1.png",
                                line_length = 8,
                                direction_count = 1,
                                width = 210,
                                height = 224,
                                frame_count = 26,
                                shift = util.mul_shift(util.by_pixel(-12, -8), attributes.scale or 1),
                                tint = attributes.tint,
                                scale = 0.5 * (attributes.scale or 1),
                            }
                        },
                        {
                            filename = "__base__/graphics/entity/acid-splash/acid-splash-2.png",
                            line_length = 8,
                            direction_count = 1,
                            width = 88,
                            height = 76,
                            frame_count = 29,
                            shift = util.mul_shift(util.by_pixel(-10, -18), attributes.scale or 1),
                            tint = attributes.tint,
                            scale = (attributes.scale or 1),
                            hr_version = {
                                filename = "__base__/graphics/entity/acid-splash/hr-acid-splash-2.png",
                                line_length = 8,
                                direction_count = 1,
                                width = 174,
                                height = 150,
                                frame_count = 29,
                                shift = util.mul_shift(util.by_pixel(-9, -17), attributes.scale or 1),
                                tint = attributes.tint,
                                scale = 0.5 * (attributes.scale or 1),
                            }
                        },
                        {
                            filename = "__base__/graphics/entity/acid-splash/acid-splash-3.png",
                            line_length = 8,
                            direction_count = 1,
                            width = 118,
                            height = 104,
                            frame_count = 29,
                            shift = util.mul_shift(util.by_pixel(22, -16), attributes.scale or 1),
                            tint = attributes.tint,
                            scale = (attributes.scale or 1),
                            hr_version = {
                                filename = "__base__/graphics/entity/acid-splash/hr-acid-splash-3.png",
                                line_length = 8,
                                direction_count = 1,
                                width = 236,
                                height = 208,
                                frame_count = 29,
                                shift = util.mul_shift(util.by_pixel(22, -16), attributes.scale or 1),
                                tint = attributes.tint,
                                scale = 0.5 * (attributes.scale or 1),
                            }
                        },
                        {
                            filename = "__base__/graphics/entity/acid-splash/acid-splash-4.png",
                            line_length = 8,
                            direction_count = 1,
                            width = 128,
                            height = 80,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(16, -20), attributes.scale or 1),
                            tint = attributes.tint,
                            scale = (attributes.scale or 1),
                            hr_version = {
                                filename = "__base__/graphics/entity/acid-splash/hr-acid-splash-4.png",
                                line_length = 8,
                                direction_count = 1,
                                width = 252,
                                height = 154,
                                frame_count = 24,
                                shift = util.mul_shift(util.by_pixel(17, -19), attributes.scale or 1),
                                tint = attributes.tint,
                                scale = 0.5 * (attributes.scale or 1),
                            }
                        }
                    },
                splash_speed = 0.03
            }
    })
end

if settings.startup["rampant--newEnemies"].value then
    swarmUtils.processFactions()
end
