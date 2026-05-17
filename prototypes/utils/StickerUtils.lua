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



local stickerUtils = {}

-- imported

local math3d = require("math3d")

-- module code

function stickerUtils.makeSticker(attributes)
    local name = attributes.name .. "-sticker-rampant"

    local o = {
        type = "sticker",
        name = name,
        flags = {"not-on-map"},

        animation = attributes.stickerAnimation or
            {
                -- TODO: fire-flame-13.png removed in Factorio 2.0. Use generic burn texture.
                filename = "__base__/graphics/entity/fire-flame/burnt-patch.png",
                line_length = 4,
                width = 84,
                height = 68,
                frame_count = 1,
                axially_symmetrical = false,
                direction_count = 1,
                blend_mode = "normal",
                animation_speed = 1,
                scale = 0.5,
                tint = attributes.tint2 or { r = 1, g = 0.5, b = 0.2, a = 0.5 },
                shift = {0, 0},
            },

        duration_in_ticks = attributes.stickerDuration or (30 * 60),
        target_movement_modifier_from = attributes.stickerMovementModifier or 1,
        target_movement_modifier_to = 1,
        vehicle_speed_modifier_from = (attributes.stickerMovementModifier and math.min(attributes.stickerMovementModifier * 1.25, 1)) or 1,
        vehicle_speed_modifier_to = 1,

        stickers_per_square_meter = 2,

        damage_per_tick = attributes.stickerDamagePerTick and { amount = attributes.stickerDamagePerTick or 100 / 60,
                                                                type = attributes.stickerDamagePerTickType or "fire" },
        spread_fire_entity = attributes.spawnEntityName,
        fire_spread_cooldown = attributes.fireSpreadCooldown,
        fire_spread_radius = attributes.fireSpreadRadius
    }

    data:extend({o})
    return name
end


return stickerUtils
