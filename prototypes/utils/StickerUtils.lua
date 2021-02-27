
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
                filename = "__base__/graphics/entity/fire-flame/fire-flame-13.png",
                line_length = 8,
                width = 60,
                height = 118,
                frame_count = 25,
                axially_symmetrical = false,
                direction_count = 1,
                blend_mode = "normal",
                animation_speed = 2,
                scale = 0.4,
                tint = attributes.tint2 or { r = 1, g = 1, b = 1, a = 0.35 },
                shift = math3d.vector2.mul({-0.078125, -1.8125}, 0.1),
            },

        duration_in_ticks = attributes.stickerDuration or (30 * 60),
        target_movement_modifier_from = attributes.stickerMovementModifier or 1,
        target_movement_modifier_to = 1,
        vehicle_speed_modifier_from = (attributes.stickerMovementModifier and math.min(attributes.stickerMovementModifier * 1.25, 1)) or 1,
        -- vehicle_speed_modifier_from = attributes.stickerMovementModifier or 1,
        vehicle_speed_modifier_to = 1,
        -- vehicle_friction_modifier_from = 1.5,
        -- vehicle_friction_modifier_from = 1,
        -- vehicle_friction_modifier_to = 1,

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
