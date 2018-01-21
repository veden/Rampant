local stickerUtils = {}

-- imported

local math3d = require("math3d")

-- module code

function stickerUtils.makeSticker(attributes)
    local name = attributes.name .. "-sticker-rampant"
    data:extend(
	{
	    {
		type = "sticker",
		name = name,
		flags = {"not-on-map"},
		
		animation = 
		    { 
			filename = "__base__/graphics/entity/fire-flame/fire-flame-13.png",
			line_length = 8,
			width = 60,
			height = 118,
			frame_count = 25,
			axially_symmetrical = false,
			direction_count = 1,
			blend_mode = "normal",
			animation_speed = 1,
			scale = 0.2,
			tint = attributes.stickerTint, --{ r = 1, g = 1, b = 1, a = 0.35 },
			shift = math3d.vector2.mul({-0.078125, -1.8125}, 0.1),
		    },
		
		duration_in_ticks = attributes.stickerDuration or (30 * 60),
		target_movement_modifier = attributes.stickerMovementModifier or 0.8,
		damage_per_tick = attributes.stickerDamagePerTick or { amount = 120 / 60, type = "fire" },
		spread_fire_entity = attributes.spawnEntityName,  
		fire_spread_cooldown = 30,
		fire_spread_radius = 0.75,
	    }
    })
    return name
end


return stickerUtils
