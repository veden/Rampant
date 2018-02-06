local projectileUtils = {}

function projectileUtils.makeProjectile(name, attributes, attack)
    local n = name .. "-projectile-rampant"
    
    data:extend({{
		type = "projectile",
		name = n,
		flags = {"not-on-map"},
		collision_box = {{-0.3, -1.1}, {0.3, 1.1}},
		collision_mask = attributes.collisionMask or { "water-tile" },
		direction_only = attributes.directionOnly,
		piercing_damage = attributes.piercingDamage or 0,
		acceleration = attributes.acceleration or 0.005,
		action = attack,
		animation =
		    {
			filename = "__base__/graphics/entity/acid-projectile-purple/acid-projectile-purple.png",
			line_length = 5,
			tint = attributes.aTint,
			width = 16,
			height = 18,
			frame_count = 33,
			priority = "high"
		    },
		shadow =
		    {
			filename = "__base__/graphics/entity/acid-projectile-purple/acid-projectile-purple-shadow.png",
			line_length = 5,
			tint = attributes.aTint,
			width = 28,
			height = 16,
			frame_count = 33,
			priority = "high",
			shift = {-0.09, 0.395}
		    },
		rotatable = false
    }})
    return n
end

return projectileUtils
