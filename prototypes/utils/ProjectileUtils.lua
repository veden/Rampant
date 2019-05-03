local projectileUtils = {}

function projectileUtils.makeProjectile(name, attributes, attack)
    local n = name .. "-projectile-rampant"
    
    data:extend({{
		type = "projectile",
		name = n,
		flags = {"not-on-map"},
		collision_box = attributes.collisionBox or {{-0.01, -0.01}, {0.01, 0.01}},
		collision_mask = attributes.collisionMask or {"layer-11"},
		direction_only = attributes.directionOnly,
		piercing_damage = attributes.piercingDamage or 0,
		acceleration = attributes.acceleration or 0.02,
                force_condition = (settings.startup["rampant-disableCollidingProjectiles"].value and "not-same") or nil,
		action = attack,
		animation =
		    {
                        filename = "__base__/graphics/entity/acid-projectile/acid-projectile-head.png",
                        line_length = 5,
                        width = 22,
                        height = 84,
                        frame_count = 15,
                        shift = util.mul_shift(util.by_pixel(-2, 30), attributes.scale or 1),
                        tint = attributes.tint,
                        priority = "high",
                        scale = (attributes.scale or 1),
                        animation_speed = 1,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/acid-projectile/hr-acid-projectile-head.png",
                                line_length = 5,
                                width = 42,
                                height = 164,
                                frame_count = 15,
                                shift = util.mul_shift(util.by_pixel(-2, 31), attributes.scale or 1),
                                tint = attributes.tint,
                                priority = "high",
                                scale = 0.5 * (attributes.scale or 1),
                                animation_speed = 1,
                            }
                    },
		shadow =
		    {
                        filename = "__base__/graphics/entity/acid-projectile/acid-projectile-shadow.png",
                        line_length = 15,
                        width = 22,
                        height = 84,
                        frame_count = 15,
                        priority = "high",
                        shift = util.mul_shift(util.by_pixel(-2, 30), attributes.scale or 1),
                        draw_as_shadow = true,
                        scale = (attributes.scale or 1),
                        animation_speed = 1,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/acid-projectile/hr-acid-projectile-shadow.png",
                                line_length = 15,
                                width = 42,
                                height = 164,
                                frame_count = 15,
                                shift = util.mul_shift(util.by_pixel(-2, 31), attributes.scale or 1),
                                draw_as_shadow = true,
                                priority = "high",
                                scale = 0.5 * (attributes.scale or 1),
                                animation_speed = 1,
                            }
                    },
		-- rotatable = false,
                oriented_particle = true,
                shadow_scale_enabled = true,

    }})

    return n
end

return projectileUtils
