local droneUtils = {}

function droneUtils.createDrone(attributes)
    return {
	type = "combat-robot",
	name = "defender2",
	icon = "__base__/graphics/icons/defender.png",
	icon_size = 32,
	flags = {"placeable-player", "player-creation", "placeable-off-grid", "not-on-map", "not-repairable"},
	resistances = { { type = "fire", percent = 95 } },
	subgroup="capsule",
	order="e-a-a",
	max_health = 60,
	alert_when_damaged = false,
	collision_box = {{0, 0}, {0, 0}},
	selection_box = {{-0.5, -1.5}, {0.5, -0.5}},
	distance_per_frame = 0.13,
	time_to_live = 60 * 45,
	follows_player = true,
	friction = 0.01,
	range_from_player = 6.0,
	speed = 0.01,
	destroy_action =
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			source_effects =
			    {
				type = "create-entity",
				entity_name = "explosion"
			    }
		    }
	    },
	attack_parameters =
	    {
		type = "projectile",
		ammo_category = "bullet",
		cooldown = 20,
		projectile_center = {0, 1},
		projectile_creation_distance = 0.6,
		range = 15,
		sound = make_light_gunshot_sounds(),
		ammo_type =
		    {
			category = "bullet",
			action =
			    {
				type = "direct",
				action_delivery =
				    {
					type = "stream",
					stream = "acid-ball-stream-rampant",
					source_effects =
					    {
						type = "create-explosion",
						entity_name = "explosion-gunshot-small"
					    },
					target_effects =
					    {
						{
						    type = "create-entity",
						    entity_name = "explosion-hit"
						},
						{
						    type = "damage",
						    damage = { amount = 5 , type = "physical"}
						}
					    }
				    }
			    }
		    }
	    },
	idle =
	    {
		layers =
		    {
			{
			    filename = "__base__/graphics/entity/defender-robot/defender-robot.png",
			    priority = "high",
			    line_length = 16,
			    width = 32,
			    height = 33,
			    frame_count = 1,
			    direction_count = 16,
			    shift = {0, 0.015625},
			    hr_version = {
				filename = "__base__/graphics/entity/defender-robot/hr-defender-robot.png",
				priority = "high",
				line_length = 16,
				width = 56,
				height = 59,
				frame_count = 1,
				direction_count = 16,
				shift = util.by_pixel(0, 0.25),
				scale = 0.5
			    }
			},
			{
			    filename = "__base__/graphics/entity/defender-robot/defender-robot-mask.png",
			    priority = "high",
			    line_length = 16,
			    width = 18,
			    height = 16,
			    frame_count = 1,
			    direction_count = 16,
			    shift = {0, -0.125},
			    apply_runtime_tint = true,
			    hr_version = {
				filename = "__base__/graphics/entity/defender-robot/hr-defender-robot-mask.png",
				priority = "high",
				line_length = 16,
				width = 28,
				height = 21,
				frame_count = 1,
				direction_count = 16,
				shift = util.by_pixel(0, -4.75),
				apply_runtime_tint = true,
				scale = 0.5
			    }
			},
		    }
	    },
	shadow_idle =
	    {
		filename = "__base__/graphics/entity/defender-robot/defender-robot-shadow.png",
		priority = "high",
		line_length = 16,
		width = 43,
		height = 23,
		frame_count = 1,
		direction_count = 16,
		shift = {0.859375, 0.609375},
		hr_version = {
		    filename = "__base__/graphics/entity/defender-robot/hr-defender-robot-shadow.png",
		    priority = "high",
		    line_length = 16,
		    width = 88,
		    height = 50,
		    frame_count = 1,
		    direction_count = 16,
		    shift = util.by_pixel(25.5, 19),
		    scale = 0.5
		}
	    },
	in_motion =
	    {
		layers =
		    {
			{
			    filename = "__base__/graphics/entity/defender-robot/defender-robot.png",
			    priority = "high",
			    line_length = 16,
			    width = 32,
			    height = 33,
			    frame_count = 1,
			    direction_count = 16,
			    shift = {0, 0.015625},
			    y = 33,
			    hr_version = {
				filename = "__base__/graphics/entity/defender-robot/hr-defender-robot.png",
				priority = "high",
				line_length = 16,
				width = 56,
				height = 59,
				frame_count = 1,
				direction_count = 16,
				shift = util.by_pixel(0, 0.25),
				y = 59,
				scale = 0.5
			    }
			},
			{
			    filename = "__base__/graphics/entity/defender-robot/defender-robot-mask.png",
			    priority = "high",
			    line_length = 16,
			    width = 18,
			    height = 16,
			    frame_count = 1,
			    direction_count = 16,
			    shift = {0, -0.125},
			    apply_runtime_tint = true,
			    y = 16,
			    hr_version = {
				filename = "__base__/graphics/entity/defender-robot/hr-defender-robot-mask.png",
				priority = "high",
				line_length = 16,
				width = 28,
				height = 21,
				frame_count = 1,
				direction_count = 16,
				shift = util.by_pixel(0, -4.75),
				apply_runtime_tint = true,
				y = 21,
				scale = 0.5
			    }
			},
		    }
	    },
	shadow_in_motion =
	    {
		filename = "__base__/graphics/entity/defender-robot/defender-robot-shadow.png",
		priority = "high",
		line_length = 16,
		width = 43,
		height = 23,
		frame_count = 1,
		direction_count = 16,
		shift = {0.859375, 0.609375},
		hr_version = {
		    filename = "__base__/graphics/entity/defender-robot/hr-defender-robot-shadow.png",
		    priority = "high",
		    line_length = 16,
		    width = 88,
		    height = 50,
		    frame_count = 1,
		    direction_count = 16,
		    shift = util.by_pixel(25.5, 19),
		    scale = 0.5
		}
	    }
    }
end

function droneUtils.createCapsuleProjectile(attributes)
    return {
	type = "projectile",
	name = "defender2-capsule",
	flags = {"not-on-map"},
	direction_only = true,
	acceleration = 0.005,
	action =
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    show_in_tooltip = true,
				    entity_name = "defender2",
				},
			    }
		    }
	    },
	light = {intensity = 0.5, size = 4},
	enable_drawing_with_mask = true,
	animation = {
	    layers = {
		{
		    filename = "__base__/graphics/entity/combat-robot-capsule/defender-capsule.png",
		    flags = { "no-crop" },
		    frame_count = 1,
		    width = 28,
		    height = 20,
		    priority = "high"
		},
		{
		    filename = "__base__/graphics/entity/combat-robot-capsule/defender-capsule-mask.png",
		    flags = { "no-crop" },
		    frame_count = 1,
		    width = 28,
		    height = 20,
		    priority = "high",
		    apply_runtime_tint = true,
		},
	    },
	},
	shadow =
	    {
		filename = "__base__/graphics/entity/combat-robot-capsule/defender-capsule-shadow.png",
		flags = { "no-crop" },
		frame_count = 1,
		width = 26,
		height = 20,
		priority = "high"
	    },
	smoke = capsule_smoke
    }
end

return droneUtils
