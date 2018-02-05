local util = require ("util")

local droneUtils = {}

function droneUtils.makeDrone(name, attributes, biterResistances, biterAttack, biterDeath)
    local n = name .. "-drone-rampant"
    local resistances = {}
    for k,v in pairs(biterResistances) do
	v.type = k
	resistances[#resistances+1] = v
    end    
    
    local drone = {
	type = "combat-robot",
	name = n,
	icon = "__base__/graphics/icons/defender.png",
	icon_size = 32,
	flags = {"placeable-player", "player-creation", "placeable-off-grid", "not-on-map", "not-repairable", "breaths-air"},
	subgroup="capsule",
	order="e-a-a",
	max_health = attributes.health or 60,
	healing_per_tick = attributes.healing,
	alert_when_damaged = false,
	collision_box = {{0, 0}, {0, 0}},
	selection_box = {{-0.5, -1.5}, {0.5, -0.5}},
	distance_per_frame = attributes.distancePerFrame or 0.13,
	time_to_live = attributes.ttl or (60 * 45),
	follows_player = attributes.followsPlayer or false,
	friction = attributes.friction or 0.01,
	range_from_player = attributes.rangeFromPlayer or 6.0,
	speed = attributes.movement or 0.01,
	destroy_action = biterDeath,
	attack_parameters = biterAttack,
	idle =
	    {
		layers =
		    {
			{
			    filename = "__base__/graphics/entity/defender-robot/defender-robot.png",
			    priority = "high",
			    line_length = 16,
			    width = 32,
			    tint = attributes.dTint or {r=1.0, g=0.0, b=0.0},
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
				tint = attributes.dTint or {r=1.0, g=0.0, b=0.0},
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
			    tint = attributes.dTint or {r=1.0, g=0.0, b=0.0},
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
				tint = attributes.dTint or {r=1.0, g=0.0, b=0.0},
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
			    tint = attributes.dTint or {r=1.0, g=0.0, b=0.0},
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
				tint = attributes.dTint or {r=1.0, g=0.0, b=0.0},
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
    
    return drone
end

function droneUtils.createCapsuleProjectile(attributes, entityName)
    local name = attributes.name .. "-capsule-rampant"
    
    data:extend({{
		type = "projectile",
		name = name,
		flags = {"not-on-map"},
		collision_box = {{-0.3, -1.1}, {0.3, 1.1}},
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
					    entity_name = entityName,
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
		smoke = {
		    {
			name = attributes.softSmokeName,
			deviation = {0.15, 0.15},
			frequency = 1,
			position = {0, 0},
			starting_frame = 3,
			starting_frame_deviation = 5,
			starting_frame_speed_deviation = 5
		    }
		}
    }})
    return name
end

return droneUtils
