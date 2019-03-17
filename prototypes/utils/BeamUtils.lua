local beamUtils = {}

function beamUtils.makeBubble(attributes)
    local name = attributes.name .. "-bubble-rampant"
    data:extend({{
		type = "explosion",
		name = name,
		flags = {"not-on-map"},
		animation_speed = 1,
		animations =
		    {
			{
			    filename = "__base__/graphics/entity/laser-bubble/laser-bubble.png",
			    priority = "extra-high",
			    width = 8,
			    height = 8,
			    frame_count = 5
			}
		    },
		light = {intensity = 1, size = 10, color = attributes.tint or {r = 1.0, g = 1.0, b = 1.0}},
		smoke = "smoke-fast",
		smoke_count = 2,
		smoke_slow_down_factor = 1
    }})
    return name
end

function beamUtils.makeLaser(attributes)
    local name = attributes.name .. "-laser-rampant"
    data:extend({{
		type = "projectile",
		name = name ,
		flags = {"not-on-map"},
		collision_box = attributes.collisionBox or {{-0.3, -1.1}, {0.3, 1.1}},
		acceleration = attributes.acceleration or 0.03,
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
					    entity_name = attributes.bubble or "laser-bubble"
					},
					{
					    type = "damage",
					    damage = { amount = attributes.damage or 5, type = attributes.damageType or "laser"}
					}
				    }
			    }
		    },
		light = {intensity = 0.5, size = 10},
		animation =
		    {
			filename = "__base__/graphics/entity/laser/laser-to-tint-medium.png",
			tint = attributes.tint or {r=1.0, g=0.0, b=0.0},
			frame_count = 1,
			width = 12,
			height = 33,
			priority = "high",
			blend_mode = "additive"
		    },
		speed = 0.15
    }})
    return name
end

function beamUtils.makeBeam(attributes)
    local result =
	{
	    type = "beam",
	    flags = {"not-on-map"},
	    width = attributes.width or 0.5,
	    collision_box = attributes.collisionBox or {{-0.3, -1.1}, {0.3, 1.1}},
	    damage_interval = attributes.damageInterval or 20,
	    action =
		{
		    type = "direct",
		    action_delivery =
			{
			    type = "instant",
			    target_effects =
				{
				    {
					type = "damage",
					damage = { amount = attributes.damage or 10, type = attributes.damageType or "electric"}
				    }
				}
			}
		},
	    start = 
		{ 
		    filename = "__base__/graphics/entity/beam/tileable-beam-START.png",
		    line_length = 4,
		    width = 52,
		    height = 40,
		    frame_count = 16,
		    axially_symmetrical = false,
		    direction_count = 1,
		    shift = {-0.03125, 0},
		    hr_version = {
			filename = "__base__/graphics/entity/beam/hr-tileable-beam-START.png",
			line_length = 4,
			width = 94,
			height = 66,
			frame_count = 16,
			axially_symmetrical = false,
			direction_count = 1,
			shift = {0.53125, 0},
			scale = 0.5,
		    }
		},
	    ending = 
		{ 
		    filename = "__base__/graphics/entity/beam/tileable-beam-END.png",
		    line_length = 4,
		    width = 49,
		    height = 54,
		    frame_count = 16,
		    axially_symmetrical = false,
		    direction_count = 1,
		    shift = {-0.046875, 0},
		    hr_version = { 
			filename = "__base__/graphics/entity/beam/hr-tileable-beam-END.png",
			line_length = 4,
			width = 91,
			height = 93,
			frame_count = 16,
			axially_symmetrical = false,
			direction_count = 1,
			shift = {-0.078125, -0.046875},
			scale = 0.5,
		    } 
		},
	    head =
		{
		    filename = "__base__/graphics/entity/beam/beam-head.png",
		    line_length = 16,
		    width = 45,
		    height = 39,
		    frame_count = 16,
		    animation_speed = 0.5,
		    blend_mode = "additive-soft",
		},
	    tail =
		{
		    filename = "__base__/graphics/entity/beam/beam-tail.png",
		    line_length = 16,
		    width = 45,
		    height = 39,
		    frame_count = 16,
		    blend_mode = "additive-soft",
		},
	    body =
		{
		    {
			filename = "__base__/graphics/entity/beam/beam-body-1.png",
			line_length = 16,
			width = 45,
			height = 39,
			frame_count = 16,
			blend_mode = "additive-soft",
		    },
		    {
			filename = "__base__/graphics/entity/beam/beam-body-2.png",
			line_length = 16,
			width = 45,
			height = 39,
			frame_count = 16,
			blend_mode = "additive-soft",
		    },
		    {
			filename = "__base__/graphics/entity/beam/beam-body-3.png",
			line_length = 16,
			width = 45,
			height = 39,
			frame_count = 16,
			blend_mode = "additive-soft",
		    },
		    {
			filename = "__base__/graphics/entity/beam/beam-body-4.png",
			line_length = 16,
			width = 45,
			height = 39,
			frame_count = 16,
			blend_mode = "additive-soft",
		    },
		    {
			filename = "__base__/graphics/entity/beam/beam-body-5.png",
			line_length = 16,
			width = 45,
			height = 39,
			frame_count = 16,
			blend_mode = "additive-soft",
		    },
		    {
			filename = "__base__/graphics/entity/beam/beam-body-6.png",
			line_length = 16,
			width = 45,
			height = 39,
			frame_count = 16,
			blend_mode = "additive-soft",
		    },
		}
	}

    result.working_sound =
	{
	    {
		filename = "__base__/sound/fight/electric-beam.ogg",
		volume = 0.7
	    }
	}

    local name = attributes.name .. "-beam-rampant"
    result.name = name

    data:extend({result})
    return name
end

return beamUtils
