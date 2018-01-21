local streamUtils = {}

-- imported

local smokeUtils = require("SmokeUtils")

-- imported functions

local makeSmokeSoft = smokeUtils.makeSmokeSoft

-- module code

function streamUtils.makeStream(info)
    local attributes = util.table.deepcopy(info)
    local softSmokeName = attributes.softSmokeName or makeSmokeSoft(attributes)
    data:extend(
	{
	    {
		type = "stream",
		name = attributes.name .. "-stream-rampant",
		flags = {"not-on-map"},
		stream_light = {intensity = 1, size = 4},
		ground_light = {intensity = 0.8, size = 4},
		
		smoke_sources =
		    {
			{
			    name = softSmokeName,
			    frequency = 0.05, --0.25,
			    position = {0.0, 0}, -- -0.8},
			    starting_frame_deviation = 60
			}
		    },
		particle_buffer_size = 90,
		particle_spawn_interval = 1,
		particle_spawn_timeout = attributes.particleTimeout or 1,
		particle_vertical_acceleration = attributes.particleVerticalAcceleration or 0.01,
		particle_horizontal_speed = attributes.particleHoizontalSpeed or 0.6,
		particle_horizontal_speed_deviation = attributes.particleHoizontalSpeedDeviation or 0.0025,
		particle_start_alpha = 0.5,
		particle_end_alpha = 1,
		particle_start_scale = 0.2,
		particle_loop_frame_count = 3,
		particle_fade_out_threshold = 0.9,
		particle_loop_exit_threshold = 0.25,
		action = attributes.actions,
		
		spine_animation = 
		    { 
			filename = "__base__/graphics/entity/flamethrower-fire-stream/flamethrower-fire-stream-spine.png",
			blend_mode = "additive",
			tint = attributes.spineAnimationTint,
			line_length = 4,
			width = 32,
			height = 18,
			frame_count = 32,
			axially_symmetrical = false,
			direction_count = 1,
			animation_speed = 2,
			shift = {0, 0},
		    },
		
		shadow =
		    {
			filename = "__base__/graphics/entity/acid-projectile-purple/acid-projectile-purple-shadow.png",
			line_length = 5,
			width = 28,
			height = 16,
			frame_count = 33,
			priority = "high",
			shift = {-0.09, 0.395}
		    },
		
		particle =
		    {
			filename = "__base__/graphics/entity/flamethrower-fire-stream/flamethrower-explosion.png",
			priority = "extra-high",
			width = 64,
			tint = attributes.particleTint,
			height = 64,
			frame_count = 32,
			line_length = 8
		    }
	    }
	}
    )
end

return streamUtils
