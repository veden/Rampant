local math3d = require "math3d"

local function make_color(r_,g_,b_,a_)
    return { r = r_ * a_, g = g_ * a_, b = b_ * a_, a = a_ }	
end

function foreach(table_, fun_)
    for k, tab in pairs(table_) do fun_(tab) end
    return table_
end

-- biter stream attack
local function createBiterStreamAttack(attributes)

    ----- UTILS
    local function create_burnt_patch_pictures()
	local base = {
	    filename = "__base__/graphics/entity/fire-flame/burnt-patch.png",
	    line_length = 3,
	    width = 115,
	    height = 56,
	    frame_count = 9,
	    axially_symmetrical = false,
	    direction_count = 1,
	    shift = {-0.09375, 0.125},
	}
	
	local variations = {}
	
	for y=1,(base.frame_count / base.line_length) do
	    for x=1,base.line_length do
		table.insert(variations, 
			     { 
				 filename = base.filename,
				 width = base.width,
				 height = base.height,
				 tint = base.tint,
				 shift = base.shift,
				 x = (x-1) * base.width,
				 y = (y-1) * base.height,
		})
	    end
	end

	return variations
    end


    local function create_fire_pictures(opts)
	local fire_blend_mode = opts.blend_mode or "additive"
	local fire_animation_speed = opts.animation_speed or 0.5
	local fire_scale =  opts.scale or 1
	local fire_tint = attributes.firePictureTint
	local fire_flags = { "compressed" }
	local retval = {
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-13.png",
		line_length = 8,
		width = 60,
		height = 118,
		frame_count = 25,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { -0.0390625, -0.90625 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-12.png",
		line_length = 8,
		width = 63,
		height = 116,
		frame_count = 25,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { -0.015625, -0.914065 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-11.png",
		line_length = 8,
		width = 61,
		height = 122,
		frame_count = 25,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { -0.0078125, -0.90625 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-10.png",
		line_length = 8,
		width = 65,
		height = 108,
		frame_count = 25,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { -0.0625, -0.64844 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-09.png",
		line_length = 8,
		width = 64,
		height = 101,
		frame_count = 25,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { -0.03125, -0.695315 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-08.png",
		line_length = 8,
		width = 50,
		height = 98,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { -0.0546875, -0.77344 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-07.png",
		line_length = 8,
		width = 54,
		height = 84,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { 0.015625, -0.640625 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-06.png",
		line_length = 8,
		width = 65,
		height = 92,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { 0, -0.83594 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-05.png",
		line_length = 8,
		width = 59,
		height = 103,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { 0.03125, -0.882815 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-04.png",
		line_length = 8,
		width = 67,
		height = 130,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { 0.015625, -1.109375 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-03.png",
		line_length = 8,
		width = 74,
		height = 117,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { 0.046875, -0.984375 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-02.png",
		line_length = 8,
		width = 74,
		height = 114,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { 0.0078125, -0.96875 }
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/fire-flame-01.png",
		line_length = 8,
		width = 66,
		height = 119,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags,
		shift = { -0.0703125, -1.039065 }
	    },
	}
	return foreach(retval, function(tab)
			   if tab.shift and tab.scale then tab.shift = { tab.shift[1] * tab.scale, tab.shift[2] * tab.scale } end
	end)
    end

    local function create_small_tree_flame_animations(opts)
	local fire_blend_mode = opts.blend_mode or "additive"
	local fire_animation_speed = opts.animation_speed or 0.5
	local fire_scale =  opts.scale or 1
	local fire_tint = attributes.smallTreeFlameTint
	local fire_flags = { "compressed" }
	local retval = {
	    {
		filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-01-a.png",
		line_length = 8,
		width = 38,
		height = 110,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		shift = {-0.03125, -1.5},
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags
	    },
	    {
		filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-01-b.png",
		line_length = 8,
		width = 39,
		height = 111,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		shift = {-0.078125, -1.51562},
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags
	    },
	    {
		filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-01-c.png",
		line_length = 8,
		width = 44,
		height = 108,
		frame_count = 32,
		axially_symmetrical = false,
		direction_count = 1,
		shift = {-0.15625, -1.5},
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-03-a.png",
		line_length = 8,
		width = 38,
		height = 110,
		frame_count = 23,
		axially_symmetrical = false,
		direction_count = 1,
		shift = {-0.03125, -1.5},
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-03-b.png",
		line_length = 8,
		width = 34,
		height = 98,
		frame_count = 23,
		axially_symmetrical = false,
		direction_count = 1,
		shift = {-0.03125, -1.34375},
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags
	    },
	    { 
		filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-03-c.png",
		line_length = 8,
		width = 39,
		height = 111,
		frame_count = 23,
		axially_symmetrical = false,
		direction_count = 1,
		shift = {-0.078125, -1.51562},
		blend_mode = fire_blend_mode,
		animation_speed = fire_animation_speed,
		scale = fire_scale,
		tint = fire_tint,
		flags = fire_flags
	    }
	}
	
	return foreach(retval, function(tab)
			   if tab.shift and tab.scale then tab.shift = { tab.shift[1] * tab.scale, tab.shift[2] * tab.scale } end
	end)
    end

    ------CONTENT -----
    -- type of stream biter spit
    data:extend(
	{
	    {
		type = "stream",
		name = attributes.fireFlameStreamName,
		flags = {"not-on-map"},
		stream_light = {intensity = 1, size = 4},
		ground_light = {intensity = 0.8, size = 4},
		
		smoke_sources =
		    {
			{
			    name = attributes.softSmokeName,
			    frequency = 0.05, --0.25,
			    position = {0.0, 0}, -- -0.8},
			    starting_frame_deviation = 60
			}
		    },
		particle_buffer_size = 90,
		particle_spawn_interval = 1,
		particle_spawn_timeout = attributes.particleTimeout,
		particle_vertical_acceleration = attributes.particleVertialAcceleration,
		particle_horizontal_speed = attributes.particleHoizontalSpeed,
		particle_horizontal_speed_deviation = attributes.particleHoizontalSpeedDeviation,
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
		    },
	    }
	}
    )

    -- the burning animation
    data:extend({
	    {
		type = "fire",
		name = attributes.fireFlameName,
		flags = {"placeable-off-grid", "not-on-map"},
		duration = 600,
		fade_away_duration = 600,
		spread_duration = 600,
		start_scale = 0.20,
		end_scale = 1.0,
		color = attributes.fireFlameTint,
		damage_per_tick = attributes.fireFlameDamagePerTick,
		
		spawn_entity = attributes.spawnEntityName,
		
		spread_delay = 300,
		spread_delay_deviation = 180,
		maximum_spread_count = 100,
		initial_lifetime = 480,
		
		flame_alpha = 0.35,
		flame_alpha_deviation = 0.05,
		
		emissions_per_tick = 0.005,
		
		add_fuel_cooldown = 10,
		increase_duration_cooldown = 10,
		increase_duration_by = 20,
		fade_in_duration = 30,
		fade_out_duration = 30,
		
		lifetime_increase_by = 20,
		lifetime_increase_cooldown = 10,
		delay_between_initial_flames = 10,
		burnt_patch_lifetime = 1800,
		
		on_fuel_added_action =
		    {
			type = "direct",
			action_delivery =
			    {
				type = "instant",
				target_effects =
				    {
					{
					    type = "create-smoke",
					    entity_name = attributes.smokeOnAddingFuel,
					    -- speed = {-0.03, 0},
					    -- speed_multiplier = 0.99,
					    -- speed_multiplier_deviation = 1.1,
					    offset_deviation = {{-0.5, -0.5}, {0.5, 0.5}},
					    speed_from_center = 0.01
					}
				    }
			    }
		    },
		
		pictures = create_fire_pictures({ blend_mode = "normal", animation_speed = 1, scale = 0.5}),
		
		smoke_source_pictures = 
		    {
			{ 
			    filename = "__base__/graphics/entity/fire-flame/fire-smoke-source-1.png",
			    line_length = 8,
			    width = 101,
			    height = 138,
			    frame_count = 31,
			    axially_symmetrical = false,
			    direction_count = 1,
			    shift = {-0.109375, -1.1875},
			    animation_speed = 0.5,
			},
			{ 
			    filename = "__base__/graphics/entity/fire-flame/fire-smoke-source-2.png",
			    line_length = 8,
			    width = 99,
			    height = 138,
			    frame_count = 31,
			    axially_symmetrical = false,
			    direction_count = 1,
			    shift = {-0.203125, -1.21875},
			    animation_speed = 0.5,
			},
		    },
		
		burnt_patch_pictures = create_burnt_patch_pictures(),
		burnt_patch_alpha_default = 0.4,
		burnt_patch_alpha_variations = {
		    -- { tile = "grass", alpha = 0.4 },
		    -- { tile = "grass-medium", alpha = 0.4 },
		    { tile = "grass-dry", alpha = 0.45 },
		    { tile = "dirt", alpha = 0.3 },
		    { tile = "dirt-dark", alpha = 0.35 },
		    { tile = "sand", alpha = 0.24 },
		    { tile = "sand-dark", alpha = 0.28 },
		    { tile = "stone-path", alpha = 0.26 },
		    { tile = "concrete", alpha = 0.24 },
		},

		smoke =
		    {
			{
			    name = attributes.smokeName,
			    deviation = {0.5, 0.5},
			    frequency = 0.25 / 2,
			    position = {0.0, -0.8},
			    starting_vertical_speed = 0.05,
			    starting_vertical_speed_deviation = 0.005,
			    vertical_speed_slowdown = 0.99,
			    starting_frame_deviation = 60,
			    height = -0.5,
			}
		    },
		
		light = {intensity = 1, size = 20},
		
		working_sound =
		    {
			sound = { filename = "__base__/sound/furnace.ogg" },
			max_sounds_per_type = 3
		    },	
		
    }})


    -- what the stream spawns on the ground for persistent damage
    data:extend(
	{
	    {
		type = "sticker",
		name = attributes.stickerName,
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
		
		duration_in_ticks = 30 * 60,
		target_movement_modifier = 0.8,
		damage_per_tick = attributes.stickerDamagePerTick,
		spread_fire_entity = attributes.spawnEntityName,  
		fire_spread_cooldown = 30,
		fire_spread_radius = 0.75,
	    },
    })

    -- types of smoke coming off stream
    local function firesmoke(opts)
	return {
	    type = "smoke",
	    name = opts.name,
	    flags = {"not-on-map"},
	    duration = opts.duration or 600,
	    fade_in_duration = opts.fade_in_duration or 0,
	    fade_away_duration = opts.fade_away_duration or 600,
	    spread_duration = opts.spread_duration or 600,
	    start_scale = opts.start_scale or 0.20,
	    end_scale = opts.end_scale or 1.0,
	    color = opts.color,
	    cyclic = true,
	    affected_by_wind = opts.affected_by_wind or true,
	    animation = opts.animation or
		{
		    width = 152,
		    height = 120,
		    line_length = 5,
		    frame_count = 60,
		    axially_symmetrical = false,
		    direction_count = 1,
		    shift = {-0.53125, -0.4375},
		    priority = "high",
		    flags = { "compressed" },
		    animation_speed = 0.25,
		    filename = "__base__/graphics/entity/smoke/smoke.png"
		},
	    glow_animation = opts.glow_animation,
	    glow_fade_away_duration = opts.glow_fade_away_duration,
	    vertical_speed_slowdown = opts.vertical_speed_slowdown
	}
    end

    data:extend(
	{
	    firesmoke
	    {
		name = attributes.smokeName, 
		color = attributes.smokeTint,
		start_scale = 0.5,
		end_scale = 1,
		duration = 300,
		spread_delay = 120,
		fade_away_duration = 90,
		fade_in_duration = 60,
		animation = 
		    {
			filename = "__base__/graphics/entity/fire-smoke/fire-smoke.png",
			flags = { "compressed" },
			line_length = 8,
			width = 253,
			height = 210,
			frame_count = 60,
			axially_symmetrical = false,
			direction_count = 1,
			shift = {-0.265625, -0.09375},
			priority = "high",
			animation_speed = 0.25,
		    },
		glow_animation = 
		    {
			filename = "__base__/graphics/entity/fire-smoke/fire-smoke-glow.png",
			flags = { "compressed" },
			blend_mode = "additive",
			line_length = 8,
			width = 253,
			height = 152,
			frame_count = 60,
			axially_symmetrical = false,
			direction_count = 1,
			shift = {-0.265625, 0.8125},
			priority = "high",
			animation_speed = 0.25,
		    },
		glow_fade_away_duration = 70
	    },
	    
	    firesmoke
	    {
		name = attributes.smokeWithoutGlowName, 
		color = attributes.smokeWithoutGlowTint,
		start_scale = 0.5,
		end_scale = 1,
		duration = 300,
		spread_delay = 120,
		fade_away_duration = 90,
		fade_in_duration = 60,
		animation = 
		    {
			filename = "__base__/graphics/entity/fire-smoke/fire-smoke.png",
			flags = { "compressed" },
			line_length = 8,
			width = 253,
			height = 210,
			frame_count = 60,
			axially_symmetrical = false,
			direction_count = 1,
			shift = {-0.265625, -0.09375},
			priority = "high",
			animation_speed = 0.25,
		    },
	    },
	    
	    firesmoke
	    {
		name = attributes.softSmokeName, 
		color = attributes.softSmokeTint,
		start_scale = 0.5,
		end_scale = 1.2,
		duration = 300,
		spread_delay = 120,
		fade_away_duration = 60,
	    }, 
	    firesmoke
	    {
		name = attributes.smokeOnAddingFuel, 
		start_scale = 0.5,
		end_scale = 0.7,
		duration = 300,
		spread_delay = 120,
		fade_away_duration = 60,
		fade_in_duration = 60,
		animation = 
		    {
			filename = "__base__/graphics/entity/fire-smoke/fire-smoke.png",
			flags = { "compressed" },
			line_length = 8,
			width = 253,
			height = 210,
			frame_count = 60,
			axially_symmetrical = false,
			direction_count = 1,
			shift = {-0.265625, -0.09375},
			priority = "high",
			animation_speed = 0.25,
		    }
	    },
	}
    )

    -- what happens when attack spreads
    data:extend({
	    {
		type = "fire",
		name = attributes.spawnEntityName,
		flags = {"placeable-off-grid", "not-on-map"},

		damage_per_tick = attributes.spawnEntityDamagePerTick,
		
		spawn_entity = attributes.spawnEntityName,
		maximum_spread_count = 100,
		
		spread_delay = 300,
		spread_delay_deviation = 180,
		flame_alpha = 0.35,
		flame_alpha_deviation = 0.05,
		
		tree_dying_factor = 0.8,
		emissions_per_tick = 0.005,
		
		fade_in_duration = 120,
		fade_out_duration = 100,
		smoke_fade_in_duration = 100,
		smoke_fade_out_duration = 130,
		delay_between_initial_flames = 20,
		
		small_tree_fire_pictures = create_small_tree_flame_animations({ blend_mode = "additive", animation_speed = 0.5, scale = 0.7 * 0.75 }),
		
		pictures = create_fire_pictures({ blend_mode = "additive", animation_speed = 1, scale = 0.5 * 1.25}),
		
		smoke_source_pictures = 
		    {
			{ 
			    filename = "__base__/graphics/entity/fire-flame/fire-smoke-source-1.png",
			    line_length = 8,
			    width = 101,
			    height = 138,
			    frame_count = 31,
			    axially_symmetrical = false,
			    direction_count = 1,
			    scale = 0.6,
			    shift = {-0.109375 * 0.6, -1.1875 * 0.6},
			    animation_speed = 0.5,
			    tint = make_color(0,1,0, 0.75),
			},
			{ 
			    filename = "__base__/graphics/entity/fire-flame/fire-smoke-source-2.png",
			    line_length = 8,
			    width = 99,
			    height = 138,
			    frame_count = 31,
			    axially_symmetrical = false,
			    direction_count = 1,
			    scale = 0.6,
			    shift = {-0.203125 * 0.6, -1.21875 * 0.6},
			    animation_speed = 0.5,
			    tint = make_color(0,1,0, 0.75),
			},
		    },
		
		smoke =
		    {
			{
			    name = attributes.smokeWithoutGlowName,
			    deviation = {0.5, 0.5},
			    frequency = 0.25 / 2,
			    position = {0.0, -0.8},
			    starting_vertical_speed = 0.008,
			    starting_vertical_speed_deviation = 0.05,
			    starting_frame_deviation = 60,
			    height = -0.5,
			}
		    },
		
		light = {intensity = 1, size = 20},

		working_sound =
		    {
			sound = { filename = "__base__/sound/furnace.ogg" },
			max_sounds_per_type = 3
		    },	
    }})    
end



--- biter attacks

createBiterStreamAttack(
    {
	fireFlameStreamName = "green-fire-stream",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 2,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
			{
			    type = "direct",
			    action_delivery =
				{
				    type = "instant",
				    target_effects =
					{
					    {
						type = "create-fire",
						entity_name = "green-fire-flame"
					    }
					}
				}
			},
			{
			    type = "area",
			    perimeter = 2.5,
			    action_delivery =
				{
				    type = "instant",
				    target_effects =
					{
					    {
						type = "create-sticker",
						sticker = "green-fire-sticker"
					    },
					    {
						type = "damage",
						damage = { amount = 1, type = "fire" }
					    }
					}
				}
			}
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "green-fire-flame",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-green-fire-smoke",
	softSmokeTint = make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "green-fire-smoke",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "green-fire-sticker",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "green-fire-smoke-without-glow",
	smokeWithoutGlowTint = make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "green-fire-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "green-fire-smoke-on-adding-fuel"
    }
)
