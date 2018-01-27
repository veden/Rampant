-- bobs replacement attacks

-- import

local streamUtils = require("StreamUtils")
local colorUtils = require("ColorUtils")
local fireUtils = require("FireUtils")
local stickerUtils = require("StickerUtils")

-- imported functions

local makeStream = streamUtils.makeStream
local makeColor = colorUtils.makeColor
local makeSpreadEffect = fireUtils.makeSpreadEffect
local makeFire = fireUtils.makeFire
local makeSticker = stickerUtils.makeSticker

-- module code

local softSmoke = "the-soft-smoke-rampant"
local smokeGlow = "the-glow-smoke-rampant"
local smokeWithoutGlow = "the-without-glow-smoke-rampant"
local smokeFuel = "the-adding-fuel-rampant"

makeStream({
	name = "bob-explosive-ball",
	particleTint = {r=1, g=0.97, b=0.34, a=0.5},
	spineAnimationTint = {r=1, g=0.97, b=0.34, a=0.5},
	softSmokeName = softSmoke,
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "small-scorchmark",
				    check_buildability = true
				},
				{
				    type = "create-entity",
				    entity_name = "big-explosion",
				    check_buildability = true
				},
				{
				    type = "create-entity",
				    entity_name = "small-fire-cloud"
				}
			    }
		    }
	    },
	    {
		type = "area",
		radius = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 25, type = "explosion" }
				}
			    }
		    }
	    }
	}
})

--

local name = "bob-fire-ball"
local spawnEntityName = makeSpreadEffect({
	name = name,
	smokeWithoutGlowName = smokeWithoutGlow
})
local fireName = makeFire({
	name = name,
	fireTint = {r=0, g=0.9, b=0, a=0.5},
	smokeWithGlowName = smokeGlow,
	smokeAddingFuelName = smokeFuel,
	spawnEntityName = spawnEntityName
})
local stickerName = makeSticker({
	name = name,
	spawnEntityName = spawnEntityName
})
makeStream({
	name = name,
	particleTint = {r=1, g=0.17, b=0.17, a=0.5},
	spineAnimationTint = {r=1, g=0.43, b=0.17, a=0.5},
	softSmokeTint = softSmoke,
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
				    entity_name = fireName
				}
			    }
		    }
	    },
	    {
		type = "area",
		radius = 2,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-sticker",
				    sticker = stickerName,
				},
				{
				    type = "damage",
				    damage = { amount = 20, type = "fire" }
				}
			    }
		    }
	    }
	}
})

--

makeStream({
	name = "bob-poison-ball",
	particleTint = {r=0.1, g=0.5, b=1, a=0.5},
	spineAnimationTint = {r=0, g=0, b=1, a=0.5},
	softSmokeName = softSmoke,
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "small-poison-cloud"
				}
			    }
		    }
	    },
	    {
		type = "area",
		radius = 2,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 20, type = "poison" }
				}
			    }
		    }
	    }
	}
})

-- piercing

data:extend({
	{
	    type = "projectile",
	    name = "piercing-spike-rampant",
	    flags = {"not-on-map"},
	    collision_box = {{-0.05, -0.25}, {0.05, 0.25}},
	    acceleration = 0.005,
	    action =
		{
		    type = "direct",
		    action_delivery =
			{
			    type = "instant",
			    target_effects =
				{
				    type = "damage",
				    damage = {amount = 8, type = "bob-pierce"}
				}
			}
		},
	    animation =
		{
		    filename = "__base__/graphics/entity/piercing-bullet/piercing-bullet.png",
		    frame_count = 1,
		    width = 3,
		    height = 50,
		    priority = "high"
		},
	}
})

makeStream({
	name = "bob-piercing-ball",
	particleTint = {r=0.1, g=0.1, b=0.1, a=0.8},
	spineAnimationTint = {r=0.1, g=0.1, b=0.1, a=0.8},
	softSmokeName = softSmoke,
	actions = {
	    {
		type = "cluster",
		cluster_count = 10,
		distance = 4,
		distance_deviation = 3,
		action_delivery =
		    {
			type = "projectile",
			projectile = "piercing-spike-rampant",
			direction_deviation = 0.6,
			starting_speed = 1,
			starting_speed_deviation = 0.0
		    }
	    },
	    {
		type = "area",
		radius = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 30, type = "bob-pierce" }
				}
			    }
		    }
	    }
	}
})

--

data:extend({
	{	
	    type = "projectile",
	    name = "electric-spike-rampant",
	    flags = {"not-on-map"},
	    collision_box = {{-0.03, -0.20}, {0.03, 0.20}},
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
					entity_name = "laser-bubble"
				    },
				    {
					type = "damage",
					damage = { amount = 8, type = "electric"}
				    }
				}
			}
		},
	    light = {intensity = 0.5, size = 10},
	    animation =
		{
		    filename = "__base__/graphics/entity/laser/laser-to-tint-medium.png",
		    tint = {r=0.0, g=0.0, b=1.0},
		    frame_count = 1,
		    width = 12,
		    height = 33,
		    priority = "high",
		    blend_mode = "additive"
		},
	    speed = 0.15
	}
})


makeStream({
	name = "bob-electric-ball",
	particleTint = {r=0, g=0.1, b=1, a=1},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	softSmokeName = softSmoke,
	actions = {
	    {
		type = "cluster",
		cluster_count = 5,
		distance = 2,
		distance_deviation = 2,
		action_delivery =
		    {
			type = "projectile",
			projectile = "electric-spike-rampant",
			direction_deviation = 0.6,
			starting_speed = 0.65,
			starting_speed_deviation = 0.0
		    }
	    },
	    {
		type = "area",
		radius = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 25, type = "electric" }
				}
			    }
		    }
	    }
	}
})

--

makeStream({
	name = "bob-titan-ball",
	particleTint = {r=0, g=0.1, b=1, a=1},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	softSmokeName = softSmoke,
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "small-fire-cloud"
				},
				{
				    type = "create-entity",
				    entity_name = "big-explosion"
				}
			    }
		    }
	    },
	    {
		type = "area",
		radius = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 10, type = "electric" }
				},
				{
				    type = "damage",
				    damage = { amount = 10, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 10, type = "fire" }
				}
			    }
		    }
	    }
	}
})

--

makeStream({
	name = "bob-behemoth-ball",
	particleTint = {r=0, g=0.1, b=1, a=1},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	softSmokeName = softSmoke,
	actions = {
	    {
		type = "direct",
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-entity",
				    entity_name = "small-poison-cloud"
				},
				{
				    type = "create-entity",
				    entity_name = "big-explosion"
				}
			    }
		    }
	    },
	    {
		type = "area",
		radius = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 15, type = "electric" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "fire" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "poison" }
				}
			    }
		    }
	    }
	}
})

--

makeStream({
	name = "bob-leviathan-ball",
	particleTint = {r=0, g=0.1, b=1, a=1},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	softSmokeName = softSmoke,
	actions = {
	    {
		type = "cluster",
		cluster_count = 4,
		distance = 3,
		distance_deviation = 1,
		action_delivery ={
		    type = "instant",
		    target_effects = {
			{
			    type = "create-entity",
			    entity_name = "big-explosion",
			    direction_deviation = 0.6,
			    starting_speed = 1,
			    starting_speed_deviation = 0.0
			}
		    }
		}
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			{
			    type = "create-entity",
			    entity_name = "big-explosion",
			    direction_deviation = 0.6,
			    starting_speed = 1,
			    starting_speed_deviation = 0.0
			}
		    }
		}
	    },
	    {
		type = "area",
		radius = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 15, type = "electric" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "fire" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "poison" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "bob-pierce" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "acid" }
				}
			    }
		    }
	    }
	}
})
