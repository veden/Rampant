local biterStreamUtils = require("BiterStreamUtils")

-- bobs replacement attacks

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-explosive-ball-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=1, g=0.97, b=0.34, a=0.5},
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
		perimeter = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 35, type = "explosion" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=1, g=0.97, b=0.34, a=0.5},	
	fireFlameName = "bob-explosive-ball-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-explosive-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "bob-explosive-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "bob-explosive-ball-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-explosive-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "bob-explosive-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-explosive-ball-smoke-on-adding-fuel-rampant"
    }
)


biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-explosive-ball-1-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=1, g=0.97, b=0.34, a=0.5},
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
		perimeter = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 55, type = "explosion" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=1, g=0.97, b=0.34, a=0.5},	
	fireFlameName = "bob-explosive-ball-1-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-explosive-ball-1-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "bob-explosive-ball-1-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "bob-explosive-ball-1-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-explosive-ball-1-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "bob-explosive-ball-1-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-explosive-ball-1-smoke-on-adding-fuel-rampant"
    }
)

-- fire

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-fire-ball-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=1, g=0.17, b=0.17, a=0.5},
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
				    entity_name = "bob-fire-ball-flame-rampant"
				}
			    }
		    }
	    },
	    {
		type = "area",
		perimeter = 2,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "create-sticker",
				    sticker = "bob-fire-ball-sticker-rampant",
				},
				{
				    type = "damage",
				    damage = { amount = 43, type = "fire" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=1, g=0.43, b=0.17, a=0.5},	
	fireFlameName = "bob-fire-ball-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-fire-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-fire-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-fire-ball-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-fire-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-fire-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-fire-ball-smoke-on-adding-fuel-rampant"
    }
)

-- poison

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-poison-ball-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0.1, g=0.5, b=1, a=0.5},
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
		perimeter = 2,
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
	},
	spineAnimationTint = {r=0, g=0, b=1, a=0.5},
	fireFlameName = "bob-poison-ball-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-poison-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-poison-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-poison-ball-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-poison-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-poison-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-poison-ball-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-poison-ball-1-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0.1, g=0.5, b=1, a=0.5},
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
		perimeter = 2,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 43, type = "poison" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=0, b=1, a=0.5},
	fireFlameName = "bob-poison-ball-1-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-poison-ball-1-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-poison-ball-1-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-poison-ball-1-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-poison-ball-1-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-poison-ball-1-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-poison-ball-1-smoke-on-adding-fuel-rampant"
    }
)

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

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-piercing-ball-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0.1, g=0.1, b=0.1, a=0.8},
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
		perimeter = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 43, type = "bob-pierce" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0.1, g=0.1, b=0.1, a=0.8},
	fireFlameName = "bob-piercing-ball-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-piercing-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-piercing-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-piercing-ball-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-piercing-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-piercing-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-piercing-ball-smoke-on-adding-fuel-rampant"
    }
)

-- electric

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

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-electric-ball-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0, g=0.1, b=1, a=1},
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
		perimeter = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 10, type = "electric" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	fireFlameName = "bob-electric-ball-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-electric-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-electric-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-electric-ball-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-electric-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-electric-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-electric-ball-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-electric-ball-1-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0, g=0.1, b=1, a=1},
	actions = {
	    {
		type = "cluster",
		cluster_count = 10,
		distance = 4,
		distance_deviation = 3,
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
		perimeter = 3,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 55, type = "electric" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	fireFlameName = "bob-electric-ball-1-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-electric-ball-1-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-electric-ball-1-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-electric-ball-1-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-electric-ball-1-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-electric-ball-1-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-electric-ball-1-smoke-on-adding-fuel-rampant"
    }
)

-- titan

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-titan-ball-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0, g=0.1, b=1, a=1},
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
		perimeter = 3,
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
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	fireFlameName = "bob-titan-ball-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-titan-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-titan-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-titan-ball-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-titan-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-titan-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-titan-ball-smoke-on-adding-fuel-rampant"
    }
)

-- behemoth

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-behemoth-ball-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0, g=0.1, b=1, a=1},
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
		perimeter = 3,
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
	},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	fireFlameName = "bob-behemoth-ball-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-behemoth-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-behemoth-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-behemoth-ball-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-behemoth-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-behemoth-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-behemoth-ball-smoke-on-adding-fuel-rampant"
    }
)

-- leviathan

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "bob-leviathan-ball-stream-rampant",
	firePictureTint = {r=1,g=1,b=1,a=1},
	smallTreeFlameTint = {r=1,g=1,b=1,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 0.60,
	particleHoizontalSpeed = 0.2* 0.75 * 1.5,
	particleHoizontalSpeedDeviation = 0.005 * 0.70,
	particleTint = {r=0, g=0.1, b=1, a=1},
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
		perimeter = 3,
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
				    damage = { amount = 20, type = "explosion" }
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
				    damage = { amount = 20, type = "acid" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=0.1, b=1, a=1},
	fireFlameName = "bob-leviathan-ball-flame-rampant",
	fireFlameTint = {r=1, g=0.64, b=0.05, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-bob-leviathan-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.7, 0.4, 0.2, 0.1),
	smokeName = "bob-leviathan-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.2, b=0.2, a=0.25},
	stickerName = "bob-leviathan-ball-sticker-rampant",
	stickerTint = { r = 0.45, g = 0.25, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "bob-leviathan-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.45,0.25,0.1, 0.25),
	spawnEntityName = "bob-leviathan-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "bob-leviathan-ball-smoke-on-adding-fuel-rampant"
    }
)
