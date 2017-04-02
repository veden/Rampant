local biterStreamUtils = require("BiterStreamUtils")

-- NE replacement attacks
-- unit launchers
biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "ne-infected-unit-ball-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=0.97, b=0.34, a=0.5},
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
				    entity_name = "unit-cluster",
				    trigger_created_entity = "true"
				},
				{
				    type = "create-sticker",
				    sticker = "slowdown-sticker",
				},
				{
				    type = "create-entity",
				    entity_name = "Infected-Poison-Cloud"
				},
				{
				    type = "damage",
				    damage = {amount = 10, type = "explosion"}
				},
				{
				    type = "damage",
				    damage = {amount = 25, type = "poison"}
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=0.57, b=0.34, a=0.5},	
	fireFlameName = "ne-infected-unit-ball-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-ne-infected-unit-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "ne-infected-unit-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "ne-infected-unit-ball-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "ne-infected-unit-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "ne-infected-unit-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "ne-infected-unit-ball-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "ne-mutated-unit-ball-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0.5, g=0.7, b=0.34, a=0.5},
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
				    entity_name = "unit-cluster",
				    trigger_created_entity = "true"
				},
				{
				    type = "create-sticker",
				    sticker = "slowdown-sticker",
				},
				{
				    type = "create-entity",
				    entity_name = "acid-splash-purple"
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
				    damage = { amount = 5, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 15, type = "acid" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0.5, g=0.97, b=0.34, a=0.5},	
	fireFlameName = "ne-mutated-unit-ball-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-ne-mutated-unit-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "ne-mutated-unit-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "ne-mutated-unit-ball-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "ne-mutated-unit-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "ne-mutated-unit-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "ne-mutated-unit-ball-smoke-on-adding-fuel-rampant"
    }
)

-- standard attack

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "ne-infected-ball-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=0.97, b=0.34, a=0.5},
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
				    entity_name = "Infected-Poison-Cloud"
				}
			    }
		    }
	    },
	    {
		type = "area",
		perimeter = 1.5,
		action_delivery =
		    {
			type = "instant",
			target_effects =
			    {
				{
				    type = "damage",
				    damage = { amount = 3, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 7, type = "poison" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=1, g=0.97, b=0.34, a=0.5},	
	fireFlameName = "ne-infected-ball-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-ne-infected-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "ne-infected-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "ne-infected-ball-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "ne-infected-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "ne-infected-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "ne-infected-ball-smoke-on-adding-fuel-rampant"
    }
)


biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "ne-mutated-ball-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0.5, g=0.7, b=0.34, a=0.5},
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
				    entity_name = "acid-splash-purple"
				},
				{
				    type = "create-entity",
				    entity_name = "acid-splash-mutated"
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
				    damage = { amount = 7, type = "explosion" }
				},
				{
				    type = "damage",
				    damage = { amount = 14, type = "acid" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0.5, g=0.97, b=0.34, a=0.5},	
	fireFlameName = "ne-mutated-ball-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-ne-mutated-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "ne-mutated-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "ne-mutated-ball-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "ne-mutated-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "ne-mutated-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "ne-mutated-ball-smoke-on-adding-fuel-rampant"
    }
)
