local biterStreamUtils = require("BiterStreamUtils")

-- persistent flame attacks
biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-flame-fire-stream-rampant",
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
				    entity_name = "acid-flame-fire-flame-rampant"
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
				    sticker = "acid-flame-fire-sticker-rampant"
				},
				{
				    type = "damage",
				    damage = { amount = 1, type = "fire" }
				},
				{
				    type = "damage",
				    damage = { amount = 1, type = "acid" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-flame-fire-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-flame-fire-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-flame-fire-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-flame-fire-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-flame-fire-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-flame-fire-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-flame-fire-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-flame-1-fire-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 4,
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
				    entity_name = "acid-flame-1-fire-flame-rampant"
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
				    sticker = "acid-flame-1-fire-sticker-rampant"
				},
				{
				    type = "damage",
				    damage = { amount = 1, type = "fire" }
				},
				{
				    type = "damage",
				    damage = { amount = 2, type = "acid" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-flame-1-fire-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-flame-1-fire-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-flame-1-fire-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-flame-1-fire-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-flame-1-fire-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-flame-1-fire-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-flame-1-fire-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-flame-2-fire-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 8,
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
				    entity_name = "acid-flame-2-fire-flame-rampant"
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
				    sticker = "acid-flame-2-fire-sticker-rampant"
				},
				{
				    type = "damage",
				    damage = { amount = 1, type = "fire" }
				},
				{
				    type = "damage",
				    damage = { amount = 2, type = "acid" }
				}
			    }
		    }
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-flame-2-fire-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-flame-2-fire-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-flame-2-fire-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-flame-2-fire-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-flame-2-fire-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-flame-2-fire-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-flame-2-fire-smoke-on-adding-fuel-rampant"
    }
)
