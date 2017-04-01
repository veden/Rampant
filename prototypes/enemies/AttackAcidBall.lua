local biterStreamUtils = require("BiterStreamUtils")

-- dumb acid projectiles
biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-ball-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
	    {
		type = "area",
		perimeter = 1.5,
		action_delivery =
		    {
			{
			    type = "instant",
			    target_effects =
				{
				    {
					type = "damage",
					damage = { amount = 5, type = "acid" }
				    }
				}
			}
		    }
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			type= "create-entity",
			entity_name = "acid-splash-purple"
		    }
		}
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-ball-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-ball-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-ball-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-ball-1-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
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
				    damage = { amount = 15, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			type= "create-entity",
			entity_name = "acid-splash-purple"
		    }
		}
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-ball-1-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-ball-1-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-ball-1-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-ball-1-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-ball-1-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-ball-1-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-ball-1-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-ball-2-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
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
				    damage = { amount = 25, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			type= "create-entity",
			entity_name = "acid-splash-purple"
		    }
		}
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-ball-2-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-ball-2-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-ball-2-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-ball-2-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-ball-2-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-ball-2-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-ball-2-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-ball-3-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
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
				    damage = { amount = 45, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			type= "create-entity",
			entity_name = "acid-splash-purple"
		    }
		}
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-ball-3-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-ball-3-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-ball-3-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-ball-3-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-ball-3-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-ball-3-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-ball-3-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "wide-acid-ball-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
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
				    damage = { amount = 35, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			type= "create-entity",
			entity_name = "acid-splash-purple"
		    }
		}
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "wide-acid-ball-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-wide-acid-ball-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "wide-acid-ball-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "wide-acid-ball-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "wide-acid-ball-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "wide-acid-ball-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "wide-acid-ball-smoke-on-adding-fuel-rampant"
    }
)


biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-ball-4-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
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
				    damage = { amount = 95, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			type= "create-entity",
			entity_name = "acid-splash-purple"
		    }
		}
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-ball-4-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-ball-4-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-ball-4-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-ball-4-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-ball-4-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-ball-4-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-ball-4-smoke-on-adding-fuel-rampant"
    }
)

biterStreamUtils.createBiterStreamAttack(
    {
	fireFlameStreamName = "acid-ball-5-stream-rampant",
	firePictureTint = {r=0.35,g=0.90,b=0,a=1},
	smallTreeFlameTint = {r=0.35,g=0.8,b=0,a=1},
	particleTimeout = 1,
	particleVertialAcceleration = 0.005 * 2,
	particleHoizontalSpeed = 0.2* 0.75 * 4,
	particleHoizontalSpeedDeviation = 0.005 * 0.50,
	particleTint = {r=0, g=1, b=1, a=0.5},
	actions = {
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
				    damage = { amount = 145, type = "acid" }
				},
				{
				    type= "create-entity",
				    entity_name = "acid-splash-purple"
				}
			    }
		    }
	    },
	    {
		type = "direct",
		action_delivery = {
		    type = "instant",
		    target_effects = {
			type= "create-entity",
			entity_name = "acid-splash-purple"
		    }
		}
	    }
	},
	spineAnimationTint = {r=0, g=1, b=1, a=0.5},	
	fireFlameName = "acid-ball-5-flame-rampant",
	fireFlameTint = {r=0, g=0.9, b=0, a=0.5},
	fireFlameDamagePerTick = { amount = 45/60, type = "fire" },
	softSmokeName = "soft-acid-ball-5-smoke-rampant",
	softSmokeTint = biterStreamUtils.make_color(0.3, 0.75, 0.3, 0.1),
	smokeName = "acid-ball-5-smoke-rampant",
	smokeTint = {r=0.2, g=0.8, b=0.2, a=0.25},
	stickerName = "acid-ball-5-sticker-rampant",
	stickerTint = { r = 0.25, g = 0.5, b = 0.25, a = 0.18 },
	stickerDamagePerTick = { amount = 120 / 60, type = "fire" },
	smokeWithoutGlowName = "acid-ball-5-smoke-without-glow-rampant",
	smokeWithoutGlowTint = biterStreamUtils.make_color(0.25,0.75,0.1, 0.25),
	spawnEntityName = "acid-ball-5-flame-on-tree",
	spawnEntityDamagePerTick = { amount = 45/60, type = "fire" },
	smokeOnAddingFuel = "acid-ball-5-smoke-on-adding-fuel-rampant"
    }
)
