-- bobs replacement attacks

local attacks = {}

-- import

local fireUtils = require("FireUtils")
local stickerUtils = require("StickerUtils")
local attackBall = require("AttackBall")

-- constants

local DISALLOW_FRIENDLY_FIRE = settings.startup["rampant-disallowFriendlyFire"].value

local FORCE_OLD_PROJECTILES = settings.startup["rampant-forceOldProjectiles"].value

-- imported functions

local makeSpreadEffect = fireUtils.makeSpreadEffect
local makeFire = fireUtils.makeFire
local makeSticker = stickerUtils.makeSticker

local createAttackBall = attackBall.createAttackBall

-- module code

local softSmoke = "the-soft-smoke-rampant"
local smokeGlow = "the-glow-smoke-rampant"
local smokeWithoutGlow = "the-without-glow-smoke-rampant"
local smokeFuel = "the-adding-fuel-rampant"

function attacks.addAttacks()

    local multipler = (FORCE_OLD_PROJECTILES and 1) or 2.7 

    createAttackBall(
	{
	    name = "bob-explosive-ball",
	    pTint = {r=1, g=0.97, b=0.34, a=0.5},
	    sTint = {r=1, g=0.97, b=0.34, a=0.5},
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return {
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
	    end,
	    radius = 3,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "damage",
			    damage = { amount = 25 * multipler, type = "explosion" }
			}
		    }
	    end	    
	}
    )

    if not FORCE_OLD_PROJECTILES then
	createAttackBall(
	    {
		name = "bob-explosive-ball-direction",
		pTint = {r=1, g=0.97, b=0.34, a=0.5},
		sTint = {r=1, g=0.97, b=0.34, a=0.5},
		softSmokeName = softSmoke,
		directionOnly = true,
		type = "projectile",
		pointEffects = function (attributes)
		    return {
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
		end,
		radius = 3,
		areaEffects = function (attributes)
		    return 
			{
			    {
				type = "damage",
				damage = { amount = 25  * multipler, type = "explosion" }
			    }
			}
		end	    
	    }
	)
    end

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
    createAttackBall(
	{
	    name = name,
	    pTint = {r=1, g=0.17, b=0.17, a=0.5},
	    sTint = {r=1, g=0.43, b=0.17, a=0.5},
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return {
		    {
			type = "create-fire",
			entity_name = fireName
		    }
		}
	    end,
	    radius = 2,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "create-sticker",
			    sticker = stickerName,
			},
			{
			    type = "damage",
			    damage = { amount = 20 * multipler, type = "fire" }
			}
		    }
	    end	    
	}
    )

    if not FORCE_OLD_PROJECTILES then
	name = "bob-fire-ball-direction"
	createAttackBall(
	    {
		name = name,
		pTint = {r=1, g=0.17, b=0.17, a=0.5},
		sTint = {r=1, g=0.43, b=0.17, a=0.5},
		softSmokeName = softSmoke,
		directionOnly = true,
		type = "projectile",
		pointEffects = function (attributes)
		    return {
			{
			    type = "create-fire",
			    entity_name = fireName
			}
		    }
		end,
		radius = 2,
		areaEffects = function (attributes)
		    return 
			{
			    {
				type = "create-sticker",
				sticker = stickerName,
			    },
			    {
				type = "damage",
				damage = { amount = 20 * multipler, type = "fire" }
			    }
			}
		end	    
	    }
	)
    end

    --

    createAttackBall(
	{
	    name = "bob-poison-ball",
	    pTint = {r=0.1, g=0.5, b=1, a=0.5},
	    sTint = {r=0, g=0, b=1, a=0.5},	
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return {
		    {
			type = "create-entity",
			entity_name = "small-poison-cloud"
		    }
		}
	    end,
	    radius = 2,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "damage",
			    damage = { amount = 20 * multipler, type = "poison" }
			}
		    }
	    end	    
	}
    )

    if not FORCE_OLD_PROJECTILES then
	createAttackBall(
	    {
		name = "bob-poison-ball-direction",
		pTint = {r=0.1, g=0.5, b=1, a=0.5},
		sTint = {r=0, g=0, b=1, a=0.5},	
		softSmokeName = softSmoke,
		directionOnly = true,
		type = "projectile",
		pointEffects = function (attributes)
		    return {
			{
			    type = "create-entity",
			    entity_name = "small-poison-cloud"
			}
		    }
		end,
		radius = 2,
		areaEffects = function (attributes)
		    return 
			{
			    {
				type = "damage",
				damage = { amount = 20 * multipler, type = "poison" }
			    }
			}
		end	    
	    }
	)
    end

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
			force = (DISALLOW_FRIENDLY_FIRE and "enemy") or nil,
			action_delivery =
			    {
				type = "instant",
				target_effects =
				    {
					type = "damage",
					damage = {amount = 8 * multipler, type = "bob-pierce"}
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

    createAttackBall(
	{
	    name = "bob-piercing-ball",
	    pTint = {r=0.1, g=0.1, b=0.1, a=0.8},
	    sTint = {r=0.1, g=0.1, b=0.1, a=0.8},	
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return 
		    {
			type = "nested-result",
			action = {
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
			}
		    }
	    end,
	    radius = 3,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "damage",
			    damage = { amount = 30 * multipler, type = "bob-pierce" }
			}
		    }
	    end	    
	}
    )

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
			force = (DISALLOW_FRIENDLY_FIRE and "enemy") or nil,
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
					    damage = { amount = 8 * multipler, type = "electric"}
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

    createAttackBall(
	{
	    name = "bob-electric-ball",
	    pTint = {r=0, g=0.1, b=1, a=1},
	    sTint = {r=0, g=0.1, b=1, a=1},	
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return 
		    {
			type = "nested-result",
			action = {
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
			}
		    }
	    end,
	    radius = 3,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "damage",
			    damage = { amount = 25 * multipler, type = "electric" }
			}
		    }
	    end	    
	}
    )

    if not FORCE_OLD_PROJECTILES then
	createAttackBall(
	    {
		name = "bob-electric-ball-direction",
		pTint = {r=0, g=0.1, b=1, a=1},
		sTint = {r=0, g=0.1, b=1, a=1},	
		softSmokeName = softSmoke,
		directionOnly = true,
		type = "projectile",
		pointEffects = function (attributes)
		    return 
			{
			    type = "nested-result",
			    action = {
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
			    }
			}
		end,
		radius = 3,
		areaEffects = function (attributes)
		    return 
			{
			    {
				type = "damage",
				damage = { amount = 25 * multipler, type = "electric" }
			    }
			}
		end	    
	    }
	)
    end

    --

    createAttackBall(
	{
	    name = "bob-titan-ball",
	    pTint = {r=0, g=0.1, b=1, a=1},
	    sTint = {r=0, g=0.1, b=1, a=1},	
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return 
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
	    end,
	    radius = 3,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "damage",
			    damage = { amount = 10 * multipler, type = "electric" }
			},
			{
			    type = "damage",
			    damage = { amount = 10 * multipler, type = "explosion" }
			},
			{
			    type = "damage",
			    damage = { amount = 10 * multipler, type = "fire" }
			}
		    }
	    end	    
	}
    )

    if not FORCE_OLD_PROJECTILES then
	createAttackBall(
	    {
		name = "bob-titan-ball-direction",
		pTint = {r=0, g=0.1, b=1, a=1},
		sTint = {r=0, g=0.1, b=1, a=1},	
		softSmokeName = softSmoke,
		type = "projectile",
		pointEffects = function (attributes)
		    return 
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
		end,
		radius = 3,
		areaEffects = function (attributes)
		    return 
			{
			    {
				type = "damage",
				damage = { amount = 10 * multipler, type = "electric" }
			    },
			    {
				type = "damage",
				damage = { amount = 10 * multipler, type = "explosion" }
			    },
			    {
				type = "damage",
				damage = { amount = 10 * multipler, type = "fire" }
			    }
			}
		end	    
	    }
	)
    end

    --

    createAttackBall(
	{
	    name = "bob-behemoth-ball",
	    pTint = {r=0, g=0.1, b=1, a=1},
	    sTint = {r=0, g=0.1, b=1, a=1},	
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return 
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
	    end,
	    radius = 3,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "electric" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "explosion" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "fire" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "poison" }
			}
		    }
	    end	    
	}
    )

    if not FORCE_OLD_PROJECTILES then
	createAttackBall(
	    {
		name = "bob-behemoth-ball-direction",
		pTint = {r=0, g=0.1, b=1, a=1},
		sTint = {r=0, g=0.1, b=1, a=1},	
		softSmokeName = softSmoke,
		directionOnly = true,
		type = "projectile",
		pointEffects = function (attributes)
		    return 
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
		end,
		radius = 3,
		areaEffects = function (attributes)
		    return 
			{
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "electric" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "explosion" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "fire" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "poison" }
			    }
			}
		end	    
	    }
	)
    end

    --

    createAttackBall(
	{
	    name = "bob-leviathan-ball",
	    pTint = {r=0, g=0.1, b=1, a=1},
	    sTint = {r=0, g=0.1, b=1, a=1},	
	    softSmokeName = softSmoke,
	    type = "projectile",
	    pointEffects = function (attributes)
		return 
		    {
			type = "nested-result",
			action = 
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
			    }
		    }
	    end,
	    radius = 3,
	    areaEffects = function (attributes)
		return 
		    {
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "electric" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "explosion" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "fire" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "poison" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "bob-pierce" }
			},
			{
			    type = "damage",
			    damage = { amount = 15 * multipler, type = "acid" }
			}
		    }
	    end	    
	}
    )

    if not FORCE_OLD_PROJECTILES then
	createAttackBall(
	    {
		name = "bob-leviathan-ball-direction",
		pTint = {r=0, g=0.1, b=1, a=1},
		sTint = {r=0, g=0.1, b=1, a=1},	
		softSmokeName = softSmoke,
		directionOnly = true,
		type = "projectile",
		pointEffects = function (attributes)
		    return 
			{
			    type = "nested-result",
			    action = 
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
				}
			}
		end,
		radius = 3,
		areaEffects = function (attributes)
		    return 
			{
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "electric" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "explosion" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "fire" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "poison" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "bob-pierce" }
			    },
			    {
				type = "damage",
				damage = { amount = 15 * multipler, type = "acid" }
			    }
			}
		end	    
	    }
	)
    end
end

return attacks
