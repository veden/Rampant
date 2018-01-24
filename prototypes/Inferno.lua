-- imports

local attackFlame = require("utils/AttackFlame")
local biterUtils = require("utils/BiterUtils")
local smokeUtils = require("utils/SmokeUtils")
local swarmUtils = require("SwarmUtils")
local colorUtils = require("utils/ColorUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")
local math3d = require("math3d")

-- constants

local INFERNO_NEST_TIERS = constants.INFERNO_NEST_TIERS
local INFERNO_NEST_VARIATIONS = constants.INFERNO_NEST_VARIATIONS

local INFERNO_WORM_TIERS = constants.INFERNO_WORM_TIERS
local INFERNO_WORM_VARIATIONS = constants.INFERNO_WORM_VARIATIONS

-- imported functions

local makeColor = colorUtils.makeColor

local makeSmokeSoft = smokeUtils.makeSmokeSoft
local makeSmokeWithGlow = smokeUtils.makeSmokeWithGlow
local makeSmokeWithoutGlow = smokeUtils.makeSmokeWithoutGlow
local makeSmokeAddingFuel = smokeUtils.makeSmokeAddingFuel


local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackFlame = attackFlame.createAttackFlame
local createStreamAttack = biterUtils.createStreamAttack

local softSmoke = makeSmokeSoft({name="inferno", softSmokeTint=makeColor(0.3, 0.75, 0.3, 0.1)})
local smokeGlow = makeSmokeWithGlow({name="inferno", smokeWithGlowTint=makeColor(0.3, 0.75, 0.3, 0.1)})
local smokeWithoutGlow = makeSmokeWithoutGlow({name="inferno", smokeWithoutGlowTint=makeColor(0.3, 0.75, 0.3, 0.1)})
local smokeFuel = makeSmokeAddingFuel({name="inferno"})

-- inferno spitters
buildUnitSpawner(
    {
	unit = {
	    name = "inferno-spitter",

	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {
		damageType = "fire",
		softSmokeName = softSmoke,
		smokeWithGlowName = smokeGlow,
		smokeWithoutGlowName = smokeWithoutGlow,
		smokeAddingFuelName = smokeFuel,
		stickerAnimation = { 
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
		    tint = { r = 1, g = 1, b = 1, a = 0.35 },
		    shift = math3d.vector2.mul({-0.078125, -1.8125}, 0.1),
		},
		stickerDamagePerTickType = "fire"
	    },
	    resistances = {},

	    type = "spitter",
	    scales = {
		[1] = 0.5,
		[2] = 0.6,
		[3] = 0.7,
		[4] = 0.8,
		[5] = 0.9,
		[6] = 1,
		[7] = 1.1,
		[8] = 1.2,
		[9] = 1.3,
		[10] = 1.4
	    },
	    attackName = "spitter-inferno",
	    tint = {r=0.65, g=0, b=0, a=0.65},
	    pTint = {r=1, g=1, b=1, a=0.5},
	    sTint = {r=1, g=1, b=1, a=0.5}
	},

	unitSpawner = {
	    name = "inferno-spitter-nest",

	    attributes = {},
	    resistances = {},
	    
	    scales = {
		[1] = 0.5,
		[2] = 0.5,
		[3] = 0.5,
		[4] = 0.5,
		[5] = 0.5,
		[6] = 0.5,
		[7] = 0.5,
		[8] = 0.5,
		[9] = 0.5,
		[10] = 0.5
	    },
	    tint = {r=0.99, g=0.09, b=0.09, a=1}
	}
    },

    {
	unit = {
	    {
		type = "attribute",
		name = "health",
		[1] = 10,
		[2] = 50,
		[3] = 200,
		[4] = 350,
		[5] = 750,
		[6] = 1000,
		[7] = 1500,
		[8] = 1500,
		[9] = 2500,
		[10] = 4500
	    },

	    {
		type = "attack",
		name = "stickerDamagePerTick",
		[1] = 1.6,
		[2] = 1.6,
		[3] = 1.8,
		[4] = 1.8,
		[5] = 2,
		[6] = 2,
		[7] = 2.5,
		[8] = 2.5,
		[9] = 3,
		[10] = 3
	    },

	    {
		type = "attack",
		name = "particleTimeout",
		[1] = 3,
		[2] = 3,
		[3] = 4,
		[4] = 4,
		[5] = 5,
		[6] = 5,
		[7] = 6,
		[8] = 6,
		[9] = 7,
		[10] = 7
	    },

	    {
		type = "attack",
		name = "fireSpreadRadius",
		[1] = 0.75,
		[2] = 0.75,
		[3] = 0.77,
		[4] = 0.77,
		[5] = 0.79,
		[6] = 0.79,
		[7] = 0.83,
		[8] = 0.83,
		[9] = 0.85,
		[10] = 0.85
	    },

	    {
		type = "attack",
		name = "damageMaxMultipler",
		[1] = 6,
		[2] = 6,
		[3] = 7,
		[4] = 7,
		[5] = 8,
		[6] = 8,
		[7] = 9,
		[8] = 9,
		[9] = 10,
		[10] = 10
	    },
	    
	    {
		type = "attack",
		name = "damageMaxMultipler",
		[1] = 6,
		[2] = 6,
		[3] = 7,
		[4] = 7,
		[5] = 8,
		[6] = 8,
		[7] = 9,
		[8] = 9,
		[9] = 10,
		[10] = 10
	    },

	    {
		type = "attack",
		name = "stickerMovementModifier",
		[1] = 1.1,
		[2] = 1.1,
		[3] = 1.1,
		[4] = 1.1,
		[5] = 1.1,
		[6] = 1.1,
		[7] = 1.1,
		[8] = 1.1,
		[9] = 1.1,
		[10] = 1.1
	    },
	    
	    {
		type = "attack",
		name = "fireSpreadCooldown",
		[1] = 30,
		[2] = 30,
		[3] = 29,
		[4] = 29,
		[5] = 28,
		[6] = 28,
		[7] = 27,
		[8] = 27,
		[9] = 25,
		[10] = 25
	    },
	    
	    {
		type = "attack",
		name = "stickerDuration",
		[1] = 1800,
		[2] = 1800,
		[3] = 1900,
		[4] = 1900,
		[5] = 2000,
		[6] = 2000,
		[7] = 2100,
		[8] = 2100,
		[9] = 2200,
		[10] = 2200
	    },
	    
	    {
		type = "attack",
		name = "cooldown",
		[1] = 100,
		[2] = 100,
		[3] = 97,
		[4] = 97,
		[5] = 95,
		[6] = 95,
		[7] = 93,
		[8] = 93,
		[9] = 90,
		[10] = 90
	    },
	    
	    {
		type = "attribute",
		name = "spawningTimeModifer",
		[1] = 0,
		[2] = 0,
		[3] = 1,
		[4] = 2,
		[5] = 3,
		[6] = 7,
		[7] = 10,
		[8] = 10,
		[9] = 12,
		[10] = 12
	    },

	    {
		type = "attribute",
		name = "pollutionToAttack",
		[1] = 200,
		[2] = 750,
		[3] = 1200,
		[4] = 1750,
		[5] = 2500,
		[6] = 5000,
		[7] = 10000,
		[8] = 12500,
		[9] = 15000,
		[10] = 20000
	    },

	    {
		type = "attack",
		name = "damage",
		[1] = 4,
		[2] = 9,
		[3] = 14,
		[4] = 23,
		[5] = 30,
		[6] = 37,
		[7] = 45,
		[8] = 57,
		[9] = 70,
		[10] = 80
	    },
	    
	    {
		type = "attribute",
		name = "healing",
		[1] = 0.01,
		[2] = 0.01,
		[3] = 0.015,
		[4] = 0.02,
		[5] = 0.05,
		[6] = 0.075,
		[7] = 0.1,
		[8] = 0.12,
		[9] = 0.14,
		[10] = 0.16
	    },

	    
	    {
		type = "attribute",
		name = "movement",
		[1] = 0.185,
		[2] = 0.18,
		[3] = 0.18,
		[4] = 0.17,
		[5] = 0.17,
		[6] = 0.16,
		[7] = 0.16,
		[8] = 0.15,
		[9] = 0.15,
		[10] = 0.14
	    },
	    {
		type = "attribute",
		name = "distancePerFrame",
		[1] = 0.04,
		[2] = 0.045,
		[3] = 0.050,
		[4] = 0.055,
		[5] = 0.060,
		[6] = 0.065,
		[7] = 0.070,
		[8] = 0.075,
		[9] = 0.08,
		[10] = 0.084
	    },

	    {
		type = "resistance",
		name = "explosion",
		percent = {
		    [1] = 0,
		    [2] = 0,
		    [3] = 10,
		    [4] = 10,
		    [5] = 20,
		    [6] = 20,
		    [7] = 30,
		    [8] = 30,
		    [9] = 40,
		    [10] = 40
		}
	    },

	    {
		type = "resistance",
		name = "fire",
		decrease = {
		    [1] = 10,
		    [2] = 10,
		    [3] = 14,
		    [4] = 14,
		    [5] = 16,
		    [6] = 16,
		    [7] = 18,
		    [8] = 18,
		    [9] = 20,
		    [10] = 20
		},
		percent = {
		    [1] = 75,
		    [2] = 75,
		    [3] = 80,
		    [4] = 85,
		    [5] = 85,
		    [6] = 90,
		    [7] = 90,
		    [8] = 95,
		    [9] = 95,
		    [10] = 97
		}
	    },
	    
	    {
		type = "attack",
		name = "range",
		[1] = 13,
		[2] = 13,
		[3] = 14,
		[4] = 14,
		[5] = 15,
		[6] = 15,
		[7] = 16,
		[8] = 16,
		[9] = 17,
		[10] = 17
	    },

	    {
		type = "attack",
		name = "radius",
		[1] = 1.2,
		[2] = 1.3,
		[3] = 1.4,
		[4] = 1.5,
		[5] = 1.6,
		[6] = 1.7,
		[7] = 1.8,
		[8] = 1.9,
		[9] = 2.0,
		[10] = 2.5
	    },

	    {
		type = "attack",
		name = "particleVerticalAcceleration",
		[1] = 0.01,
		[2] = 0.01,
		[3] = 0.02,
		[4] = 0.02,
		[5] = 0.03,
		[6] = 0.03,
		[7] = 0.04,
		[8] = 0.04,
		[9] = 0.05,
		[10] = 0.05
	    },

	    {
		type = "attack",
		name = "particleHoizontalSpeed",
		[1] = 0.6,
		[2] = 0.6,
		[3] = 0.7,
		[4] = 0.7,
		[5] = 0.8,
		[6] = 0.8,
		[7] = 0.9,
		[8] = 0.9,
		[9] = 1,
		[10] = 1
	    },

	    {
		type = "attack",
		name = "particleHoizontalSpeedDeviation",
		[1] = 0.0025,
		[2] = 0.0025,
		[3] = 0.0024,
		[4] = 0.0024,
		[5] = 0.0023,
		[6] = 0.0023,
		[7] = 0.0022,
		[8] = 0.0022,
		[9] = 0.0021,
		[10] = 0.0021
	    }
	},
	
	unitSpawner = {

	    {
		type = "attribute",
		name = "health",
		[1] = 350,
		[2] = 500,
		[3] = 750,
		[4] = 1500,
		[5] = 2500,
		[6] = 3500,
		[7] = 5000,
		[8] = 7000,
		[9] = 10000,
		[10] = 15000
	    },

	    {
		type = "attribute",
		name = "healing",
		[1] = 0.02,
		[2] = 0.02,
		[3] = 0.022,
		[4] = 0.024,
		[5] = 0.026,
		[6] = 0.028,
		[7] = 0.03,
		[8] = 0.032,
		[9] = 0.034,
		[10] = 0.036
	    },

	    {
		type = "attribute",
		name = "spawingCooldownStart",
		[1] = 360,
		[2] = 360,
		[3] = 355,
		[4] = 355,
		[5] = 350,
		[6] = 350,
		[7] = 345,
		[8] = 345,
		[9] = 340,
		[10] = 340
	    },

	    {
		type = "attribute",
		name = "spawingCooldownEnd",
		[1] = 150,
		[2] = 150,
		[3] = 145,
		[4] = 145,
		[5] = 140,
		[6] = 140,
		[7] = 135,
		[8] = 135,
		[9] = 130,
		[10] = 130
	    },

	    {
		type = "attribute",
		name = "evolutionRequirement",
		[1] = 0.08,
		[2] = 0.16,
		[3] = 0.24,
		[4] = 0.35,
		[5] = 0.40,
		[6] = 0.45,
		[7] = 0.55,
		[8] = 0.65,
		[9] = 0.75,
		[10] = 0.9
	    },

	    {
		type = "attribute",
		name = "unitsOwned",
		[1] = 7,
		[2] = 7,
		[3] = 8,
		[4] = 8,
		[5] = 9,
		[6] = 9,
		[7] = 10,
		[8] = 10,
		[9] = 11,
		[10] = 11
	    },
	    {
		type = "attribute",
		name = "unitsToSpawn",
		[1] = 5,
		[2] = 5,
		[3] = 6,
		[4] = 6,
		[5] = 7,
		[6] = 7,
		[7] = 8,
		[8] = 8,
		[9] = 9,
		[10] = 9
	    },

	    {
		type = "resistance",
		name = "physical",
		decrease = {
		    [1] = 2,
		    [2] = 2,
		    [3] = 4,
		    [4] = 4,
		    [5] = 6,
		    [6] = 6,
		    [7] = 10,
		    [8] = 12,
		    [9] = 12,
		    [10] = 14
		},
		percent = {
		    [1] = 15,
		    [2] = 15,
		    [3] = 17,
		    [4] = 17,
		    [5] = 18,
		    [6] = 18,
		    [7] = 19,
		    [8] = 19,
		    [9] = 20,
		    [10] = 20
		}
	    },

	    {
		type = "resistance",
		name = "explosion",
		decrease = {
		    [1] = 5,
		    [2] = 5,
		    [3] = 6,
		    [4] = 6,
		    [5] = 7,
		    [6] = 7,
		    [7] = 8,
		    [8] = 8,
		    [9] = 9,
		    [10] = 9
		},
		percent = {
		    [1] = 15,
		    [2] = 15,
		    [3] = 17,
		    [4] = 17,
		    [5] = 18,
		    [6] = 18,
		    [7] = 19,
		    [8] = 19,
		    [9] = 20,
		    [10] = 20
		}
	    },

	    {
		type = "resistance",
		name = "fire",
		decrease = {
		    [1] = 10,
		    [2] = 10,
		    [3] = 14,
		    [4] = 14,
		    [5] = 16,
		    [6] = 16,
		    [7] = 18,
		    [8] = 18,
		    [9] = 20,
		    [10] = 20
		},
		percent = {
		    [1] = 75,
		    [2] = 75,
		    [3] = 80,
		    [4] = 85,
		    [5] = 85,
		    [6] = 90,
		    [7] = 90,
		    [8] = 95,
		    [9] = 95,
		    [10] = 97
		}
	    }	    
	},

	probabilityTable = {
	    [1] = 1,
	    [2] = 1,
	    [3] = 1,
	    [4] = 1,
	    [5] = 1,
	    [6] = 1,
	    [7] = 1,
	    [8] = 1,
	    [9] = 1,
	    [10] = 1,
	}
    },

    function (attributes)
	return createStreamAttack(attributes, createAttackFlame(attributes))
    end,
    
    {
	unit = 10,
	unitSpawner = INFERNO_NEST_VARIATIONS
    },

    {
	unit = 10,
	unitSpawner = INFERNO_NEST_TIERS
    }
)

-- inferno worms
buildWorm(
    {
	name = "inferno-worm",

	attributes = {},
	attack = {
	    damageType = "fire",
	    softSmokeName = softSmoke,
	    smokeWithGlowName = smokeGlow,
	    smokeWithoutGlowName = smokeWithoutGlow,
	    smokeAddingFuelName = smokeFuel,
	    stickerAnimation = { 
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
		tint = { r = 1, g = 1, b = 1, a = 0.35 },
		shift = math3d.vector2.mul({-0.078125, -1.8125}, 0.1),
	    },
	    stickerDamagePerTickType = "fire"
	},
	resistances = {},

	scales = {
	    [1] = 0.5,
	    [2] = 0.6,
	    [3] = 0.7,
	    [4] = 0.8,
	    [5] = 0.9,
	    [6] = 1,
	    [7] = 1.1,
	    [8] = 1.2,
	    [9] = 1.3,
	    [10] = 1.4
	},
	attackName = "worm-inferno",
	tint = {r=0.65, g=0, b=0, a=0.65},
	pTint = {r=1, g=1, b=1, a=0.5},
	sTint = {r=1, g=1, b=1, a=0.5}
    },

    {
	{    
	    type = "attribute",
	    name = "health",
	    [1] = 200,
	    [2] = 350,
	    [3] = 500,
	    [4] = 750,
	    [5] = 900,
	    [6] = 1000,
	    [7] = 1500,
	    [8] = 1500,
	    [9] = 2500,
	    [10] = 4500
	},

	{
	    type = "attack",
	    name = "stickerDamagePerTick",
	    [1] = 1.6,
	    [2] = 1.6,
	    [3] = 1.8,
	    [4] = 1.8,
	    [5] = 2,
	    [6] = 2,
	    [7] = 2.5,
	    [8] = 2.5,
	    [9] = 3,
	    [10] = 3
	},

	{
	    type = "attack",
	    name = "particleTimeout",
	    [1] = 3,
	    [2] = 3,
	    [3] = 4,
	    [4] = 4,
	    [5] = 5,
	    [6] = 5,
	    [7] = 6,
	    [8] = 6,
	    [9] = 7,
	    [10] = 7
	},

	{
	    type = "attack",
	    name = "fireSpreadRadius",
	    [1] = 0.75,
	    [2] = 0.75,
	    [3] = 0.77,
	    [4] = 0.77,
	    [5] = 0.79,
	    [6] = 0.79,
	    [7] = 0.83,
	    [8] = 0.83,
	    [9] = 0.85,
	    [10] = 0.85
	},

	{
	    type = "attack",
	    name = "damageMaxMultipler",
	    [1] = 6,
	    [2] = 6,
	    [3] = 7,
	    [4] = 7,
	    [5] = 8,
	    [6] = 8,
	    [7] = 9,
	    [8] = 9,
	    [9] = 10,
	    [10] = 10
	},
	
	{
	    type = "attack",
	    name = "damageMaxMultipler",
	    [1] = 6,
	    [2] = 6,
	    [3] = 7,
	    [4] = 7,
	    [5] = 8,
	    [6] = 8,
	    [7] = 9,
	    [8] = 9,
	    [9] = 10,
	    [10] = 10
	},

	{
	    type = "attack",
	    name = "stickerMovementModifier",
	    [1] = 1.1,
	    [2] = 1.1,
	    [3] = 1.1,
	    [4] = 1.1,
	    [5] = 1.1,
	    [6] = 1.1,
	    [7] = 1.1,
	    [8] = 1.1,
	    [9] = 1.1,
	    [10] = 1.1
	},
	
	{
	    type = "attack",
	    name = "fireSpreadCooldown",
	    [1] = 30,
	    [2] = 30,
	    [3] = 29,
	    [4] = 29,
	    [5] = 28,
	    [6] = 28,
	    [7] = 27,
	    [8] = 27,
	    [9] = 25,
	    [10] = 25
	},
	
	{
	    type = "attack",
	    name = "stickerDuration",
	    [1] = 1800,
	    [2] = 1800,
	    [3] = 1900,
	    [4] = 1900,
	    [5] = 2000,
	    [6] = 2000,
	    [7] = 2100,
	    [8] = 2100,
	    [9] = 2200,
	    [10] = 2200
	},
	
	{    
	    type = "attack",
	    name = "cooldown",
	    [1] = 50,
	    [2] = 50,
	    [3] = 45,
	    [4] = 45,
	    [5] = 40,
	    [6] = 40,
	    [7] = 35,
	    [8] = 35,
	    [9] = 30,
	    [10] = 30
	},

	{    
	    type = "attribute",
	    name = "evolutionRequirement",
	    [1] = 0,
	    [2] = 0.1,
	    [3] = 0.2,
	    [4] = 0.3,
	    [5] = 0.4,
	    [6] = 0.5,
	    [7] = 0.6,
	    [8] = 0.7,
	    [9] = 0.8,
	    [10] = 0.9
	},
	
	{    
	    type = "attack",
	    name = "damage",
	    [1] = 12,
	    [2] = 20,
	    [3] = 25,
	    [4] = 30,
	    [5] = 35,
	    [6] = 40,
	    [7] = 50,
	    [8] = 60,
	    [9] = 70,
	    [10] = 80
	},
	
	{    
	    type = "attribute",
	    name = "healing",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.015,
	    [4] = 0.02,
	    [5] = 0.05,
	    [6] = 0.075,
	    [7] = 0.1,
	    [8] = 0.12,
	    [9] = 0.14,
	    [10] = 0.16
	},

	{    
	    type = "attribute",
	    name = "prepareRange",
	    [1] = 30,
	    [2] = 30,
	    [3] = 31,
	    [4] = 31,
	    [5] = 32,
	    [6] = 32,
	    [7] = 33,
	    [8] = 33,
	    [9] = 34,
	    [10] = 34
	},
	
	{    
	    type = "attribute",
	    name = "foldingSpeed",
	    [1] = 0.15,
	    [2] = 0.15,
	    [3] = 0.16,
	    [4] = 0.16,
	    [5] = 0.16,
	    [6] = 0.17,
	    [7] = 0.17,
	    [8] = 0.18,
	    [9] = 0.18,
	    [10] = 0.19
	},
	
	{    
	    type = "attribute",
	    name = "preparingSpeed",
	    [1] = 0.025,
	    [2] = 0.025,
	    [3] = 0.026,
	    [4] = 0.026,
	    [5] = 0.027,
	    [6] = 0.027,
	    [7] = 0.028,
	    [8] = 0.028,
	    [9] = 0.029,
	    [10] = 0.029
	},

	{
	    type = "resistance",
	    name = "physical",
	    decrease = {
		[1] = 0,
		[2] = 0,
		[3] = 5,
		[4] = 5,
		[5] = 8,
		[6] = 8,
		[7] = 10,
		[8] = 10,
		[9] = 12,
		[10] = 12
	    }
	},

	{
	    type = "resistance",
	    name = "explosion",
	    decrease = {
		[1] = 0,
		[2] = 0,
		[3] = 5,
		[4] = 5,
		[5] = 8,
		[6] = 8,
		[7] = 10,
		[8] = 10,
		[9] = 12,
		[10] = 12
	    },
	    percent = {
		[1] = 0,
		[2] = 0,
		[3] = 10,
		[4] = 10,
		[5] = 20,
		[6] = 20,
		[7] = 30,
		[8] = 30,
		[9] = 40,
		[10] = 40
	    }
	},

	{
	    type = "resistance",
	    name = "fire",
	    decrease = {
		[1] = 7,
		[2] = 7,
		[3] = 10,
		[4] = 10,
		[5] = 13,
		[6] = 13,
		[7] = 16,
		[8] = 16,
		[9] = 19,
		[10] = 23
	    },
	    percent = {
		[1] = 65,
		[2] = 65,
		[3] = 70,
		[4] = 75,
		[5] = 75,
		[6] = 80,
		[7] = 85,
		[8] = 85,
		[9] = 90,
		[10] = 90
	    }
	},

	{
	    type = "attack",
	    name = "range",
	    [1] = 21,
	    [2] = 22,
	    [3] = 23,
	    [4] = 23,
	    [5] = 24,
	    [6] = 26,
	    [7] = 26,
	    [8] = 28,
	    [9] = 30,
	    [10] = 32
	},

	{
	    type = "attack",
	    name = "radius",
	    [1] = 1.5,
	    [2] = 1.6,
	    [3] = 1.7,
	    [4] = 1.8,
	    [5] = 1.9,
	    [6] = 2.0,
	    [7] = 2.2,
	    [8] = 2.3,
	    [9] = 2.5,
	    [10] = 3.0
	},

	{
	    type = "attack",
	    name = "particleVerticalAcceleration",
	    [1] = 0.01,
	    [2] = 0.01,
	    [3] = 0.02,
	    [4] = 0.02,
	    [5] = 0.03,
	    [6] = 0.03,
	    [7] = 0.04,
	    [8] = 0.04,
	    [9] = 0.05,
	    [10] = 0.05
	},

	{	      
	    type = "attack",
	    name = "particleHoizontalSpeed",
	    [1] = 0.6,
	    [2] = 0.6,
	    [3] = 0.7,
	    [4] = 0.7,
	    [5] = 0.8,
	    [6] = 0.8,
	    [7] = 0.9,
	    [8] = 0.9,
	    [9] = 1,
	    [10] = 1
	},

	{	    
	    type = "attack",
	    name = "particleHoizontalSpeedDeviation",
	    [1] = 0.0025,
	    [2] = 0.0025,
	    [3] = 0.0024,
	    [4] = 0.0024,
	    [5] = 0.0023,
	    [6] = 0.0023,
	    [7] = 0.0022,
	    [8] = 0.0022,
	    [9] = 0.0021,
	    [10] = 0.0021
	}
    },

    function (attributes)
	return createStreamAttack(attributes, createAttackFlame(attributes))
    end,

    INFERNO_WORM_VARIATIONS,
    INFERNO_WORM_TIERS
)
