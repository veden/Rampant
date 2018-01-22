-- imports

local physicalBall = require("utils/AttackAcidBall")
local biterUtils = require("utils/BiterUtils")
local smokeUtils = require("utils/SmokeUtils")
local swarmUtils = require("SwarmUtils")
local colorUtils = require("utils/ColorUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local PHYSICAL_NEST_TIERS = constants.PHYSICAL_NEST_TIERS
local PHYSICAL_NEST_VARIATIONS = constants.PHYSICAL_NEST_VARIATIONS

local PHYSICAL_WORM_TIERS = constants.PHYSICAL_WORM_TIERS
local PHYSICAL_WORM_VARIATIONS = constants.PHYSICAL_WORM_VARIATIONS

-- imported functions

local makeColor = colorUtils.makeColor

local makeSmokeSoft = smokeUtils.makeSmokeSoft

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAcidBall = physicalBall.createAcidBall
local createFireAttack = biterUtils.createFireAttack
local createMeleeAttack = biterUtils.createMeleeAttack

makeSmokeSoft({name="physical", softSmokeTint=makeColor(0.75, 0.75, 0.75, 0.1)})

-- physical biters
buildUnitSpawner(
    {
	unit = {
	    name = "physical-biter",

	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {

	    },
	    resistances = {},

	    type = "biter",
	    scales = {
		[1] = 0.7,
		[2] = 0.8,
		[3] = 0.9,
		[4] = 1,
		[5] = 1.1,
		[6] = 1.2,
		[7] = 1.3,
		[8] = 1.4,
		[9] = 1.5,
		[10] = 1.6
	    },
	    tint1 = {r=0.85, g=0.85, b=0.83, a=0.65},
	    tint2 = {r=0.85, g=0.85, b=0.83, a=0.65}
	},

	unitSpawner = {
	    name = "physical-biter-nest",

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
	    tint = {r=0.85, g=0.85, b=0.83, a=0.65}
	}
    },

    {
	unit = {
	    
	    {
		type = "attribute",
		name = "health",
		[1] = 30,
		[2] = 150,
		[3] = 300,
		[4] = 500,
		[5] = 800,
		[6] = 1500,
		[7] = 3000,
		[8] = 6000,
		[9] = 10000,
		[10] = 20000
	    },
	    
	    {
		type = "attack",
		name = "cooldown",
		[1] = 35,
		[2] = 35,
		[3] = 35,
		[4] = 35,
		[5] = 35,
		[6] = 35,
		[7] = 50,
		[8] = 50,
		[9] = 55,
		[10] = 57
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
		[3] = 1750,
		[4] = 3500,
		[5] = 5000,
		[6] = 10000,
		[7] = 20000,
		[8] = 25000,
		[9] = 30000,
		[10] = 40000
	    },

	    {
		type = "attack",
		name = "damage",
		[1] = 7,
		[2] = 15,
		[3] = 22.5,
		[4] = 35,
		[5] = 45,
		[6] = 60,
		[7] = 75,
		[8] = 90,
		[9] = 150,
		[10] = 200
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
		[1] = 0.17,
		[2] = 0.17,
		[3] = 0.1675,
		[4] = 0.1675,
		[5] = 0.165,
		[6] = 0.165,
		[7] = 0.16,
		[8] = 0.16,
		[9] = 0.1575,
		[10] = 0.1575
	    },

	    {
		type = "attribute",
		name = "distancePerFrame",
		[1] = 0.075,
		[2] = 0.075,
		[3] = 0.08,
		[4] = 0.08,
		[5] = 0.085,
		[6] = 0.085,
		[7] = 0.09,
		[8] = 0.09,
		[9] = 0.1,
		[10] = 0.1
	    },

	    {
		type = "resistance",
		name = "explosion",
		decrease = {
		    [1] = 0,
		    [2] = 0,
		    [3] = 0,
		    [4] = 0,
		    [5] = 0,
		    [6] = 0,
		    [7] = 10,
		    [8] = 12,
		    [9] = 14,
		    [10] = 15
		},
		percent = {
		    [1] = 0,
		    [2] = 0,
		    [3] = 0,
		    [4] = 10,
		    [5] = 12,
		    [6] = 12,
		    [7] = 13,
		    [8] = 13,
		    [9] = 14,
		    [10] = 15
		}
	    },
	    
	    {
		type = "resistance",
		name = "physical",
		decrease = {
		    [1] = 5,
		    [2] = 5,
		    [3] = 7,
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
		[1] = 0.5,
		[2] = 0.5,
		[3] = 0.75,
		[4] = 0.75,
		[5] = 1.0,
		[6] = 1.0,
		[7] = 1.25,
		[8] = 1.50,
		[9] = 1.75,
		[10] = 2.0
	    }
	},
	
	unitSpawner = {
	    {
		type = "attribute",
		name = "health",
		[1] = 700,
		[2] = 1000,
		[3] = 1500,
		[4] = 3000,
		[5] = 5000,
		[6] = 7000,
		[7] = 10000,
		[8] = 14000,
		[9] = 20000,
		[10] = 30000
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
		    [1] = 3,
		    [2] = 3,
		    [3] = 4,
		    [4] = 4,
		    [5] = 6,
		    [6] = 6,
		    [7] = 6,
		    [8] = 6,
		    [9] = 7,
		    [10] = 7
		},
		percent = {
		    [1] = 60,
		    [2] = 60,
		    [3] = 62,
		    [4] = 62,
		    [5] = 63,
		    [6] = 63,
		    [7] = 64,
		    [8] = 64,
		    [9] = 65,
		    [10] = 65
		}
	    },

	    {
		type = "resistance",
		name = "physical",
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

    createMeleeAttack,

    {
	unit = 10,
	unitSpawner = PHYSICAL_NEST_VARIATIONS
    },

    {
	unit = 10,
	unitSpawner = PHYSICAL_NEST_TIERS
    }
)

-- physical worms
buildWorm(
    {
	name = "physical-worm",

	attributes = {
	},
	attack = {
	    damageType = "physical",
	    pointEffects = function (attributes)
		return {
		    {
			type= "create-entity",
			entity_name = "small-scorchmark"
		    },
		    {
			type= "create-entity",
			entity_name = "explosion"
		    }
		}
	    end,
	    softSmokeName = "physical-soft-smoke-rampant"
	},
	resistances = {},

	scales = {
	    [1] = 0.7,
	    [2] = 0.8,
	    [3] = 0.9,
	    [4] = 1,
	    [5] = 1.1,
	    [6] = 1.2,
	    [7] = 1.3,
	    [8] = 1.4,
	    [9] = 1.5,
	    [10] = 1.6
	},
	attackName = "worm-physical",
	tint = {r=0.85, g=0.85, b=0.83, a=0.65},
	pTint = {r=0, g=0, b=0, a=1},
	sTint = {r=0, g=0, b=0, a=1}
    },

    {
	
	{
	    type = "attribute",
	    name = "health",
	    [1] = 400,
	    [2] = 700,
	    [3] = 1000,
	    [4] = 1500,
	    [5] = 1800,
	    [6] = 2000,
	    [7] = 3000,
	    [8] = 4000,
	    [9] = 5000,
	    [10] = 9000
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
	    name = "physical",
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
	    
	    type = "resistance",
	    name = "fire",
	    decrease = {
		[1] = 3,
		[2] = 3,
		[3] = 4,
		[4] = 4,
		[5] = 6,
		[6] = 6,
		[7] = 6,
		[8] = 6,
		[9] = 7,
		[10] = 7
	    },
	    percent = {
		[1] = 70,
		[2] = 70,
		[3] = 72,
		[4] = 72,
		[5] = 73,
		[6] = 73,
		[7] = 74,
		[8] = 74,
		[9] = 75,
		[10] = 75
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
	createAcidBall(attributes)
	return createFireAttack(attributes, attributes.name .. "-stream-rampant")
    end,

    PHYSICAL_WORM_VARIATIONS,
    PHYSICAL_WORM_TIERS
)
