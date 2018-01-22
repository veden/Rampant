-- imports

local acidBall = require("utils/AttackAcidBall")
local biterUtils = require("utils/BiterUtils")
local smokeUtils = require("utils/SmokeUtils")
local swarmUtils = require("SwarmUtils")
local colorUtils = require("utils/ColorUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local INFERNAL_NEST_TIERS = constants.INFERNAL_NEST_TIERS
local INFERNAL_NEST_VARIATIONS = constants.INFERNAL_NEST_VARIATIONS

local INFERNAL_WORM_TIERS = constants.INFERNAL_WORM_TIERS
local INFERNAL_WORM_VARIATIONS = constants.INFERNAL_WORM_VARIATIONS

-- imported functions

local makeColor = colorUtils.makeColor

local makeSmokeSoft = smokeUtils.makeSmokeSoft

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAcidBall = acidBall.createAcidBall
local createFlameAttack = biterUtils.createFlameAttack
local createMeleeAttack = biterUtils.createMeleeAttack

makeSmokeSoft({name="inferno", softSmokeTint=makeColor(0.3, 0.75, 0.3, 0.1)})

-- inferno biters
buildUnitSpawner(
    {
	unit = {
	    name = "inferno-biter",

	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {
		damageType = "inferno"
	    },
	    resistances = {},

	    type = "biter",
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
	    tint1 = {r=1, g=0, b=0, a=0.65},
	    tint2 = {r=1, g=0, b=0, a=0.4}
	},

	unitSpawner = {
	    name = "inferno-biter-nest",

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
	    tint = {r=1.0, g=0, b=0, a=1.0}
	}
    },

    {
	unit = {
	    {

		type = "attribute",
		name = "health",
		[1] = 15,
		[2] = 75,
		[3] = 150,
		[4] = 250,
		[5] = 400,
		[6] = 750,
		[7] = 1500,
		[8] = 3000,
		[9] = 5000,
		[10] = 10000

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
		[1] = 0.2,
		[2] = 0.19,
		[3] = 0.185,
		[4] = 0.18,
		[5] = 0.175,
		[6] = 0.17,
		[7] = 0.17,
		[8] = 0.17,
		[9] = 0.17,
		[10] = 0.17
	    },
	    {
		type = "attribute",
		name = "distancePerFrame",
		[1] = 0.1,
		[2] = 0.125,
		[3] = 0.15,
		[4] = 0.19,
		[5] = 0.195,
		[6] = 0.2,
		[7] = 0.2,
		[8] = 0.2,
		[9] = 0.2,
		[10] = 0.2
	    },

	    {
		type = "resistance",
		name = "physical",
		decrease = {
		    [1] = 0,
		    [2] = 0,
		    [3] = 4,
		    [4] = 5,
		    [5] = 6,
		    [6] = 8,
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
		[1] = 0,
		[2] = 0.12,
		[3] = 0.22,
		[4] = 0.32,
		[5] = 0.42,
		[6] = 0.52,
		[7] = 0.62,
		[8] = 0.72,
		[9] = 0.82,
		[10] = 0.92
		
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

    createMeleeAttack,

    {
	unit = 10,
	unitSpawner = INFERNAL_NEST_VARIATIONS
    },

    {
	unit = 10,
	unitSpawner = INFERNAL_NEST_TIERS
    }
)

-- inferno spitters
buildUnitSpawner(
    {
	unit = {
	    name = "inferno-spitter",

	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {
		damageType = "inferno",
		softSmokeName = "inferno-soft-smoke-rampant"
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
	createAcidBall(attributes)
	return createFlameAttack(attributes, attributes.name .. "-stream-rampant")
    end,
    
    {
	unit = 10,
	unitSpawner = INFERNAL_NEST_VARIATIONS
    },

    {
	unit = 10,
	unitSpawner = INFERNAL_NEST_TIERS
    }
)

-- inferno worms
buildWorm(
    {
	name = "inferno-worm",

	attributes = {},
	attack = {
	    damageType = "inferno",
	    softSmokeName = "inferno-soft-smoke-rampant"
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
	    name = "inferno",
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
	createAcidBall(attributes)
	return createFlameAttack(attributes, attributes.name .. "-stream-rampant")
    end,

    INFERNAL_WORM_VARIATIONS,
    INFERNAL_WORM_TIERS
)
