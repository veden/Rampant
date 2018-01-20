-- imports

local acidBall = require("prototypes/enemies/AttackAcidBall")
local biterUtils = require("prototypes/enemies/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("libs/Constants")
local colorUtils = require("ColorUtils")

-- constants

local SUICIDE_BITER_NEST_TIERS = constants.SUICIDE_BITER_NEST_TIERS
local SUICIDE_BITER_NEST_VARIATIONS = constants.SUICIDE_BITER_NEST_VARIATIONS

local NEUTRAL_NEST_TIERS = constants.NEUTRAL_NEST_TIERS
local NEUTRAL_NEST_VARIATIONS = constants.NEUTRAL_NEST_VARIATIONS


-- imported functions

local makeColor = colorUtils.makeColor

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAcidBall = acidBall.createAcidBall
local createFireAttack = biterUtils.createFireAttack
local createSuicideAttack = biterUtils.createSuicideAttack
local createMeleeAttack = biterUtils.createMeleeAttack


-- module code

-- suicide
-- buildUnitSpawner(
--     {
-- 	unit = 10,
-- 	unitSpawner = 5,
-- 	probabilityTable = 5
--     },
--     {
-- 	unit = {
-- 	    name = "suicide-biter",

-- 	    attributes = {
-- 		health = 30,
-- 		movement = 0.21,
-- 		distancePerFrame = 0.1,
-- 		healing = 0.01,
-- 		explosion = "blood-explosion-small",
-- 	    },

-- 	    attack = {
-- 		area = 3.5,
-- 		damage = 20,
-- 		explosion = "explosion",
-- 		scorchmark = "small-scorchmark",
-- 		explosionCount = 2,
-- 		explosionDistance = 2,
-- 	    },

-- 	    resistances = {
-- 		explosion = {
-- 		    decrease = 0,
-- 		    percent = -50
-- 		},
-- 		laser = {
-- 		    decrease = 1,
-- 		    percent = 0
-- 		},
-- 		fire = {
-- 		    decrease = 0,
-- 		    percent = -60
-- 		}
-- 	    },

-- 	    type = "biter",
-- 	    scale = 0.55,
-- 	    tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
-- 	    tint2 = {r=0.7, g=0.0, b=0.72, a=0.4}
-- 	},

-- 	unitSpawner = {
-- 	    name = "suicide-biter-nest",
-- 	    attributes = {
-- 		health = 30,
-- 		healing = 0.01,
-- 		unitsOwned = 7,
-- 		unitsToSpawn = 5,
-- 		spawingCooldownStart = 360,
-- 		spawingCooldownStop = 150,

-- 	    },

-- 	    resistances = {
-- 		explosion = {
-- 		    decrease = 0,
-- 		    percent = -50
-- 		},
-- 		laser = {
-- 		    decrease = 1,
-- 		    percent = 0
-- 		},
-- 		fire = {
-- 		    decrease = 0,
-- 		    percent = -60
-- 		}
-- 	    },
-- 	    scale = 0.95,
-- 	    tint = {r=0.7, g=0.0, b=0.72, a=0.4}
-- 	}
--     },

--     {
-- 	unit = {
-- 	    {
-- 		cost = 1,
-- 		bonus = {
-- 		    {
-- 			type = "attribute",
-- 			name = "health",
-- 			adjustment = 50
-- 		    }
-- 		}
-- 	    }
-- 	},

-- 	unitSpawner = {
-- 	    {
-- 		cost = 1,
-- 		bonus = {
-- 		    {
-- 			type = "attribute",
-- 			name = "health",
-- 			adjustment = 50
-- 		    }
-- 		}
-- 	    }
-- 	},

-- 	probabilityTable = {
-- 	    {
-- 		cost = 1,
-- 		index = 1,
-- 		adjustment = 1
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 2,
-- 		adjustment = 1
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 3,
-- 		adjustment = 1
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 4,
-- 		adjustment = 1
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 5,
-- 		adjustment = 1
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 6,
-- 		adjustment = 1.5
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 7,
-- 		adjustment = 2
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 8,
-- 		adjustment = 2
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 9,
-- 		adjustment = 1.5
-- 	    },
-- 	    {
-- 		cost = 1,
-- 		index = 10,
-- 		adjustment = 1
-- 	    }
-- 	}
--     },

--     createSuicideAttack,

--     {
-- 	unit = 5,
-- 	unitSpawner = SUICIDE_BITER_NEST_VARIATIONS
--     },

--     {
-- 	unit = 10,
-- 	unitSpawner = SUICIDE_BITER_NEST_TIERS
--     }
-- )

-- neutral biters
buildUnitSpawner(
    {
	unit = {
	    name = "neutral-biter",

	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {},
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
	    tint1 = {r=0.56, g=0.46, b=0.42, a=0.65},
	    tint2 = {r=1, g=0.63, b=0, a=0.4}
	},

	unitSpawner = {
	    name = "neutral-biter-nest",

	    attributes = {},	    
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
	    tint = {r=1.0, g=1.0, b=1.0, a=1.0}
	}
    },

    {
	unit = {
	    {
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
		}
	    },

	    {
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
		}
	    },
	    
	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },
	    
	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
		
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
	    }
	},
	
	unitSpawner = {

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}		
	    },

	    {		
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
		}
	    },
	    {
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
	    },

	    {
		
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
	unitSpawner = NEUTRAL_NEST_VARIATIONS
    },

    {
	unit = 10,
	unitSpawner = NEUTRAL_NEST_TIERS
    }
)

-- neutral spitters
buildUnitSpawner(
    {
	unit = {
	    name = "neutral-spitter",

	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {},
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
	    attackName = "acid-ball",
	    tint = {r=0.56, g=0.46, b=0.42, a=0.65},
	    pTint = {r=0, g=1, b=1, a=0.5},
	    sTint = {r=0, g=1, b=1, a=0.5},
	    smTint = makeColor(0.3, 0.75, 0.3, 0.1)
	},

	unitSpawner = {
	    name = "neutral-spitter-nest",

	    attributes = {},
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
	    tint = {r=0.99, g=0.09, b=0.09, a=1}
	}
    },

    {
	unit = {
	    {
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
		}
	    },

	    {
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
		}
	    },
	    
	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },
	    
	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
		
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
		}
	    },

	    {
		
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
		}
	    }
	},
	
	unitSpawner = {

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}
	    },

	    {
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
		}		
	    },

	    {		
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
		}
	    },
	    {
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
	    },

	    {
		
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
	return createFireAttack(attributes, attributes.name .. "-stream-rampant")
    end,
    
    {
	unit = 10,
	unitSpawner = NEUTRAL_NEST_VARIATIONS
    },

    {
	unit = 10,
	unitSpawner = NEUTRAL_NEST_TIERS
    }
)

-- neutral worms
buildWorm(
    {
	name = "neutral-worm",

	attributes = {},
	attack = {},
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
	attackName = "acid-ball",
	tint = {r=0.56, g=0.46, b=0.42, a=0.65},
	pTint = {r=0, g=1, b=1, a=0.5},
	sTint = {r=0, g=1, b=1, a=0.5},
	smTint = makeColor(0.3, 0.75, 0.3, 0.1)
    },

    {
	{
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
	    }
	},

	{
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
	    }
	},
	
	{
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
	    }
	},

	{
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
	    }
	},

	{
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
	    }
	},
	
	{
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
	    }
	},

	{
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
	    }
	},

	{
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
	    }
	},

	{
	    
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
	    }
	},

	{
	    
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
	    }
	}
    },

    function (attributes)
	createAcidBall(attributes)
	return createFireAttack(attributes, attributes.name .. "-stream-rampant")
    end,
    
    10,
    10
)

function generateLocal() 
    local names = {"Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"}
    local sizes = {"Larva", "Pupae", "Worker", "Grunt", "Soldier", "General", "Overlord", "Titan", "Leviathan", "Juggernaut"}

    print("[entity-name]")


    local name = names[1]

    for t = 1, 10 do
	local size = sizes[t]

	for v = 1, 20 do
	    
	    print("neutral-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("neutral-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")

	    if (v <= 10) then
		print("neutral-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
		print("neutral-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    end
	end
    end
end
