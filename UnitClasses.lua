-- imports

local biterUtils = require("prototypes/enemies/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("libs/Constants")

-- constants

local SUICIDE_BITER_NEST_TIERS = constants.SUICIDE_BITER_NEST_TIERS
local SUICIDE_BITER_NEST_VARIATIONS = constants.SUICIDE_BITER_NEST_VARIATIONS

local NEUTRAL_NEST_TIERS = constants.NEUTRAL_NEST_TIERS
local NEUTRAL_NEST_VARIATIONS = constants.NEUTRAL_NEST_VARIATIONS


-- imported functions



local buildUnitSpawner = swarmUtils.buildUnitSpawner

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

-- neutral
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
	    scale = 0.5,
	    tint1 = {r=0.56, g=0.46, b=0.42, a=0.65},
	    tint2 = {r=1, g=0.63, b=0, a=0.4}
	},

	unitSpawner = {
	    name = "neutral-biter-nest",
	    attributes = {
		health = 300,
		healing = 0.01,
		unitsOwned = 7,
		unitsToSpawn = 5,
		spawingCooldownStart = 360,
		spawingCooldownStop = 150,
	    },
	    
	    resistances = {
		explosion = {
		    decrease = 3,
		    percent = 10
		},
		physical = {
		    decrease = 1,
		    percent = 10
		},
		fire = {
		    decrease = 1.5,
		    percent = 40
		}
	    },
	    scale = 1,
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
		    [3] = 0.012,
		    [4] = 0.013,
		    [5] = 0.014,
		    [6] = 0.016,
		    [7] = 0.017,
		    [8] = 0.02,
		    [9] = 0.022,
		    [10] = 0.028
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
			[5] = 10,
			[6] = 10,
			[7] = 10,
			[8] = 10,
			[9] = 10,
			[10] = 10
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
		    adjustment = 50
		}
	    },

	    {
		{
		    type = "attribute",
		    name = "healing",
		    adjustment = 0.02
		}
	    },

	    {
		
		{
		    type = "attribute",
		    name = "spawingCooldownStart",
		    adjustment = -10
		},
		{
		    type = "attribute",
		    name = "spawingCooldownEnd",
		    adjustment = -10
		},
		{
		    type = "attribute",
		    name = "evolutionRequirement",
		    adjustment = 0.01
		}
		
	    },

	    {
		
		{
		    type = "attribute",
		    name = "unitsOwned",
		    adjustment = 2
		},
		{
		    type = "attribute",
		    name = "unitsToSpawn",
		    adjustment = 1
		},
		{
		    type = "attribute",
		    name = "evolutionRequirement",
		    adjustment = 0.01
		}
		
	    },

	    {
		
		{
		    type = "resistance",
		    name = "physical",
		    decrease = 0.5,
		    percent = 2
		},
		{
		    type = "resistance",
		    name = "explosion",
		    decrease = 1,
		    percent = 2
		},
		{
		    type = "resistance",
		    name = "fire",
		    decrease = 0.7,
		    percent = 5
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

for k,v in pairs(data.raw.unit) do
    print(k)
end
print(serpent.dump(data.raw.unit['neutral-biter-v1-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v2-t10-rampant']))

print(serpent.dump(data.raw.unit['neutral-biter-v3-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v4-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v5-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v6-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v7-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v8-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v9-t1-rampant']))
print(serpent.dump(data.raw.unit['neutral-biter-v10-t1-rampant']))

--print(serpent.dump(data.raw.unit))
constants.et()
