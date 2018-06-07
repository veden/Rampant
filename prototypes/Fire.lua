-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local fire = {}

local FIRE_UNIT_TIERS = constants.FIRE_UNIT_TIERS
local FIRE_UNIT_VARIATIONS = constants.FIRE_UNIT_VARIATIONS

local FIRE_NEST_TIERS = constants.FIRE_NEST_TIERS
local FIRE_NEST_VARIATIONS = constants.FIRE_NEST_VARIATIONS

local FIRE_WORM_TIERS = constants.FIRE_WORM_TIERS
local FIRE_WORM_VARIATIONS = constants.FIRE_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = acidBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack

local softSmoke = "the-soft-smoke-rampant"

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local biterLoot = makeUnitAlienLootTable("red")
local spawnerLoot = makeSpawnerAlienLootTable("red")
local wormLoot = makeWormAlienLootTable("red")

function fire.addFaction()

    -- fire biters
    buildUnitSpawner(
	{
	    unit = {
		name = "fire-biter",

		loot = biterLoot,
		attributes = {
		    explosion = "blood-explosion-small"
		},
		attack = {
		    damageType = "acid"
		},
		resistances = {},

		type = "biter",
		tint1 = {r=0.65, g=0, b=0, a=0.65},
		tint2 = {r=1, g=1, b=1, a=0.4}
	    },

	    unitSpawner = {
		name = "fire-biter-nest",

		loot = spawnerLoot,
		attributes = {},	    
		resistances = {},
		tint = {r=1.0, g=0, b=0, a=1.0}
	    }
	},

	{
	    unit = {
			
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
	    
	    unitSpawner = {

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
		    name = "acid",
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
	    }
	},

	createMeleeAttack,

	{
	    unit = FIRE_UNIT_VARIATIONS,
	    unitSpawner = FIRE_NEST_VARIATIONS
	},

	{
	    unit = FIRE_UNIT_TIERS,
	    unitSpawner = FIRE_NEST_TIERS
	}
    )

    -- fire spitters
    buildUnitSpawner(
	{
	    unit = {
		name = "fire-spitter",

		attributes = {
		    explosion = "blood-explosion-small"
		},
		loot = biterLoot,
		attack = {
		    type = "projectile",
		    damageType = "acid",
		    directionOnly = true,
		    softSmokeName = softSmoke
		},
		resistances = {},

		type = "spitter",
		attackName = "fire-spitter",
		tint = {r=0.65, g=0, b=0, a=0.65},
		pTint = {r=1, g=1, b=1, a=0.5},
		sTint = {r=1, g=1, b=1, a=0.5}
	    },

	    unitSpawner = {
		name = "fire-spitter-nest",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},
		
		tint = {r=0.99, g=0.09, b=0.09, a=1}
	    }
	},

	{
	    unit = {
				
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
		    name = "acid",
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
	    
	    unitSpawner = {

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
		    name = "acid",
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
	    }
	    
	},

	function (attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes),
				      spitterattackanimation(attributes.scale,
							     attributes.tint))
	end,
	
	{
	    unit = FIRE_UNIT_VARIATIONS,
	    unitSpawner = FIRE_NEST_VARIATIONS
	},

	{
	    unit = FIRE_UNIT_TIERS,
	    unitSpawner = FIRE_NEST_TIERS
	}
    )

    -- fire worms
    buildWorm(
	{
	    name = "fire-worm",

	    attributes = {},
	    loot = wormLoot,
	    attack = {
		type = "projectile",
		damageType = "acid",
		softSmokeName = softSmoke
	    },
	    resistances = {},

	    attackName = "fire-worm",
	    tint = {r=0.65, g=0, b=0, a=0.65},
	    pTint = {r=1, g=1, b=1, a=0.5},
	    sTint = {r=1, g=1, b=1, a=0.5}
	},

	{
	    
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
		type = "resistance",
		name = "acid",
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

	function (attributes)	
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	FIRE_WORM_VARIATIONS,
	FIRE_WORM_TIERS
    )
end


return fire
