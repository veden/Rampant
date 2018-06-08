-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local acid = {}

local ACID_UNIT_TIERS = constants.ACID_UNIT_TIERS
local ACID_UNIT_VARIATIONS = constants.ACID_UNIT_VARIATIONS

local ACID_NEST_TIERS = constants.ACID_NEST_TIERS
local ACID_NEST_VARIATIONS = constants.ACID_NEST_VARIATIONS

local ACID_WORM_TIERS = constants.ACID_WORM_TIERS
local ACID_WORM_VARIATIONS = constants.ACID_WORM_VARIATIONS

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

local biterLoot = makeUnitAlienLootTable("green")
local spawnerLoot = makeSpawnerAlienLootTable("green")
local wormLoot = makeWormAlienLootTable("green")

function acid.addFaction()

    -- acid biters
    buildUnitSpawner(
	{
	    unit = {
		name = "acid-biter",

		attributes = {
		    explosion = "blood-explosion-small"
		},
		attack = {
		    damageType = "acid"
		},
		resistances = {},

		type = "biter",
		loot = biterLoot,
		tint1 = {r=0, g=0.85, b=0.13, a=0.65},
		tint2 = {r=0, g=0.85, b=0.13, a=0.65}
	    },

	    unitSpawner = {
		name = "acid-biter-nest",

		loot = spawnerLoot,
		attributes = {},	    
		resistances = {},
		tint = {r=0, g=0.85, b=0.13, a=0.65}
	    }
	},

	{
	    unit = {
				
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
	    unit = ACID_UNIT_VARIATIONS,
	    unitSpawner = ACID_NEST_VARIATIONS
	},

	{
	    unit = ACID_UNIT_TIERS,
	    unitSpawner = ACID_NEST_TIERS
	}
    )

    -- acid spitters
    buildUnitSpawner(
	{
	    unit = {
		name = "acid-spitter",

		loot = biterLoot,
		attributes = {
		    explosion = "blood-explosion-small"
		},
		attack = {
		    type = "projectile",
		    directionOnly = true,
		    softSmokeName = softSmoke
		},
		resistances = {},

		type = "spitter",
		attackName = "acid-spitter",
		tint = {r=0, g=0.85, b=0.1, a=0.65},
		pTint = {r=0, g=1, b=0.1, a=0.5},
		sTint = {r=0, g=1, b=0.1, a=0.5}
	    },

	    unitSpawner = {
		name = "acid-spitter-nest",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},

		tint = {r=0, g=0.85, b=0.13, a=1}
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
	    unit = ACID_UNIT_VARIATIONS,
	    unitSpawner = ACID_NEST_VARIATIONS
	},

	{
	    unit = ACID_UNIT_TIERS,
	    unitSpawner = ACID_NEST_TIERS
	}
    )

    -- acid worms
    buildWorm(
	{
	    name = "acid-worm",

	    loot = wormLoot,
	    attributes = {},
	    attack = {
		type = "projectile",
		softSmokeName = softSmoke
	    },
	    resistances = {},

	    attackName = "acid-worm",
	    tint = {r=0, g=0.85, b=0.1, a=0.65},
	    pTint = {r=0, g=1, b=0.1, a=0.5},
	    sTint = {r=0, g=1, b=0.1, a=0.5}
	},

	{
	    
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

	ACID_WORM_VARIATIONS,
	ACID_WORM_TIERS
    )
end

return acid
