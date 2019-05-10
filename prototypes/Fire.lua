-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

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
		tint = {r=0.65, g=0, b=0, a=0.65}
	    },

	    unitSpawner = {
		name = "fire-biter-spawner",

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
                    directionOnly = true
		},
		resistances = {},

		type = "spitter",
		attackName = "fire-spitter",
		tint = {r=0.65, g=0, b=0, a=0.65}
	    },

	    unitSpawner = {
		name = "fire-spitter-spawner",

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
							     attributes.tint,
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
		damageType = "acid"
	    },
	    resistances = {},

	    attackName = "fire-worm",
	    tint = {r=0.65, g=0, b=0, a=0.65}
	},

	{

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
