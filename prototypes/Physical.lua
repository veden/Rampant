-- imports

local physicalBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local physical = {}

local PHYSICAL_UNIT_TIERS = constants.PHYSICAL_UNIT_TIERS
local PHYSICAL_UNIT_VARIATIONS = constants.PHYSICAL_UNIT_VARIATIONS

local PHYSICAL_NEST_TIERS = constants.PHYSICAL_NEST_TIERS
local PHYSICAL_NEST_VARIATIONS = constants.PHYSICAL_NEST_VARIATIONS

local PHYSICAL_WORM_TIERS = constants.PHYSICAL_WORM_TIERS
local PHYSICAL_WORM_VARIATIONS = constants.PHYSICAL_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = physicalBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack

local softSmoke = "the-soft-smoke-rampant"

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

function physical.addFaction()

    local biterLoot = makeUnitAlienLootTable("red")
    local spawnerLoot = makeSpawnerAlienLootTable("red")
    local wormLoot = makeWormAlienLootTable("red")



    -- physical biters
    buildUnitSpawner(
	{
	    unit = {
		name = "physical-biter",

		loot = biterLoot,
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
		tint1 = {r=0.1, g=0.1, b=0.1, a=1},
		tint2 = {r=0.1, g=0.1, b=0.1, a=1}
	    },

	    unitSpawner = {
		name = "physical-biter-nest",

		loot = spawnerLoot,
		attributes = {},	    
		resistances = {},
		scales = {
		    [1] = 0.7,
		    [2] = 0.8,
		    [3] = 0.9,
		    [4] = 1.0,
		    [5] = 1.1,
		    [6] = 1.2,
		    [7] = 1.3,
		    [8] = 1.4,
		    [9] = 1.5,
		    [10] = 1.6
		},
		tint = {r=0.1, g=0.1, b=0.1, a=1}
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
		
	    }
	},

	createMeleeAttack,

	{
	    unit = PHYSICAL_UNIT_VARIATIONS,
	    unitSpawner = PHYSICAL_NEST_VARIATIONS
	},

	{
	    unit = PHYSICAL_UNIT_TIERS,
	    unitSpawner = PHYSICAL_NEST_TIERS
	}
    )

    -- physical worms
    buildWorm(
	{
	    name = "physical-worm",

	    loot = wormLoot,
	    attributes = {
	    },
	    attack = {
		type = "projectile",
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
		softSmokeName =  softSmoke
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
	    attackName = "physical-worm",
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
		[3] = 1200,
		[4] = 1700,
		[5] = 2000,
		[6] = 3000,
		[7] = 4000,
		[8] = 5000,
		[9] = 7500,
		[10] = 12000
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
	    }
	},

	function (attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	PHYSICAL_WORM_VARIATIONS,
	PHYSICAL_WORM_TIERS
    )
end

return physical
