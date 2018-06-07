-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local neutral = {}

local NEUTRAL_UNIT_TIERS = constants.NEUTRAL_UNIT_TIERS
local NEUTRAL_UNIT_VARIATIONS = constants.NEUTRAL_UNIT_VARIATIONS

local NEUTRAL_NEST_TIERS = constants.NEUTRAL_NEST_TIERS
local NEUTRAL_NEST_VARIATIONS = constants.NEUTRAL_NEST_VARIATIONS

local NEUTRAL_WORM_TIERS = constants.NEUTRAL_WORM_TIERS
local NEUTRAL_WORM_VARIATIONS = constants.NEUTRAL_WORM_VARIATIONS

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

function neutral.addFaction()

    local biterLoot = makeUnitAlienLootTable(nil)
    local spawnerLoot = makeSpawnerAlienLootTable(nil)
    local wormLoot = makeWormAlienLootTable(nil)

    -- neutral biters
    buildUnitSpawner(
	{
	    unit = {
		name = "neutral-biter",

		loot = biterLoot,
		attributes = {
		    explosion = "blood-explosion-small"
		},
		attack = {},
		resistances = {},

		type = "biter",
		tint1 = {r=0.56, g=0.46, b=0.42, a=0.65},
		tint2 = {r=1, g=0.63, b=0, a=0.4}
	    },

	    unitSpawner = {
		name = "neutral-biter-nest",

		loot = spawnerLoot,
		attributes = {},	    
		resistances = {},
		tint = {r=0.56, g=0.46, b=0.42, a=0.65}
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

	    }
	},

	createMeleeAttack,

	{
	    unit = NEUTRAL_UNIT_VARIATIONS,
	    unitSpawner = NEUTRAL_NEST_VARIATIONS
	},

	{
	    unit = NEUTRAL_UNIT_TIERS,
	    unitSpawner = NEUTRAL_NEST_TIERS
	}
    )

    -- neutral spitters
    buildUnitSpawner(
	{
	    unit = {
		name = "neutral-spitter",

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
		attackName = "neutral-spitter",
		tint = {r=0.56, g=0.46, b=0.42, a=1},
		pTint = {r=0, g=1, b=1, a=0.5},
		sTint = {r=0, g=1, b=1, a=0.5}
	    },

	    unitSpawner = {
		name = "neutral-spitter-nest",

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
		}
		
	    },
	    
	    unitSpawner = {

	    }
	},

	function (attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes),
				      spitterattackanimation(attributes.scale,
							     attributes.tint))
	end,
	
	{
	    unit = NEUTRAL_UNIT_VARIATIONS,
	    unitSpawner = NEUTRAL_NEST_VARIATIONS
	},

	{
	    unit = NEUTRAL_UNIT_TIERS,
	    unitSpawner = NEUTRAL_NEST_TIERS
	}
    )

    -- neutral worms
    buildWorm(
	{
	    name = "neutral-worm",

	    loot = wormLoot,	    
	    attributes = {},
	    attack = {
		type = "projectile",
		softSmokeName = softSmoke
	    },
	    resistances = {},

	    attackName = "neutral-worm",
	    tint = {r=0.56, g=0.46, b=0.42, a=0.65},
	    pTint = {r=0, g=1, b=1, a=0.5},
	    sTint = {r=0, g=1, b=1, a=0.5}
	},

	{	    	    
	    
	},

	function (attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	NEUTRAL_WORM_VARIATIONS,
	NEUTRAL_WORM_TIERS
    )
end


return neutral
