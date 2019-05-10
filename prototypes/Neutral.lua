-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

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
		tint = {r=0.56, g=0.46, b=0.42, a=0.65}
	    },

	    unitSpawner = {
		name = "neutral-biter-spawner",

		loot = spawnerLoot,
		attributes = {},	    
		resistances = {},
		tint = {r=0.56, g=0.46, b=0.42, a=0.65}
	    }
	},

	{
	    unit = {

	    },
	    
	    unitSpawner = {		

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
                    directionOnly = true
		},
		resistances = {},

		type = "spitter",
		attackName = "neutral-spitter",
		tint = {r=0.56, g=0.46, b=0.42, a=1}
	    },

	    unitSpawner = {
		name = "neutral-spitter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},
		
		tint = {r=0.99, g=0.09, b=0.09, a=1}
	    }
	},

	{
	    unit = {
		
	    },
	    
	    unitSpawner = {

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
		type = "projectile"
	    },
	    resistances = {},

	    attackName = "neutral-worm",
	    tint = {r=0.56, g=0.46, b=0.42, a=0.65}
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
