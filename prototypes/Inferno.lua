-- imports

local attackFlame = require("utils/AttackFlame")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")

local constants = require("__Rampant__/libs/Constants")
local math3d = require("math3d")

-- constants

local inferno = {}

local INFERNO_UNIT_TIERS = constants.INFERNO_UNIT_TIERS
local INFERNO_UNIT_VARIATIONS = constants.INFERNO_UNIT_VARIATIONS

local INFERNO_NEST_TIERS = constants.INFERNO_NEST_TIERS
local INFERNO_NEST_VARIATIONS = constants.INFERNO_NEST_VARIATIONS

local INFERNO_WORM_TIERS = constants.INFERNO_WORM_TIERS
local INFERNO_WORM_VARIATIONS = constants.INFERNO_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackFlame = attackFlame.createAttackFlame
local createStreamAttack = biterUtils.createStreamAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local biterLoot = makeUnitAlienLootTable("orange")
local spawnerLoot = makeSpawnerAlienLootTable("orange")
local wormLoot = makeWormAlienLootTable("orange")

function inferno.addFaction()


    -- inferno spitters
    buildUnitSpawner(
	{
	    unit = {
		name = "inferno-spitter",

		loot = biterLoot,
		attributes = {
		    explosion = "blood-explosion-small"
		},
		attack = {
		    damageType = "acid",
		    fireDamagePerTickType = "acid",
		    stickerDamagePerTickType = "acid"
		},
		resistances = {},

		type = "spitter",
		attackName = "spitter-inferno",
		tint = {r=0.65, g=0, b=0, a=1}
	    },

	    unitSpawner = {
		name = "inferno-spitter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},

		tint = {r=0.99, g=0.09, b=0.09, a=1}
	    }
	},

	{
	    unit = {

		{
		    type = "attack",
		    name = "stickerDamagePerTick",
		    [1] = 1.6,
		    [2] = 1.6,
		    [3] = 1.8,
		    [4] = 1.8,
		    [5] = 1.8,
		    [6] = 1.9,
		    [7] = 2,
		    [8] = 2,
		    [9] = 2.3,
		    [10] = 2.5
		},

		{
		    type = "attack",
		    name = "particleTimeout",
		    [1] = 3,
		    [2] = 3,
		    [3] = 4,
		    [4] = 4,
		    [5] = 5,
		    [6] = 5,
		    [7] = 6,
		    [8] = 6,
		    [9] = 7,
		    [10] = 7
		},

		{
		    type = "attack",
		    name = "fireSpreadRadius",
		    [1] = 0.75,
		    [2] = 0.75,
		    [3] = 0.77,
		    [4] = 0.77,
		    [5] = 0.79,
		    [6] = 0.79,
		    [7] = 0.83,
		    [8] = 0.83,
		    [9] = 0.85,
		    [10] = 0.85
		},

		{
		    type = "attack",
		    name = "damageMaxMultipler",
		    [1] = 6,
		    [2] = 6,
		    [3] = 7,
		    [4] = 7,
		    [5] = 7,
		    [6] = 7,
		    [7] = 8,
		    [8] = 8,
		    [9] = 8,
		    [10] = 9
		},

		{
		    type = "attack",
		    name = "stickerMovementModifier",
		    [1] = 1.1,
		    [2] = 1.1,
		    [3] = 1.1,
		    [4] = 1.1,
		    [5] = 1.1,
		    [6] = 1.1,
		    [7] = 1.1,
		    [8] = 1.1,
		    [9] = 1.1,
		    [10] = 1.1
		},

		{
		    type = "attack",
		    name = "fireSpreadCooldown",
		    [1] = 30,
		    [2] = 30,
		    [3] = 29,
		    [4] = 29,
		    [5] = 28,
		    [6] = 28,
		    [7] = 27,
		    [8] = 27,
		    [9] = 25,
		    [10] = 25
		},

		{
		    type = "attack",
		    name = "stickerDuration",
		    [1] = 1800,
		    [2] = 1800,
		    [3] = 1900,
		    [4] = 1900,
		    [5] = 2000,
		    [6] = 2000,
		    [7] = 2100,
		    [8] = 2100,
		    [9] = 2200,
		    [10] = 2200
		},

		{
		    type = "attack",
		    name = "damage",
		    [1] = 4,
		    [2] = 4,
		    [3] = 5,
		    [4] = 5,
		    [5] = 6,
		    [6] = 6,
		    [7] = 6,
		    [8] = 6,
		    [9] = 6,
		    [10] = 7
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
                },


                {
                    type = "resistance",
                    name = "poison",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
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

	    unitSpawner = {
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
                },

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    [1] = 0,
                    [2] = 0.15,
                    [3] = 0.25,
                    [4] = 0.35,
                    [5] = 0.45,
                    [6] = 0.55,
                    [7] = 0.65,
                    [8] = 0.70,
                    [9] = 0.75,
                    [10] = 0.95
                },
                
                {
                    type = "resistance",
                    name = "poison",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
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
	    }
	},

	function (attributes)
	    return createStreamAttack(attributes,
				      createAttackFlame(attributes),
				      spitterattackanimation(attributes.scale,
							     attributes.tint,
                                                             attributes.tint))
	end,

	{
	    unit = INFERNO_UNIT_VARIATIONS,
	    unitSpawner = INFERNO_NEST_VARIATIONS
	},

	{
	    unit = INFERNO_UNIT_TIERS,
	    unitSpawner = INFERNO_NEST_TIERS
	}
    )

    -- inferno worms
    buildWorm(
	{
	    name = "inferno-worm",

	    loot = wormLoot,
	    attributes = {},
	    attack = {
		damageType = "acid",
		fireDamagePerTickType = "acid",
		stickerDamagePerTickType = "acid"
	    },
	    resistances = {},

	    attackName = "worm-inferno",
	    tint = {r=0.65, g=0, b=0, a=0.65}
	},

	{

	    {
		type = "attack",
		name = "stickerDamagePerTick",
		[1] = 1.6,
		[2] = 1.6,
		[3] = 1.8,
		[4] = 1.8,
		[5] = 1.8,
		[6] = 1.9,
		[7] = 2,
		[8] = 2,
		[9] = 2.3,
		[10] = 2.5
	    },

            {
                type = "attribute",
                name = "evolutionRequirement",
                [1] = 0,
                [2] = 0.15,
                [3] = 0.25,
                [4] = 0.35,
                [5] = 0.45,
                [6] = 0.55,
                [7] = 0.65,
                [8] = 0.70,
                [9] = 0.75,
                [10] = 0.95
            },
            
	    {
		type = "attack",
		name = "particleTimeout",
		[1] = 3,
		[2] = 3,
		[3] = 4,
		[4] = 4,
		[5] = 5,
		[6] = 5,
		[7] = 6,
		[8] = 6,
		[9] = 7,
		[10] = 7
	    },

	    {
		type = "attack",
		name = "fireSpreadRadius",
		[1] = 0.75,
		[2] = 0.75,
		[3] = 0.77,
		[4] = 0.77,
		[5] = 0.79,
		[6] = 0.79,
		[7] = 0.83,
		[8] = 0.83,
		[9] = 0.85,
		[10] = 0.85
	    },

	    {
		type = "attack",
		name = "damageMaxMultipler",
		[1] = 6,
		[2] = 6,
		[3] = 7,
		[4] = 7,
		[5] = 7,
		[6] = 7,
		[7] = 8,
		[8] = 8,
		[9] = 8,
		[10] = 9
	    },

	    {
		type = "attack",
		name = "stickerMovementModifier",
		[1] = 1.1,
		[2] = 1.1,
		[3] = 1.1,
		[4] = 1.1,
		[5] = 1.1,
		[6] = 1.1,
		[7] = 1.1,
		[8] = 1.1,
		[9] = 1.1,
		[10] = 1.1
	    },

	    {
		type = "attack",
		name = "fireSpreadCooldown",
		[1] = 30,
		[2] = 30,
		[3] = 29,
		[4] = 29,
		[5] = 28,
		[6] = 28,
		[7] = 27,
		[8] = 27,
		[9] = 25,
		[10] = 25
	    },

	    {
		type = "attack",
		name = "stickerDuration",
		[1] = 1800,
		[2] = 1800,
		[3] = 1900,
		[4] = 1900,
		[5] = 2000,
		[6] = 2000,
		[7] = 2100,
		[8] = 2100,
		[9] = 2200,
		[10] = 2200
	    },

	    {
		type = "attack",
		name = "damage",
		[1] = 4,
		[2] = 4,
		[3] = 5,
		[4] = 5,
		[5] = 6,
		[6] = 6,
		[7] = 6,
		[8] = 6,
		[9] = 6,
		[10] = 7
	    },

            {
                type = "resistance",
                name = "poison",
                decrease = {
                    [1] = -3,
                    [2] = -3,
                    [3] = -7,
                    [4] = -7,
                    [5] = -10,
                    [6] = -10,
                    [7] = -13,
                    [8] = -13,
                    [9] = -16,
                    [10] = -18
                },
                percent = {
                    [1] = -35,
                    [2] = -35,
                    [3] = -40,
                    [4] = -40,
                    [5] = -45,
                    [6] = -45,
                    [7] = -50,
                    [8] = -55,
                    [9] = -55,
                    [10] = -60
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

	function (attributes)
	    return createStreamAttack(attributes,
				      createAttackFlame(attributes))
	end,

	INFERNO_WORM_VARIATIONS,
	INFERNO_WORM_TIERS
    )
end

return inferno
