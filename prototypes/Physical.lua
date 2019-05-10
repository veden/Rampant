-- imports

local physicalBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

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
		    [6] = 1.3,
		    [7] = 1.5,
		    [8] = 1.7,
		    [9] = 1.9,
		    [10] = 2.1
		},
		tint = {r=0.1, g=0.1, b=0.1, a=1}
	    },

	    unitSpawner = {
		name = "physical-biter-spawner",

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
		    [5] = 1500,
		    [6] = 3000,
		    [7] = 5000,
		    [8] = 12000,
		    [9] = 20000,
		    [10] = 40000
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
                    type = "resistance",
                    name = "laser",
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
                    name = "electric",
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
		    name = "explosion",
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
		    [5] = 7000,
		    [6] = 15000,
		    [7] = 22000,
		    [8] = 40000,
		    [9] = 60000,
		    [10] = 70000
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
                    name = "laser",
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
                    name = "electric",
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
		    name = "explosion",
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
			    entity_name = attributes.explosion
			}
		    }
		end
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
	    tint = {r=0.85, g=0.85, b=0.83, a=0.65}
	},

	{

            {
                type = "attack",
                mapping = "explosion",
                [1] = "explosion",
                [2] = "explosion",
                [3] = "big-explosion",
                [4] = "big-explosion",
                [5] = "big-explosion",
                [6] = "big-explosion",
                [7] = "massive-explosion",
                [8] = "massive-explosion",
                [9] = "massive-explosion",
                [10] = "massive-explosion"
            },
            
	    {
                type = "attribute",
                name = "health",
                [1] = 400,
                [2] = 700,
                [3] = 1000,
                [4] = 1500,
                [5] = 4000,
                [6] = 7000,
                [7] = 15000,
                [8] = 24000,
                [9] = 40000,
                [10] = 50000
            },

            {
                type = "resistance",
                name = "explosion",
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
                type = "resistance",
                name = "electric",
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
                name = "laser",
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
