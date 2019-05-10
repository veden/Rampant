-- imports

local biterUtils = require("utils/BiterUtils")
local beamUtils = require("utils/BeamUtils")
local attackBall = require("utils/AttackBall")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

-- constants

local electric = {}

local ELECTRIC_UNIT_TIERS = constants.ELECTRIC_UNIT_TIERS
local ELECTRIC_UNIT_VARIATIONS = constants.ELECTRIC_UNIT_VARIATIONS

local ELECTRIC_NEST_TIERS = constants.ELECTRIC_NEST_TIERS
local ELECTRIC_NEST_VARIATIONS = constants.ELECTRIC_NEST_VARIATIONS

local ELECTRIC_WORM_TIERS = constants.ELECTRIC_WORM_TIERS
local ELECTRIC_WORM_VARIATIONS = constants.ELECTRIC_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createElectricAttack = biterUtils.createElectricAttack
local createAttackBall = attackBall.createAttackBall
local makeLaser = beamUtils.makeLaser
local createRangedAttack = biterUtils.createRangedAttack
local makeBeam = beamUtils.makeBeam
local makeBubble = beamUtils.makeBubble

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

function electric.addFaction()

    local biterLoot = makeUnitAlienLootTable("blue")
    local spawnerLoot = makeSpawnerAlienLootTable("blue")
    local wormLoot = makeWormAlienLootTable("blue")

    local electricBubble = makeBubble({
	    name = "electric-worm",
	    tint = {r=0, g=0.1, b=1, a=1}
    })

    -- electric biters
    buildUnitSpawner(
	{
	    unit = {
		name = "electric-biter",

		attributes = {
		    explosion = "blood-explosion-small"
		},
		loot = biterLoot,
		attack = {
		    damageType = "electric"
		},
		resistances = {},

		type = "biter",
		attackName = "biter-electric",
		tint = {r=0, g=0.25, b=0.83, a=0.65}
	    },

	    unitSpawner = {
		name = "electric-biter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},
		tint = {r=0, g=0.25, b=0.83, a=0.65}
	    }
	},

	{
	    unit = {
		{
                    type = "attribute",
                    name = "health",
                    [1] = 10,
                    [2] = 50,
                    [3] = 200,
                    [4] = 350,
                    [5] = 1250,
                    [6] = 2250,
                    [7] = 3250,
                    [8] = 6500,
                    [9] = 12500,
                    [10] = 25000
                },

		{
		    type = "attack",
		    name = "width",
		    [1] = 1.5,
		    [2] = 1.5,
		    [3] = 1.6,
		    [4] = 1.6,
		    [5] = 1.7,
		    [6] = 1.7,
		    [7] = 1.8,
		    [8] = 1.8,
		    [9] = 1.9,
		    [10] = 1.9
		},

		{
		    type = "attack",
		    name = "damageInterval",
		    [1] = 20,
		    [2] = 20,
		    [3] = 21,
		    [4] = 21,
		    [5] = 22,
		    [6] = 22,
		    [7] = 23,
		    [8] = 23,
		    [9] = 24,
		    [10] = 24
		},

		{
		    type = "attack",
		    name = "duration",
		    [1] = 20,
		    [2] = 20,
		    [3] = 21,
		    [4] = 21,
		    [5] = 22,
		    [6] = 22,
		    [7] = 23,
		    [8] = 23,
		    [9] = 24,
		    [10] = 24
		},

                {
                    type = "attack",
                    name = "damage",
                    [1] = 6,
                    [2] = 10,
                    [3] = 15,
                    [4] = 20,
                    [5] = 30,
                    [6] = 45,
                    [7] = 60,
                    [8] = 75,
                    [9] = 90,
                    [10] = 150
                },

		{
		    type = "resistance",
		    name = "electric",
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
			[1] = 3,
			[2] = 3,
			[3] = 7,
			[4] = 7,
			[5] = 10,
			[6] = 10,
			[7] = 13,
			[8] = 13,
			[9] = 16,
			[10] = 18
		    },
		    percent = {
			[1] = 35,
			[2] = 35,
			[3] = 40,
			[4] = 40,
			[5] = 45,
			[6] = 45,
			[7] = 50,
			[8] = 55,
			[9] = 55,
			[10] = 60
		    }
		},

		{
		    type = "attack",
		    name = "range",
		    [1] = 11,
		    [2] = 11,
		    [3] = 12,
		    [4] = 12,
		    [5] = 13,
		    [6] = 13,
		    [7] = 14,
		    [8] = 14,
		    [9] = 15,
		    [10] = 15
		}
	    },

	    unitSpawner = {

		{
		    type = "resistance",
		    name = "electric",
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
		    [2] = 0.12,
		    [3] = 0.17,
		    [4] = 0.32,
		    [5] = 0.42,
		    [6] = 0.57,
		    [7] = 0.72,
		    [8] = 0.82,
		    [9] = 0.87,
		    [10] = 0.92
		},
                
                {
		    type = "resistance",
		    name = "laser",
		    decrease = {
			[1] = 3,
			[2] = 3,
			[3] = 7,
			[4] = 7,
			[5] = 10,
			[6] = 10,
			[7] = 13,
			[8] = 13,
			[9] = 16,
			[10] = 18
		    },
		    percent = {
			[1] = 35,
			[2] = 35,
			[3] = 40,
			[4] = 40,
			[5] = 45,
			[6] = 45,
			[7] = 50,
			[8] = 55,
			[9] = 55,
			[10] = 60
		    }
		},
	    }
	},

	function (attributes)
	    return createElectricAttack(attributes,
					makeBeam(attributes),
					biterattackanimation(attributes.scale, attributes.tint, attributes.tint))
	end,

	{
	    unit = ELECTRIC_UNIT_VARIATIONS,
	    unitSpawner = ELECTRIC_NEST_VARIATIONS
	},

	{
	    unit = ELECTRIC_UNIT_TIERS,
	    unitSpawner = ELECTRIC_NEST_TIERS
	}
    )

    -- electric worms
    buildWorm(
	{
	    name = "electric-worm",

	    loot = wormLoot,
	    attributes = {},
	    attack = {
		type = "projectile",
		bubble = electricBubble,
		damageType = "electric",
		pointEffects = function(attributes)
		    return
			{
			    {
				type="nested-result",
				action = {
				    {
					type = "cluster",
					cluster_count = attributes.clusters,
					distance = attributes.clusterDistance,
					distance_deviation = 3,
					action_delivery =
					    {
						type = "projectile",
						projectile = attributes.laserName,
						duration = 20,
						direction_deviation = 0.6,
						starting_speed = attributes.startingSpeed,
						starting_speed_deviation = 0.3
					    }
				    }
				},
			    }
			}
		end
	    },
	    resistances = {},

	    attackName = "worm-electric",
	    tint = {r=0, g=0.25, b=0.83, a=0.65}
	},

	{
	    {
		type = "attack",
		name = "startingSpeed",
		[1] = 0.25,
		[2] = 0.25,
		[3] = 0.27,
		[4] = 0.27,
		[5] = 0.29,
		[6] = 0.29,
		[7] = 0.31,
		[8] = 0.31,
		[9] = 0.33,
		[10] = 0.33
	    },

            {
                type = "attribute",
                name = "evolutionRequirement",
                [1] = 0,
                [2] = 0.12,
                [3] = 0.17,
                [4] = 0.32,
                [5] = 0.42,
                [6] = 0.57,
                [7] = 0.72,
                [8] = 0.82,
                [9] = 0.87,
                [10] = 0.92
            },
            
	    {
		type = "attack",
		name = "clusterDistance",
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
		name = "clusters",
		min = 2,
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

	    {
		type = "resistance",
		name = "electric",
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
                    [1] = 3,
                    [2] = 3,
                    [3] = 7,
                    [4] = 7,
                    [5] = 10,
                    [6] = 10,
                    [7] = 13,
                    [8] = 13,
                    [9] = 16,
                    [10] = 18
                },
                percent = {
                    [1] = 35,
                    [2] = 35,
                    [3] = 40,
                    [4] = 40,
                    [5] = 45,
                    [6] = 45,
                    [7] = 50,
                    [8] = 55,
                    [9] = 55,
                    [10] = 60
                }
            }
	},

	function (attributes)
	    attributes.laserName = makeLaser(attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	ELECTRIC_WORM_VARIATIONS,
	ELECTRIC_WORM_TIERS
    )
end

return electric
