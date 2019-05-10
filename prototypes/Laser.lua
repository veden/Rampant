-- imports

local acidBall = require("utils/AttackBall")
local beamUtils = require("utils/BeamUtils")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

-- constants

local LASER_UNIT_TIERS = constants.LASER_UNIT_TIERS
local LASER_UNIT_VARIATIONS = constants.LASER_UNIT_VARIATIONS

local LASER_NEST_TIERS = constants.LASER_NEST_TIERS
local LASER_NEST_VARIATIONS = constants.LASER_NEST_VARIATIONS

local LASER_WORM_TIERS = constants.LASER_WORM_TIERS
local LASER_WORM_VARIATIONS = constants.LASER_WORM_VARIATIONS

-- imported functions

local laser = {}

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = acidBall.createAttackBall
local makeLaser = beamUtils.makeLaser
local createRangedAttack = biterUtils.createRangedAttack
local makeBubble = beamUtils.makeBubble

local createMeleeAttack = biterUtils.createMeleeAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local biterLoot = makeUnitAlienLootTable("blue")
local spawnerLoot = makeSpawnerAlienLootTable("blue")
local wormLoot = makeWormAlienLootTable("blue")

function laser.addFaction()

    local laserBubble = makeBubble({
	    name = "laser-worm",
	    tint = {r=0, g=0, b=0.42, a=0.65}
    })


    -- laser biters
    buildUnitSpawner(
	{
	    unit = {
		name = "laser-biter",

		attributes = {
		    damageType = "laser",
		    explosion = "blood-explosion-small"
		},
		attack = {},
		resistances = {},

		loot = biterLoot,
		type = "biter",
		tint = {r=0, g=0, b=0.42, a=0.65}
	    },

	    unitSpawner = {
		name = "laser-biter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},
		tint = {r=0, g=0, b=0.42, a=0.65}
	    }
	},

	{
	    unit = {

		{
		    type = "resistance",
		    name = "laser",
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
		    name = "electric",
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
		    name = "laser",
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
		    name = "electric",
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
	    }
	},

	createMeleeAttack,

	{
	    unit = LASER_UNIT_VARIATIONS,
	    unitSpawner = LASER_NEST_VARIATIONS
	},

	{
	    unit = LASER_UNIT_TIERS,
	    unitSpawner = LASER_NEST_TIERS
	}
    )

    -- laser spitters
    buildUnitSpawner(
	{
	    unit = {
		name = "laser-spitter",

		loot = biterLoot,
		attributes = {
		    damageType = "laser",
		    explosion = "blood-explosion-small"
		},
		attack = {
		    type = "projectile",
		    bubble = laserBubble,
		    damageType = "laser",
                    directionOnly = true,
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
						},
					    repeat_count = 2
					}
				    }
				}
			    }
		    end
		},
		resistances = {},

		type = "spitter",
		attackName = "laser-spitter",
		tint = {r=0, g=0, b=0.42, a=0.65}
	    },

	    unitSpawner = {
		name = "laser-spitter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},

		tint = {r=0, g=0, b=0.42, a=0.65}
	    }
	},

	{
	    unit = {

		{
		    type = "resistance",
		    name = "laser",
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
		    name = "electric",
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
		    [1] = 2,
		    [2] = 3,
		    [3] = 3,
		    [4] = 4,
		    [5] = 4,
		    [6] = 5,
		    [7] = 5,
		    [8] = 5,
		    [9] = 6,
		    [10] = 6
		}

	    },

	    unitSpawner = {

		{
		    type = "resistance",
		    name = "laser",
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
	    attributes.laserName = makeLaser(attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes),
				      spitterattackanimation(attributes.scale,
							     attributes.tint,
                                                             attributes.tint))
	end,

	{
	    unit = LASER_UNIT_VARIATIONS,
	    unitSpawner = LASER_NEST_VARIATIONS
	},

	{
	    unit = LASER_UNIT_TIERS,
	    unitSpawner = LASER_NEST_TIERS
	}
    )

    -- laser worms
    buildWorm(
	{
	    name = "laser-worm",

	    loot = wormLoot,
	    attributes = {},
	    attack = {
		type = "projectile",
		bubble = laserBubble,
		damageType = "laser",
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
					    },
					repeat_count = 3
				    }
				}
			    }
			}
		end
	    },
	    resistances = {},

	    attackName = "laser-worm",
	    tint = {r=0, g=0, b=0.42, a=0.65}
	},

	{

	    {
		type = "resistance",
		name = "laser",
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
                name = "electric",
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
	    }

	},

	function (attributes)
	    attributes.laserName = makeLaser(attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	LASER_WORM_VARIATIONS,
	LASER_WORM_TIERS
    )
end

return laser
