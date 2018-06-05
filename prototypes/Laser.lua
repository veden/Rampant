-- imports

local acidBall = require("utils/AttackBall")
local beamUtils = require("utils/BeamUtils")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

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

local softSmoke = "the-soft-smoke-rampant"

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local biterLoot = makeUnitAlienLootTable("blue")
local spawnerLoot = makeSpawnerAlienLootTable("blue")
local wormLoot = makeWormAlienLootTable("blue")

function laser.addFaction()

    local laserBubble = makeBubble({
	    name = "laser-worm",
	    lTint = {r=0, g=0, b=0.42, a=0.65}
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
		tint1 = {r=0, g=0, b=0.42, a=0.65},
		tint2 = {r=0, g=0, b=0.42, a=0.4}
	    },

	    unitSpawner = {
		name = "laser-biter-nest",

		loot = spawnerLoot,
		attributes = {},	    
		resistances = {},
		tint = {r=0, g=0, b=0.42, a=0.65}
	    }
	},

	{
	    unit = {
		
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
		    softSmokeName = softSmoke,
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
		tint = {r=0, g=0, b=0.42, a=0.65},
		pTint = {r=0, g=0, b=1, a=0.5},
		sTint = {r=0, g=0, b=1, a=0.5}
	    },

	    unitSpawner = {
		name = "laser-spitter-nest",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},
		
		tint = {r=0, g=0, b=0.42, a=0.65}
	    }
	},

	{
	    unit = {

		{		
		    type = "attack",
		    name = "damage",
		    [1] = 4,
		    [2] = 9,
		    [3] = 14,
		    [4] = 23,
		    [5] = 30,
		    [6] = 37,
		    [7] = 45,
		    [8] = 57,
		    [9] = 70,
		    [10] = 80
		},

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
		},
		
		{
		    type = "attack",
		    name = "range",
		    [1] = 13,
		    [2] = 13,
		    [3] = 14,
		    [4] = 14,
		    [5] = 15,
		    [6] = 15,
		    [7] = 16,
		    [8] = 16,
		    [9] = 17,
		    [10] = 17
		},

		{
		    type = "attack",
		    name = "radius",
		    [1] = 1.2,
		    [2] = 1.3,
		    [3] = 1.4,
		    [4] = 1.5,
		    [5] = 1.6,
		    [6] = 1.7,
		    [7] = 1.8,
		    [8] = 1.9,
		    [9] = 2.0,
		    [10] = 2.5
		},

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
		softSmokeName = softSmoke,
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
	    tint = {r=0, g=0, b=0.42, a=0.65},
	    pTint = {r=0, g=0, b=1, a=0.5},
	    sTint = {r=0, g=0, b=1, a=0.5},
	    lTint = {r=0, g=0, b=1, a=0.5}
	},

	{
	    
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
