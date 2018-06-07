
-- imports

local biterUtils = require("utils/BiterUtils")
local beamUtils = require("utils/BeamUtils")
local attackBall = require("utils/AttackBall")
local swarmUtils = require("SwarmUtils")
package.path = "../libs/?.lua;" .. package.path
local constants = require("Constants")

-- constants

local energyThief = {}

local ENERGY_THIEF_UNIT_TIERS = constants.ENERGY_THIEF_UNIT_TIERS
local ENERGY_THIEF_UNIT_VARIATIONS = constants.ENERGY_THIEF_UNIT_VARIATIONS

local ENERGY_THIEF_NEST_TIERS = constants.ENERGY_THIEF_NEST_TIERS
local ENERGY_THIEF_NEST_VARIATIONS = constants.ENERGY_THIEF_NEST_VARIATIONS

local ENERGY_THIEF_WORM_TIERS = constants.ENERGY_THIEF_WORM_TIERS
local ENERGY_THIEF_WORM_VARIATIONS = constants.ENERGY_THIEF_WORM_VARIATIONS

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

function energyThief.addFaction()

    local biterLoot = makeUnitAlienLootTable("blue")
    local spawnerLoot = makeSpawnerAlienLootTable("blue")
    local wormLoot = makeWormAlienLootTable("blue")

    local electricBubble = makeBubble({
	    name = "energyThief-worm",
	    lTint = {r=0, g=0.8, b=0.8, a=1}
    })

    local softSmoke = "the-soft-smoke-rampant"

    -- energyThief biters
    buildUnitSpawner(
	{
	    unit = {
		name = "energyThief-biter",

		attributes = {
		    explosion = "blood-explosion-small"
		},
		loot = biterLoot,
		attack = {
		    damageType = "electric"
		},
		resistances = {},

		type = "biter",
		attackName = "biter-energyThief",
		tint1 = {r=0, g=0.8, b=0.83, a=0.65},
		tint2 = {r=0, g=0.6, b=0.63, a=0.65}
	    },

	    unitSpawner = {
		name = "energyThief-biter-nest",

		loot = spawnerLoot,
		attributes = {},	    
		resistances = {},
		tint = {r=0, g=0.8, b=0.83, a=0.65}
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
		    [5] = 750,
		    [6] = 1000,
		    [7] = 1500,
		    [8] = 2500,
		    [9] = 4500,
		    [10] = 7000
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
		}
	    }
	},

	function (attributes)
	    return createElectricAttack(attributes,
					makeBeam(attributes),
					biterattackanimation(attributes.scale, attributes.tint1, attributes.tint2))
	end,

	{
	    unit = ENERGY_THIEF_UNIT_VARIATIONS,
	    unitSpawner = ENERGY_THIEF_NEST_VARIATIONS
	},

	{
	    unit = ENERGY_THIEF_UNIT_TIERS,
	    unitSpawner = ENERGY_THIEF_NEST_TIERS
	}
    )

    -- energyThief worms
    buildWorm(
	{
	    name = "energyThief-worm",

	    loot = wormLoot,
	    attributes = {},
	    attack = {
		type = "projectile",
		bubble = electricBubble,
		damageType = "electric",
		softSmokeName = softSmoke,
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
	    attackName = "worm-energyThief",
	    tint = {r=0, g=0.8, b=0.83, a=0.65},
	    pTint = {r=0, g=0.8, b=0.8, a=1},
	    sTint = {r=0, g=0.8, b=0.8, a=1},
	    lTint = {r=0, g=0.8, b=0.8, a=1}
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
	    attributes.laserName = makeLaser(attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	ENERGY_THIEF_WORM_VARIATIONS,
	ENERGY_THIEF_WORM_TIERS
    )
end

return energyThief

{
    type = "electric-energy-interface",
    name = "electric-energy-interface",
    icon = "__base__/graphics/icons/accumulator.png",
    icon_size = 32,
    flags = {"placeable-neutral", "player-creation"},
    minable = {hardness = 0.2, mining_time = 0.5, result = "electric-energy-interface"},
    max_health = 150,
    corpse = "medium-remnants",
    collision_box = {{-0.9, -0.9}, {0.9, 0.9}},
    selection_box = {{-1, -1}, {1, 1}},
    enable_gui = true,
    allow_copy_paste = true,
    energy_source =
    {
      type = "electric",
      buffer_capacity = "10GJ",
      usage_priority = "terciary",
      input_flow_limit = "0kW",
      output_flow_limit = "500GW"
    },

    energy_production = "500GW",
    energy_usage = "0kW",
    -- also 'pictures' for 4-way sprite is available, or 'animation' resp. 'animations'
    picture =
    {
      filename = "__base__/graphics/entity/accumulator/accumulator.png",
      priority = "extra-high",
      width = 124,
      height = 103,
      shift = {0.6875, -0.203125},
      tint = {r=1, g=0.8, b=1, a=1}
    },
    vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65},
    working_sound =
    {
      sound =
      {
        filename = "__base__/sound/accumulator-working.ogg",
        volume = 1
      },
      idle_sound =
      {
        filename = "__base__/sound/accumulator-idle.ogg",
        volume = 0.4
      },
      max_sounds_per_type = 5
    }
}
