-- imports

local acidBall = require("utils/AttackBall")
local droneUtils = require("utils/DroneUtils")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

-- constants

local spawner = {}

local SPAWNER_UNIT_TIERS = constants.SPAWNER_UNIT_TIERS
local SPAWNER_UNIT_VARIATIONS = constants.SPAWNER_UNIT_VARIATIONS

local SPAWNER_NEST_TIERS = constants.SPAWNER_NEST_TIERS
local SPAWNER_NEST_VARIATIONS = constants.SPAWNER_NEST_VARIATIONS

local SPAWNER_WORM_TIERS = constants.SPAWNER_WORM_TIERS
local SPAWNER_WORM_VARIATIONS = constants.SPAWNER_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local buildUnits = swarmUtils.buildUnits
local createMeleeAttack = biterUtils.createMeleeAttack
local createProjectileAttack = biterUtils.createProjectileAttack

local biterAttackSounds = biterUtils.biterAttackSounds

local createCapsuleProjectile = droneUtils.createCapsuleProjectile

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

function spawner.addFaction()

    local biterLoot = makeUnitAlienLootTable("orange")
    local spawnerLoot = makeSpawnerAlienLootTable("orange")
    local wormLoot = makeWormAlienLootTable("orange")

    -- spawner
    buildUnits(
	{
	    name = "spawner-drone",

	    attributes = {
		followsPlayer = false
	    },
	    attack = {
		checkBuildability = true
	    },
	    death = function (attack, attributes)
		return {
		    {
			type = "cluster",
			cluster_count = attack.clusters,
			distance = attack.clusterDistance,
			distance_deviation = 3,
			action_delivery =
			    {
				type = "projectile",
				projectile = createCapsuleProjectile("spawner-drone-sub" .. attributes.tier,
								     attack,
								     "spawner-biter" ..  attributes.tier .. "-rampant"),
				direction_deviation = 0.6,
				starting_speed = 0.25,
				max_range = attack.range,
				starting_speed_deviation = 0.3
			    }
		    }
		}
	    end,
	    scales = {
		[1] = 0.5,
		[2] = 0.5,
		[3] = 0.6,
		[4] = 0.6,
		[5] = 0.7,
		[6] = 0.7,
		[7] = 0.8,
		[8] = 0.8,
		[9] = 0.9,
		[10] = 0.9
	    },
	    resistances = {},

	    type = "drone",
	    attackName = "spawner-drone",
	    tint = {r=1, g=0, b=1, a=1}
	},
	function (attack)
	    return {
		type = "projectile",
		ammo_category = "biological",
		cooldown = attack.cooldown or 20,
		projectile_center = {0, 1},
		projectile_creation_distance = 0.6,
		range = attack.range or 15,
		sound = biterAttackSounds(),
		ammo_type =
		    {
			category = "biological",
			action =
			    {
				type = "direct",
				action_delivery =
				    {
					type = "instant",
					damage = { damage = 0, damageType = "acid" }
				    }
			    }
		    }
	    }
	end,
	{
	    {
		type = "attribute",
		name = "health",
		[1] = 100,
		[2] = 120,
		[3] = 120,
		[4] = 170,
		[5] = 170,
		[6] = 200,
		[7] = 200,
		[8] = 250,
		[9] = 300,
		[10] = 350
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
	    },

	    {
		type = "attribute",
		name = "ttl",
		[1] = 400,
		[2] = 400,
		[3] = 450,
		[4] = 450,
		[5] = 500,
		[6] = 500,
		[7] = 550,
		[8] = 550,
		[9] = 600,
		[10] = 600
	    },

	    {
		type = "attack",
		name = "damage",
		[1] = 2,
		[2] = 4,
		[3] = 7,
		[4] = 13,
		[5] = 15,
		[6] = 18,
		[7] = 22,
		[8] = 28,
		[9] = 35,
		[10] = 40
	    },

	    {
		type = "attribute",
		name = "movement",
		[1] = 0.00,
		[2] = 0.00,
		[3] = 0.00,
		[4] = 0.00,
		[5] = 0.00,
		[6] = 0.00,
		[7] = 0.00,
		[8] = 0.00,
		[9] = 0.00,
		[10] = 0.00
	    },
	    {
		type = "attribute",
		name = "distancePerFrame",
		[1] = 0.013,
		[2] = 0.013,
		[3] = 0.014,
		[4] = 0.014,
		[5] = 0.015,
		[6] = 0.015,
		[7] = 0.016,
		[8] = 0.016,
		[9] = 0.017,
		[10] = 0.017
	    },

	    {
		type = "attack",
		name = "rangeFromPlayer",
		[1] = 10,
		[2] = 10,
		[3] = 11,
		[4] = 11,
		[5] = 12,
		[6] = 12,
		[7] = 13,
		[8] = 13,
		[9] = 14,
		[10] = 14
	    },

	    {
		type = "attack",
		name = "range",
		[1] = 10,
		[2] = 10,
		[3] = 11,
		[4] = 11,
		[5] = 12,
		[6] = 12,
		[7] = 13,
		[8] = 13,
		[9] = 14,
		[10] = 14
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
	    }

	},
	SPAWNER_UNIT_VARIATIONS,
	SPAWNER_UNIT_TIERS
    )

    buildUnits(
	{
	    name = "spawner-worm-drone",

	    attributes = {
		followsPlayer = false
	    },
	    attack = {
		checkBuildability = true
	    },
	    death = function (attack, attributes)
		return {
		    {
			type = "cluster",
			cluster_count = attack.clusters,
			distance = attack.clusterDistance,
			distance_deviation = 3,
			action_delivery =
			    {
				type = "projectile",
				projectile = createCapsuleProjectile("spawner-drone-sub" .. attributes.tier,
								     attack,
								     "spawner-biter" ..  attributes.tier .. "-rampant"),
				direction_deviation = 0.6,
				starting_speed = 0.25,
				max_range = attack.range,
				starting_speed_deviation = 0.3
			    }
		    }
		}
	    end,
	    scales = {
		[1] = 0.5,
		[2] = 0.5,
		[3] = 0.6,
		[4] = 0.6,
		[5] = 0.7,
		[6] = 0.7,
		[7] = 0.8,
		[8] = 0.8,
		[9] = 0.9,
		[10] = 0.9
	    },
	    resistances = {},

	    type = "drone",
	    attackName = "spawner-drone",
	    tint = {r=1, g=0, b=1, a=1}
	},
	function (attack)
	    return {
		type = "projectile",
		ammo_category = "biological",
		cooldown = attack.cooldown or 20,
		projectile_center = {0, 1},
		projectile_creation_distance = 0.6,
		range = attack.range or 15,
		sound = biterAttackSounds(),
		ammo_type =
		    {
			category = "biological",
			action =
			    {
				type = "direct",
				action_delivery =
				    {
					type = "instant",
					damage = { damage = 0, damageType = "acid" }
				    }
			    }
		    }
	    }
	end,
	{
	    {
		type = "attribute",
		name = "health",
		[1] = 100,
		[2] = 100,
		[3] = 110,
		[4] = 110,
		[5] = 120,
		[6] = 120,
		[7] = 130,
		[8] = 130,
		[9] = 140,
		[10] = 140
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
	    },

	    {
		type = "attribute",
		name = "ttl",
		[1] = 400,
		[2] = 400,
		[3] = 450,
		[4] = 450,
		[5] = 500,
		[6] = 500,
		[7] = 550,
		[8] = 550,
		[9] = 600,
		[10] = 600
	    },

	    {
		type = "attack",
		name = "damage",
		[1] = 2,
		[2] = 4,
		[3] = 7,
		[4] = 13,
		[5] = 15,
		[6] = 18,
		[7] = 22,
		[8] = 28,
		[9] = 35,
		[10] = 40
	    },

	    {
		type = "attribute",
		name = "movement",
		[1] = 0.0,
		[2] = 0.0,
		[3] = 0.0,
		[4] = 0.0,
		[5] = 0.0,
		[6] = 0.0,
		[7] = 0.0,
		[8] = 0.0,
		[9] = 0.0,
		[10] = 0.0
	    },
	    {
		type = "attribute",
		name = "distancePerFrame",
		[1] = 0.013,
		[2] = 0.013,
		[3] = 0.014,
		[4] = 0.014,
		[5] = 0.015,
		[6] = 0.015,
		[7] = 0.016,
		[8] = 0.016,
		[9] = 0.017,
		[10] = 0.017
	    },

	    {
		type = "attack",
		name = "rangeFromPlayer",
		[1] = 10,
		[2] = 10,
		[3] = 11,
		[4] = 11,
		[5] = 12,
		[6] = 12,
		[7] = 13,
		[8] = 13,
		[9] = 14,
		[10] = 14
	    },

	    {
		type = "attack",
		name = "range",
		[1] = 10,
		[2] = 10,
		[3] = 11,
		[4] = 11,
		[5] = 12,
		[6] = 12,
		[7] = 13,
		[8] = 13,
		[9] = 14,
		[10] = 14
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
	    }

	},
	SPAWNER_WORM_VARIATIONS,
	SPAWNER_WORM_TIERS
    )

    -- spawner units
    buildUnits(
	{
	    name = "spawner-biter",

	    attributes = {
		explosion = "blood-explosion-small"
	    },
	    attack = {
	    },
	    scales = {
		[1] = 0.3,
		[2] = 0.3,
		[3] = 0.4,
		[4] = 0.4,
		[5] = 0.5,
		[6] = 0.5,
		[7] = 0.6,
		[8] = 0.6,
		[9] = 0.7,
		[10] = 0.7
	    },
	    resistances = {},

	    type = "biter",
	    tint = {r=1, g=0, b=1, a=1}
	},
	createMeleeAttack,
	{
	    {
		type = "attribute",
		name = "health",
		[1] = 30,
		[2] = 60,
		[3] = 90,
		[4] = 120,
		[5] = 150,
		[6] = 180,
		[7] = 220,
		[8] = 500,
		[9] = 1000,
		[10] = 2000
	    },
            
            {
		type = "attribute",
		name = "healing",
		[1] = -0.05,
		[2] = -0.05,
		[3] = -0.05,
		[4] = -0.06,
		[5] = -0.07,
		[6] = -0.07,
		[7] = -0.08,
		[8] = -0.08,
		[9] = -0.09,
		[10] = -0.09
	    }

	},
	math.max(SPAWNER_UNIT_VARIATIONS,SPAWNER_WORM_VARIATIONS),
	math.max(SPAWNER_UNIT_TIERS,SPAWNER_WORM_TIERS)
    )


    -- spawner spitters
    buildUnitSpawner(
	{
	    unit = {
		name = "spawner-spitter",

		loot = biterLoot,
		attributes = {
		    explosion = "blood-explosion-small"
		},
		attack = {
		    type = "projectile",
		    triggerCreated = false,
                    directionOnly = true,
		    sourceEffect = function (attributes)
			return
			    {
				{
				    type = "damage",
				    affects_target = true,
				    damage = {amount = attributes.healthDamage or 5, type = attributes.damageType or "physical"}
				}
			    }
		    end
		},
		resistances = {},

		type = "spitter",
		attackName = "spawner-drone",
		tint = {r=1, g=0, b=1, a=1}
	    },

	    unitSpawner = {
		name = "spawner-spitter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},

		tint = {r=1, g=0, b=1, a=1}
	    }
	},

	{
	    unit = {

		{
		    type = "attack",
		    name = "cooldown",
		    [1] = 180,
		    [2] = 180,
		    [3] = 177,
		    [4] = 177,
		    [5] = 175,
		    [6] = 175,
		    [7] = 173,
		    [8] = 173,
		    [9] = 170,
		    [10] = 170
		}

	    },

	    unitSpawner = {
                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    [1] = 0,
                    [2] = 0.17,
                    [3] = 0.27,
                    [4] = 0.37,
                    [5] = 0.47,
                    [6] = 0.57,
                    [7] = 0.67,
                    [8] = 0.77,
                    [9] = 0.87,
                    [10] = 0.97
                }
	    }

	},

	function (attack, attributes)
	    local divider
	    if attributes.health < 100 then
		divider = 2
	    else
		divider = 2.5
	    end
	    attack.healthDamage = attributes.health / divider
	    return createProjectileAttack(attack,
					  createCapsuleProjectile(attack.name,
								  attack,
								  attack.name .. "-drone-rampant"),
					  spitterattackanimation(attack.scale, attack.tint, attack.tint))
	end,

	{
	    unit = SPAWNER_UNIT_VARIATIONS,
	    unitSpawner = SPAWNER_NEST_VARIATIONS
	},

	{
	    unit = SPAWNER_UNIT_TIERS,
	    unitSpawner = SPAWNER_NEST_TIERS
	}
    )

    -- spawner worms
    buildWorm(
	{
	    name = "spawner-worm",

	    loot = wormLoot,
	    attributes = {
	    },
	    attack = {
		type = "projectile",
		triggerCreated = false
	    },
	    resistances = {},

	    attackName = "spawner-worm-drone",
	    tint = {r=1, g=0, b=1, a=1}
	},

	{

            {
                type = "attribute",
                name = "evolutionRequirement",
                [1] = 0,
                [2] = 0.17,
                [3] = 0.27,
                [4] = 0.37,
                [5] = 0.47,
                [6] = 0.57,
                [7] = 0.67,
                [8] = 0.77,
                [9] = 0.87,
                [10] = 0.97
            }
            
	},

	function (attributes)
	    return createProjectileAttack(attributes,
					  createCapsuleProjectile(attributes.name,
								  attributes,
								  attributes.name .. "-drone-rampant"))
	end,

	SPAWNER_WORM_VARIATIONS,
	SPAWNER_WORM_TIERS
    )
end

return spawner
