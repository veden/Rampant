-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local stickerUtils = require("utils/StickerUtils")
local bombUtils = require("utils/BombUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

-- constants

local nuclear = {}

local NUCLEAR_UNIT_TIERS = constants.NUCLEAR_UNIT_TIERS
local NUCLEAR_UNIT_VARIATIONS = constants.NUCLEAR_UNIT_VARIATIONS

local NUCLEAR_NEST_TIERS = constants.NUCLEAR_NEST_TIERS
local NUCLEAR_NEST_VARIATIONS = constants.NUCLEAR_NEST_VARIATIONS

local NUCLEAR_WORM_TIERS = constants.NUCLEAR_WORM_TIERS
local NUCLEAR_WORM_VARIATIONS = constants.NUCLEAR_WORM_VARIATIONS

-- imported functions

local makeSticker = stickerUtils.makeSticker
local makeAtomicBlast = bombUtils.makeAtomicBlast
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = acidBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createSuicideAttack = biterUtils.createSuicideAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

function nuclear.addFaction()


    local biterLoot = makeUnitAlienLootTable("yellow")
    local spawnerLoot = makeSpawnerAlienLootTable("yellow")
    local wormLoot = makeWormAlienLootTable("yellow")

    -- nuclear biters
    buildUnitSpawner(
	{
	    unit = {
		name = "nuclear-biter",

		loot = biterLoot,
		attributes = {
		    explosion = "blood-explosion-small"
		},
		attack = {
		    nuclear = true,
		    scorchmark = "small-scorchmark"
		},
		resistances = {},

		attackName = "nuclear-biter",
		type = "biter",
		tint = {r=0.76, g=0.76, b=0, a=0.65}
	    },

	    unitSpawner = {
		name = "nuclear-biter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},
		tint = {r=0.76, g=0.76, b=0, a=0.65}
	    }
	},

	{
	    unit = {
		{

		    type = "attribute",
		    name = "health",
		    [1] = 7,
		    [2] = 42,
		    [3] = 75,
		    [4] = 125,
		    [5] = 200,
		    [6] = 350,
		    [7] = 750,
		    [8] = 1500,
		    [9] = 5000,
		    [10] = 10000
		},

		{
		    type = "attribute",
		    name = "spawningTimeModifer",
		    [1] = 0,
		    [2] = 0,
		    [3] = 1,
		    [4] = 2,
		    [5] = 3,
		    [6] = 5,
		    [7] = 6,
		    [8] = 6,
		    [9] = 8,
		    [10] = 8
		},

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
		    type = "attack",
		    name = "radius",
		    [1] = 5,
		    [2] = 10,
		    [3] = 10,
		    [4] = 12,
		    [5] = 14,
		    [6] = 16,
		    [7] = 16,
		    [8] = 18,
		    [9] = 18,
		    [10] = 20
		},

		{
		    type = "attack",
		    name = "repeatCount",
		    [1] = 150,
		    [2] = 175,
		    [3] = 250,
		    [4] = 300,
		    [5] = 350,
		    [6] = 400,
		    [7] = 450,
		    [8] = 500,
		    [9] = 550,
		    [10] = 600
		},

		{
		    type = "attack",
		    name = "damage",
		    [1] = 50,
		    [2] = 60,
		    [3] = 80,
		    [4] = 100,
		    [5] = 120,
		    [6] = 130,
		    [7] = 140,
		    [8] = 150,
		    [9] = 180,
		    [10] = 200
		},

		{
		    type = "attribute",
		    name = "movement",
		    [1] = 0.23,
		    [2] = 0.23,
		    [3] = 0.22,
		    [4] = 0.22,
		    [5] = 0.21,
		    [6] = 0.21,
		    [7] = 0.2,
		    [8] = 0.2,
		    [9] = 0.19,
		    [10] = 0.19
		},

		{
		    type = "attribute",
		    name = "distancePerFrame",
		    [1] = 0.12,
		    [2] = 0.145,
		    [3] = 0.17,
		    [4] = 0.21,
		    [5] = 0.21,
		    [6] = 0.22,
		    [7] = 0.22,
		    [8] = 0.23,
		    [9] = 0.23,
		    [10] = 0.24
		},

		{
		    type = "resistance",
		    name = "explosion",
		    decrease = {
			[1] = -7,
			[2] = -7,
			[3] = -10,
			[4] = -10,
			[5] = -13,
			[6] = -13,
			[7] = -16,
			[8] = -16,
			[9] = -19,
			[10] = -23
		    },
		    percent = {
			[1] = -65,
			[2] = -65,
			[3] = -70,
			[4] = -75,
			[5] = -75,
			[6] = -80,
			[7] = -85,
			[8] = -85,
			[9] = -90,
			[10] = -90
		    }
		}
	    },

	    unitSpawner = {

		{

		    type = "attribute",
		    name = "spawingCooldownStart",
		    [1] = 330,
		    [2] = 330,
		    [3] = 325,
		    [4] = 325,
		    [5] = 320,
		    [6] = 320,
		    [7] = 315,
		    [8] = 315,
		    [9] = 310,
		    [10] = 310
		},

		{

		    type = "attribute",
		    name = "spawingCooldownEnd",
		    [1] = 120,
		    [2] = 120,
		    [3] = 115,
		    [4] = 115,
		    [5] = 110,
		    [6] = 110,
		    [7] = 105,
		    [8] = 105,
		    [9] = 100,
		    [10] = 100
		},

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
		},

		{
		    type = "attribute",
		    name = "unitsOwned",
		    [1] = 1,
		    [2] = 2,
		    [3] = 3,
		    [4] = 4,
		    [5] = 4,
		    [6] = 5,
		    [7] = 5,
		    [8] = 6,
		    [9] = 6,
		    [10] = 6
		},

		{
		    type = "attribute",
		    name = "unitsToSpawn",
		    [1] = 1,
		    [2] = 2,
		    [3] = 3,
		    [4] = 3,
		    [5] = 4,
		    [6] = 4,
		    [7] = 5,
		    [8] = 5,
		    [9] = 5,
		    [10] = 5
		},

		{
		    type = "resistance",
		    name = "explosion",
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
			[1] = 1,
			[2] = 1,
			[3] = 2,
			[4] = 2,
			[5] = 3,
			[6] = 3,
			[7] = 4,
			[8] = 4,
			[9] = 5,
			[10] = 5
		    },
		    percent = {
			[1] = 40,
			[2] = 40,
			[3] = 42,
			[4] = 42,
			[5] = 43,
			[6] = 43,
			[7] = 44,
			[8] = 44,
			[9] = 45,
			[10] = 45
		    }
		}
	    }
	},

	function (attributes)
	    return createSuicideAttack(attributes,
				       makeAtomicBlast(attributes))
	end,

	{
	    unit = NUCLEAR_UNIT_VARIATIONS,
	    unitSpawner = NUCLEAR_NEST_VARIATIONS
	},

	{
	    unit = NUCLEAR_UNIT_TIERS,
	    unitSpawner = NUCLEAR_NEST_TIERS
	}
    )

    -- nuclear worms
    buildWorm(
	{
	    name = "nuclear-worm",

	    loot = wormLoot,
	    attributes = {},
	    attack = {
		type = "projectile",
		force = "enemy",
		stickerAnimation = {
		    filename = "__base__/graphics/entity/slowdown-sticker/slowdown-sticker.png",
		    priority = "extra-high",
		    width = 11,
		    height = 11,
		    frame_count = 13,
		    animation_speed = 0.4
		},
		areaEffects = function (attributes)
		    return {
			{
			    type = "damage",
			    damage = { amount = attributes.damage, type = "acid" }
			},
			{
			    type = "create-sticker",
			    sticker = attributes.name .. "-sticker-rampant"
			}
		    }
		end
	    },
	    resistances = {},

	    attackName = "nuclear-worm",
	    tint = {r=0.76, g=0.76, b=0, a=0.65}
	},

	{

	    {
		type = "attack",
		name = "stickerMovementModifier",
		[1] = 0.8,
		[2] = 0.8,
		[3] = 0.7,
		[4] = 0.7,
		[5] = 0.6,
		[6] = 0.6,
		[7] = 0.5,
		[8] = 0.5,
		[9] = 0.4,
		[10] = 0.4
	    },

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
		type = "resistance",
		name = "explosion",
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
		    [1] = 1,
		    [2] = 1,
		    [3] = 2,
		    [4] = 2,
		    [5] = 3,
		    [6] = 3,
		    [7] = 4,
		    [8] = 4,
		    [9] = 5,
		    [10] = 5
		},
		percent = {
		    [1] = 40,
		    [2] = 40,
		    [3] = 42,
		    [4] = 42,
		    [5] = 43,
		    [6] = 43,
		    [7] = 44,
		    [8] = 44,
		    [9] = 45,
		    [10] = 45
		}
	    }

	},

	function (attributes)
	    makeSticker(attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	NUCLEAR_WORM_VARIATIONS,
	NUCLEAR_WORM_TIERS
    )
end

return nuclear
