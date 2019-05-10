
-- imports

local thiefUtils = require("utils/ThiefUtils")
local biterUtils = require("utils/BiterUtils")
local beamUtils = require("utils/BeamUtils")
local attackBall = require("utils/AttackBall")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

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
local makeDrainCrystal = thiefUtils.makeDrainCrystal

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

function energyThief.addFaction()

    local biterLoot = makeUnitAlienLootTable("blue")
    local spawnerLoot = makeSpawnerAlienLootTable("blue")
    local wormLoot = makeWormAlienLootTable("blue")

    local electricBubble = makeBubble({
	    name = "energy-thief-worm",
	    tint = {r=0, g=0, b=1, a=1}
    })

    -- energy-thief biters
    buildUnitSpawner(
	{
	    unit = {
		name = "energy-thief-biter",

		attributes = {
		    explosion = "blood-explosion-small"
		},
		loot = biterLoot,
		attack = {
		    damageType = "electric",
                    actions = function(attributes, electricBeam)
                        return
                            {
                                {
                                    type = "instant",
                                    target_effects =
                                        {
                                            type = "create-entity",
                                            trigger_created_entity = true,
                                            entity_name = "drain-trigger-rampant"
                                        }
                                },
                                {
				    type = "beam",
				    beam = electricBeam or "electric-beam",
				    duration = attributes.duration or 20
				}
                            }
                    end
		},
		resistances = {},

		type = "biter",
		attackName = "biter-energy-thief",
		tint = {r=0, g=0, b=0.83, a=0.65}
	    },

	    unitSpawner = {
		name = "energy-thief-biter-spawner",

		loot = spawnerLoot,
		attributes = {},
		resistances = {},
		tint = {r=0, g=0, b=0.83, a=0.65}
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
		    [1] = 1,
		    [2] = 1,
		    [3] = 1.2,
		    [4] = 1.2,
		    [5] = 1.3,
		    [6] = 1.3,
		    [7] = 1.4,
		    [8] = 1.4,
		    [9] = 1.5,
		    [10] = 1.5
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
                    name = "damage",
                    [1] = 4,
                    [2] = 8,
                    [3] = 15,
                    [4] = 20,
                    [5] = 25,
                    [6] = 35,
                    [7] = 50,
                    [8] = 65,
                    [9] = 80,
                    [10] = 140
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
		    [1] = 9,
		    [2] = 9,
		    [3] = 10,
		    [4] = 10,
		    [5] = 11,
		    [6] = 11,
		    [7] = 12,
		    [8] = 12,
		    [9] = 13,
		    [10] = 13
		}
	    },

	    unitSpawner = {

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
					biterattackanimation(attributes.scale, attributes.tint, attributes.tint))
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

    -- energy-thief worms
    buildWorm(
	{
	    name = "energy-thief-worm",

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
	    attackName = "worm-energy-thief",
	    tint = {r=0, g=0, b=0.83, a=0.65}
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

	},

	function (attributes)
	    attributes.laserName = makeLaser(attributes)
	    return createRangedAttack(attributes,
				      createAttackBall(attributes))
	end,

	ENERGY_THIEF_WORM_VARIATIONS,
	ENERGY_THIEF_WORM_TIERS
    )

    data:extend({
            {
                type = "simple-entity-with-force",
                name = "drain-trigger-rampant",
                render_layer = "object",
                icon = "__base__/graphics/icons/steel-chest.png",
                icon_size = 32,
                flags = {"placeable-neutral", "player-creation"},
                order = "s-e-w-f",
                minable = {mining_time = 1, result = "drain-trigger-rampant"},
                max_health = 100,
                selectable_in_game = false,
                corpse = "small-remnants",
                collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
                selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
                picture =
                    {
                        filename = "__core__/graphics/empty.png",
                        priority = "extra-high",
                        width = 1,
                        height = 1,
                        shift = {0, 0}
                    }
            },

            {
                type = "item",
                name = "drain-trigger-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                flags = {"hidden"},
                subgroup = "energy",
                order = "e[accumulator]-a[accumulator]",
                place_result = "drain-trigger-rampant",
                stack_size = 50
            },

            {
                type = "item",
                name = "crystal-drain-pole-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                flags = {"hidden"},
                subgroup = "energy",
                order = "e[accumulator]-a[accumulator]",
                place_result = "crystal-drain-pole-rampant",
                stack_size = 50
            },

            {
                type = "electric-pole",
                name = "crystal-drain-pole-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                flags = {"hidden"},
                selectable_in_game = false,
                minable = {hardness = 0.2, mining_time = 0.5, result = "big-electric-pole"},
                max_health = 750,
                healing_per_tick = 0.02,
                corpse = "medium-remnants",
                resistances =
                    {
                        {
                            type = "physical",
                            percent = 25
                        },
                        {
                            type = "fire",
                            percent = 85
                        },
                        {
                            type = "electric",
                            percent = 95
                        },
                        {
                            type = "laser",
                            percent = 90
                        }
                    },
                collision_box = {{-0.55, -0.55}, {0.55, 0.55}},
                selection_box = {{-0.55, -0.55}, {0.55, 0.55}},
                drawing_box = {{-1, -3}, {1, 0.5}},
                maximum_wire_distance = 30,
                supply_area_distance = 9,
                vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
                pictures =
                    {
                        filename = "__Rampant__/graphics/entities/thief/crystal-drain-pole.png",
                        priority = "high",
                        width = 168,
                        height = 130,
                        direction_count = 4,
                        shift = {1.6, -1.4}
                    },
                connection_points =
                    {
                        {
                            shadow =
                                {
                                    copper = {2.7, 0},
                                    green = {1.8, 0},
                                    red = {3.6, 0}
                                },
                            wire =
                                {
                                    copper = {0, -2.5},
                                    green = {-0.59375, -2.5},
                                    red = {0.625, -2.5}
                                }
                        },
                        {
                            shadow =
                                {
                                    copper = {3.1, 0.2},
                                    green = {2.3, -0.3},
                                    red = {3.8, 0.6}
                                },
                            wire =
                                {
                                    copper = {-0.0625, -2.5},
                                    green = {-0.5, -3},
                                    red = {0.34375, -2}
                                }
                        },
                        {
                            shadow =
                                {
                                    copper = {2.9, 0.06},
                                    green = {3.0, -0.6},
                                    red = {3.0, 0.8}
                                },
                            wire =
                                {
                                    copper = {-0.09375, -2.5},
                                    green = {-0.09375, -3},
                                    red = {-0.09375, -2}
                                }
                        },
                        {
                            shadow =
                                {
                                    copper = {3.1, 0.2},
                                    green = {3.8, -0.3},
                                    red = {2.35, 0.6}
                                },
                            wire =
                                {
                                    copper = {-0.0625, -2.4},
                                    green = {0.375, -2.9},
                                    red = {-0.46875, -2.4}
                                }
                        }
                    },
                radius_visualisation_picture =
                    {
                        filename = "__base__/graphics/entity/small-electric-pole/electric-pole-radius-visualization.png",
                        width = 12,
                        height = 12,
                        priority = "extra-high-no-scale"
                    }
            }
    })

    local chest = util.table.deepcopy(data.raw["radar"]["radar"])
    chest.name = "pylon-target-rampant"
    chest.icon = "__Rampant__/graphics/icons/thief/crystal-drain.png"
    chest.flags = {"not-repairable", "not-on-map", "hidden"}
    chest.subgroup = "enemies"
    chest.pictures = {
        layers={
            {
                filename = "__core__/graphics/empty.png",
                priority = "low",
                width = 1,
                height = 1,
                direction_count = 1,
                line_length = 1,
                shift = {0, 0}
            }
    }}
    chest.max_health = 750
    chest.resistances =
        {
            {
                type = "physical",
                percent = 25
            },
            {
                type = "fire",
                percent = 85
            },
            {
                type = "electric",
                percent = 95
            },
            {
                type = "laser",
                percent = 90
            }
        }
    chest.energy_usage = "500kW"
    -- chest.collision_mask = {}
    chest.collision_box = nil
    chest.selection_box = {{-0.55, -0.55}, {0.55, 0.55}}
    chest.minable.result = "pylon-target-rampant"
    chest.working_sound = {
        sound = {
            {
                filename = "__base__/sound/accumulator-working.ogg"
            }
        },
        apparent_volume = 2,
    }

    data:extend({
            chest,

            {
                type = "item",
                name = "pylon-target-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                flags = {"hidden"},
                subgroup = "enemies",
                order = "a[items]-h[steel-collector]",
                place_result = "pylon-target-rampant",
                stack_size = 50
            }
    })


    for i=1,10 do
        local drainCrystalAttributes = {
            name = "crystal-v" .. i,
            drain = i * 1.3 .. "MW",
            scale = (i * 0.1) + 0.5,
            health = 400 * i
        }

        makeDrainCrystal(drainCrystalAttributes)
    end

end

return energyThief
