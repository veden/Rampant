if constantsG then
    return constantsG
end
local constants = {}

-- versions

constants.VERSION_5 = 5
constants.VERSION_10 = 10
constants.VERSION_11 = 11
constants.VERSION_12 = 12
constants.VERSION_16 = 16
constants.VERSION_18 = 18
constants.VERSION_20 = 20
constants.VERSION_22 = 22
constants.VERSION_23 = 23
constants.VERSION_25 = 25
constants.VERSION_26 = 26
constants.VERSION_27 = 27
constants.VERSION_28 = 28
constants.VERSION_33 = 33
constants.VERSION_38 = 38
constants.VERSION_41 = 41
constants.VERSION_44 = 44
constants.VERSION_51 = 51
constants.VERSION_57 = 57
constants.VERSION_72 = 72
constants.VERSION_73 = 73
constants.VERSION_75 = 75
constants.VERSION_76 = 76
constants.VERSION_77 = 77
constants.VERSION_85 = 85
constants.VERSION_86 = 86
constants.VERSION_87 = 87
constants.VERSION_88 = 88

-- misc

-- constants.WATER_TILE_NAMES = { "water", "deepwater", "water-green", "deepwater-green" }

constants.MAGIC_MAXIMUM_NUMBER = 1e99 -- used in loops trying to find the lowest/highest score
constants.MAGIC_MAXIMUM_BASE_NUMBER = 100000000
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN = 13000
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX = 221000

constants.PROCESS_QUEUE_SIZE = 85
constants.SCAN_QUEUE_SIZE = 10
constants.ATTACK_QUEUE_SIZE = 18
constants.BASE_QUEUE_SIZE = 1
constants.SQUAD_QUEUE_SIZE = 2
constants.PROCESS_PLAYER_BOUND = 128

constants.TICKS_A_SECOND = 60
constants.TICKS_A_MINUTE = constants.TICKS_A_SECOND * 60

constants.CHUNK_PASS_THRESHOLD = 0.25

constants.INTERVAL_PLAYER_PROCESS = (settings.startup["rampant-liteMode"].value and 124) or 62
constants.INTERVAL_MAP_PROCESS = (settings.startup["rampant-liteMode"].value and 8) or 5
constants.INTERVAL_SCAN = (settings.startup["rampant-liteMode"].value and 42) or 21
constants.INTERVAL_CHUNK = 17
constants.INTERVAL_LOGIC = 61
constants.INTERVAL_SQUAD = 41
constants.INTERVAL_RESQUAD = 101
constants.INTERVAL_BUILDERS = 300
constants.INTERVAL_SPAWNER = constants.TICKS_A_SECOND * 10
constants.INTERVAL_RALLY = constants.TICKS_A_SECOND * 10
constants.INTERVAL_RETREAT = constants.TICKS_A_SECOND * 10

constants.RESOURCE_NORMALIZER = 1 / 1024

constants.PLAYER_PHEROMONE_MULTIPLER = 2500

-- chunk properties

constants.CHUNK_SIZE = 32
constants.CHUNK_AND_HALF_SIZE = constants.CHUNK_SIZE * 1.5
constants.DOUBLE_CHUNK_SIZE = constants.CHUNK_SIZE * 2
constants.TRIPLE_CHUNK_SIZE = constants.CHUNK_SIZE * 3
constants.HALF_CHUNK_SIZE = constants.CHUNK_SIZE / 2
constants.QUARTER_CHUNK_SIZE = constants.HALF_CHUNK_SIZE / 2

constants.CHUNK_SIZE_DIVIDER = 1 / constants.CHUNK_SIZE

constants.CHUNK_IMPASSABLE = 0
constants.CHUNK_NORTH_SOUTH = 1
constants.CHUNK_EAST_WEST = 2
constants.CHUNK_ALL_DIRECTIONS = 3
-- constants.CHUNK_PLAYER_BORDER = 4
-- constants.CHUNK_PLAYER_INTERIOR = 5

constants.BASE_SEARCH_RADIUS = 4 * constants.CHUNK_SIZE
constants.EVOLUTION_INCREMENTS = 0.05

constants.DIVISOR_DEATH_TRAIL_TABLE = { 0.75, 0.65, 0.55, 0.45, 0.35 }

-- ai

constants.MAX_TICKS_BEFORE_SORT_CHUNKS = 60 * 60 * 30 -- 1 tick = 1/60 sec * 60 = 1 second

constants.RESOURCE_MINIMUM_FORMATION_DELTA = 15

constants.AI_POINT_GENERATOR_AMOUNT = 6
constants.AI_SQUAD_COST = 175
constants.RECOVER_NEST_COST = constants.AI_SQUAD_COST
constants.RECOVER_WORM_COST = constants.AI_SQUAD_COST * 0.5
constants.AI_VENGENCE_SQUAD_COST = 45
constants.AI_SETTLER_COST = 200
constants.AI_BASE_BUILDING_COST = 500
constants.AI_TUNNEL_COST = 100
constants.AI_MAX_POINTS = 12500
constants.AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_POINTS * 3

constants.RAIDING_MINIMUM_BASE_THRESHOLD = 550

constants.AI_UNIT_REFUND = 3

constants.AI_MAX_SQUAD_COUNT = 35
constants.AI_MAX_BITER_GROUP_SIZE = 450

constants.AI_SQUAD_MERGE_THRESHOLD = constants.AI_MAX_BITER_GROUP_SIZE * 0.75

constants.AI_STATE_PEACEFUL = 1
constants.AI_STATE_AGGRESSIVE = 2
constants.AI_STATE_NOCTURNAL = 3
constants.AI_STATE_RAIDING = 4
constants.AI_STATE_MIGRATING = 5
constants.AI_STATE_SIEGE = 6
constants.AI_STATE_ONSLAUGHT = 7

constants.BASE_AI_STATE_DORMANT = 0
constants.BASE_AI_STATE_ACTIVE = 1
-- constants.BASE_AI_STATE_WORMS = 2
-- constants.BASE_AI_STATE_NESTS = 3
constants.BASE_AI_STATE_OVERDRIVE = 2
constants.BASE_AI_STATE_MUTATE = 3


constants.AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION = 0.5
constants.AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION = 3

constants.AI_MIN_STATE_DURATION = 7
constants.AI_MAX_STATE_DURATION = 17
constants.AI_MIN_TEMPERAMENT_DURATION = 25
constants.AI_MAX_TEMPERAMENT_DURATION = 32

constants.BASE_AI_MIN_STATE_DURATION = 2
constants.BASE_AI_MAX_STATE_DURATION = 10
constants.BASE_AI_MIN_TEMPERAMENT_DURATION = 5
constants.BASE_AI_MAX_TEMPERAMENT_DURATION = 15


-- ai base

constants.BASE_CLEAN_DISTANCE = 13

constants.BASE_DEADZONE_TTL = constants.TICKS_A_MINUTE * 18

constants.BASE_COLLECTION_THRESHOLD = constants.TICKS_A_MINUTE * 2

constants.BASE_DISTANCE_TO_EVO_INDEX = 1 / 9600

constants.BASE_SPAWNER_UPGRADE = 250
constants.BASE_WORM_UPGRADE = 200
constants.BASE_UPGRADE = 1500

constants.BASE_DISTANCE_THRESHOLD = 30 * constants.CHUNK_SIZE
constants.BASE_DISTANCE_LEVEL_BONUS = 15

constants.BASE_PROCESS_INTERVAL = constants.TICKS_A_SECOND * 2

-- ai retreat

constants.NO_RETREAT_BASE_PERCENT = 0.10
constants.NO_RETREAT_EVOLUTION_BONUS_MAX = 0.25
constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX = 0.40

-- pheromone amounts

constants.MOVEMENT_PENALTY_AMOUNT = 300000
constants.DEATH_PHEROMONE_GENERATOR_AMOUNT = 1300
constants.DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT * 2
constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT = 300

constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT = 0

-- pheromone diffusion amounts

constants.MOVEMENT_GENERATOR_PERSISTANCE = 0.90
constants.MOVEMENT_PHEROMONE_PERSISTANCE = 0.975
constants.BASE_PHEROMONE_PERSISTANCE = 0.99
constants.PLAYER_PHEROMONE_PERSISTANCE = 0.97
constants.RESOURCE_PHEROMONE_PERSISTANCE = 0.97

-- chunk attributes

constants.MOVEMENT_PHEROMONE = 1
constants.BASE_PHEROMONE = 2
constants.PLAYER_PHEROMONE = 3
constants.RESOURCE_PHEROMONE = 4

-- constants.PASSABLE = 5

constants.CHUNK_TICK = 5

-- constants.PATH_RATING = 7

-- Squad status

constants.SQUAD_RETREATING = 1 -- used during squad retreat
constants.SQUAD_GUARDING = 2 -- used when squad is idle
constants.SQUAD_BURROWING = 3
constants.SQUAD_RAIDING = 4 -- used when player stuff is close
constants.SQUAD_SETTLING = 5
constants.SQUAD_BUILDING = 6

-- Squad Related

constants.ATTACK_SCORE = 1
constants.ATTACK_SCORE_KAMIKAZE = 2

constants.RETREAT_GRAB_RADIUS = 24
constants.RETREAT_SPAWNER_GRAB_RADIUS = 75

constants.BASE_RALLY_CHANCE = 0.02
constants.BONUS_RALLY_CHANCE = 0.06

constants.RALLY_CRY_DISTANCE = 96
constants.SETTLER_DISTANCE = 224

constants.GROUP_MERGE_DISTANCE = 28

constants.MAX_PENALTY_BEFORE_PURGE = 36000

-- player building pheromones

constants.GENERATOR_PHEROMONE_LEVEL_1 = 25
constants.GENERATOR_PHEROMONE_LEVEL_2 = 100
constants.GENERATOR_PHEROMONE_LEVEL_3 = 500
constants.GENERATOR_PHEROMONE_LEVEL_4 = 1000
constants.GENERATOR_PHEROMONE_LEVEL_5 = 1750
constants.GENERATOR_PHEROMONE_LEVEL_6 = 6000

constants.BUILDING_PHEROMONES = {}
constants.BUILDING_PHEROMONES["wall"] = constants.GENERATOR_PHEROMONE_LEVEL_1
constants.BUILDING_PHEROMONES["transport-belt"] = constants.GENERATOR_PHEROMONE_LEVEL_1

constants.BUILDING_PHEROMONES["splitter"] = constants.GENERATOR_PHEROMONE_LEVEL_2
constants.BUILDING_PHEROMONES["pump"] = constants.GENERATOR_PHEROMONE_LEVEL_2
constants.BUILDING_PHEROMONES["offshore-pump"] = constants.GENERATOR_PHEROMONE_LEVEL_2

constants.BUILDING_PHEROMONES["lamp"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["solar-panel"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["programmable-speaker"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["accumulator"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["assembling-machine"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["turret"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["ammo-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_3

constants.BUILDING_PHEROMONES["furnace"] = constants.GENERATOR_PHEROMONE_LEVEL_4
constants.BUILDING_PHEROMONES["lab"] = constants.GENERATOR_PHEROMONE_LEVEL_4
constants.BUILDING_PHEROMONES["roboport"] = constants.GENERATOR_PHEROMONE_LEVEL_4
constants.BUILDING_PHEROMONES["beacon"] = constants.GENERATOR_PHEROMONE_LEVEL_4
constants.BUILDING_PHEROMONES["radar"] = constants.GENERATOR_PHEROMONE_LEVEL_4
constants.BUILDING_PHEROMONES["electric-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_4

constants.BUILDING_PHEROMONES["boiler"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["generator"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["fluid-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["mining-drill"] = constants.GENERATOR_PHEROMONE_LEVEL_5

constants.BUILDING_PHEROMONES["artillery-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_6
constants.BUILDING_PHEROMONES["reactor"] = constants.GENERATOR_PHEROMONE_LEVEL_6
constants.BUILDING_PHEROMONES["rocket-silo"] = constants.GENERATOR_PHEROMONE_LEVEL_6

constants.VICTORY_SCENT = {}
constants.VICTORY_SCENT["wall"] = constants.BUILDING_PHEROMONES["wall"] * 3
constants.VICTORY_SCENT["transport-belt"] = constants.BUILDING_PHEROMONES["transport-belt"] * 3

constants.VICTORY_SCENT["splitter"] = constants.BUILDING_PHEROMONES["splitter"] * 3
constants.VICTORY_SCENT["pump"] = constants.BUILDING_PHEROMONES["pump"] * 3
constants.VICTORY_SCENT["offshore-pump"] = constants.BUILDING_PHEROMONES["offshore-pump"] * 3

constants.VICTORY_SCENT["lamp"] = constants.BUILDING_PHEROMONES["lamp"] * 3
constants.VICTORY_SCENT["solar-panel"] = constants.BUILDING_PHEROMONES["solar-panel"] * 3
constants.VICTORY_SCENT["programmable-speaker"] = constants.BUILDING_PHEROMONES["programmable-speaker"] * 3
constants.VICTORY_SCENT["accumulator"] = constants.BUILDING_PHEROMONES["accumulator"] * 3
constants.VICTORY_SCENT["assembling-machine"] = constants.BUILDING_PHEROMONES["assembling-machine"] * 3
constants.VICTORY_SCENT["turret"] = constants.BUILDING_PHEROMONES["turret"] * 3
constants.VICTORY_SCENT["ammo-turret"] = constants.BUILDING_PHEROMONES["ammo-turret"] * 3

constants.VICTORY_SCENT["furnace"] = constants.BUILDING_PHEROMONES["furnace"] * 3
constants.VICTORY_SCENT["lab"] = constants.BUILDING_PHEROMONES["lab"] * 3
constants.VICTORY_SCENT["roboport"] = constants.BUILDING_PHEROMONES["roboport"] * 3
constants.VICTORY_SCENT["beacon"] = constants.BUILDING_PHEROMONES["beacon"] * 3
constants.VICTORY_SCENT["radar"] = constants.BUILDING_PHEROMONES["radar"] * 3
constants.VICTORY_SCENT["electric-turret"] = constants.BUILDING_PHEROMONES["electric-turret"] * 3

constants.VICTORY_SCENT["boiler"] = constants.BUILDING_PHEROMONES["boiler"] * 3
constants.VICTORY_SCENT["generator"] = constants.BUILDING_PHEROMONES["generator"] * 3
constants.VICTORY_SCENT["fluid-turret"] = constants.BUILDING_PHEROMONES["fluid-turret"] * 3
constants.VICTORY_SCENT["mining-drill"] = constants.BUILDING_PHEROMONES["mining-drill"] * 3

constants.VICTORY_SCENT["artillery-turret"] = constants.BUILDING_PHEROMONES["artillery-turret"] * 3
constants.VICTORY_SCENT["reactor"] = constants.BUILDING_PHEROMONES["reactor"] * 3
constants.VICTORY_SCENT["rocket-silo"] = constants.BUILDING_PHEROMONES["rocket-silo"] * 3

-- map settings tweaks

constants.PATH_FINDER_SHORT_REQUEST_RATIO = 0.8
constants.PATH_FINDER_SHORT_CACHE_SIZE = 25
constants.PATH_FINDER_LONG_REQUEST_RATIO = 5
constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH = 1000

constants.MAX_FAILED_BEHAVIORS = 1000

constants.UNIT_GROUP_DISOWN_DISTANCE = 100
constants.UNIT_GROUP_TICK_TOLERANCE = 3600000

constants.UNIT_GROUP_MAX_RADIUS = 15
constants.UNIT_GROUP_MAX_SPEED_UP = 2
constants.UNIT_GROUP_MAX_SLOWDOWN = 1.0
constants.UNIT_GROUP_SLOWDOWN_FACTOR = 1.0

-- sentinels

constants.SENTINEL_IMPASSABLE_CHUNK = {}

constants.SENTINEL_IMPASSABLE_CHUNK.name = "ImpassableChunk"
constants.SENTINEL_IMPASSABLE_CHUNK[constants.MOVEMENT_PHEROMONE] = constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT
constants.SENTINEL_IMPASSABLE_CHUNK[constants.BASE_PHEROMONE] = constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT
constants.SENTINEL_IMPASSABLE_CHUNK[constants.PLAYER_PHEROMONE] = constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT
constants.SENTINEL_IMPASSABLE_CHUNK[constants.RESOURCE_PHEROMONE] = constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT
constants.SENTINEL_IMPASSABLE_CHUNK[constants.CHUNK_TICK] = 0
constants.SENTINEL_IMPASSABLE_CHUNK.x = -1
constants.SENTINEL_IMPASSABLE_CHUNK.y = -1

constants.ENERGY_THIEF_CONVERSION_TABLE = {
    ["generator"] = "unit",
    ["pump"] = "smallUnit",
    ["inserter"] = "smallUnit",
    ["reactor"] = "bigUnit",
    ["accumulator"] = "unit",
    ["solar-panel"] = "unit",
    ["assembling-machine"] = "unit",
    ["roboport"] = "bigUnit",
    ["beacon"] = "bigUnit",
    ["programmable-speaker"] = "unit",
    ["mining-drill"] = "unit",
    ["rocket-silo"] = "bigUnit",
    ["lamp"] = "smallUnit",
    ["radar"] = "bigUnit",
    ["lab"] = "unit",
    ["electric-turret"] = "unit",
    ["electric-pole"] = "pole"
}

constants.ENERGY_THIEF_DRAIN_CRYSTALS = {
    "crystal-v1-drain-rampant",
    "crystal-v2-drain-rampant",
    "crystal-v3-drain-rampant",
    "crystal-v4-drain-rampant",
    "crystal-v5-drain-rampant",
    "crystal-v6-drain-rampant",
    "crystal-v7-drain-rampant",
    "crystal-v8-drain-rampant",
    "crystal-v9-drain-rampant",
    "crystal-v10-drain-rampant"
}

constants.NEIGHBOR_DIVIDER = {1/1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8}

-- unit spawners

local function roundToNearest(number, multiple)
    local num = number + (multiple * 0.5)
    return num - (num % multiple)
end

local tiers10 = {}

local tierStart = settings.startup["rampant-tierStart"].value
local tierEnd = settings.startup["rampant-tierEnd"].value

local function buildTier(size, tiers)
    local step = (tierEnd - tierStart) / (size - 1)
    local i = tierStart
    for _=1,size do
        tiers[#tiers+1] = roundToNearest(i, 1)
        i = i + step
    end
end

buildTier(10, tiers10)

constants.TIER_UPGRADE_SET_10 = tiers10

local variations = settings.startup["rampant-newEnemyVariations"].value

constants.ENERGY_THIEF_LOOKUP = {}

for tier=1, 10 do
    for i=1,variations do
        constants.ENERGY_THIEF_LOOKUP["energy-thief-worm-v" .. i .. "-t" .. tier .. "-rampant"] = true
    end
end

for tier=1, 10 do
    for i=1,variations do
        constants.ENERGY_THIEF_LOOKUP["energy-thief-biter-v" .. i .. "-t" .. tier .. "-rampant"] = true
    end
end

constants.FACTION_SET = {}

constants.FACTION_SET[#constants.FACTION_SET+1] = {
    type = "neutral",
    tint = {r=0.9, g=0.9, b=0.9, a=1},
    tint2 = {r=1, g=1, b=1, a=1},
    acceptRate = {1, 7, 0.3, 0.1},
    evo = 0,
    units = {
        {
            type = "biter",
            attackAttributes = {"melee"},
            name = "biter",
            majorResistances = {},
            minorResistances = {},
            attributes = {},
            drops = {"nilArtifact"}
        },
        {
            type = "spitter",
            attackAttributes = {"spit", "acid"},
            name = "spitter",
            majorResistances = {},
            minorResistances = {},
            attributes = {},
            drops = {"nilArtifact"}
        }
    },
    buildings = {
        {
            type = "spitter-spawner",
            name = "spitter-spawner",
            majorResistances = {},
            acceptRate = {1, 10, 0.1, 0.2},
            minorResistances = {},
            attributes = {},
            drops = {"nilArtifact"},
            buildSets = {
                {"spitter", 1, 10}
            }
        },
        {
            type = "biter-spawner",
            name = "biter-spawner",
            majorResistances = {},
            acceptRate = {1, 10, 0.1, 0.2},
            minorResistances = {},
            attributes = {},
            drops = {"nilArtifact"},
            buildSets = {
                {"biter", 1, 10}
            }
        },
        {
            type = "turret",
            name = "worm",
            majorResistances = {},
            acceptRate = {1, 10, 0.8, 0.6},
            minorResistances = {},
            attackAttributes = {"spit", "acid"},
            attributes = {},
            drops = {"nilArtifact"}
        },
        {
            type = "hive",
            name = "hive",
            majorResistances = {},
            minorResistances = {},
            attributes = {},
            acceptRate = {2, 10, 0.005, 0.03},
            drops = {"nilArtifact"},
            buildSets = {
                {"biter-spawner", 1, 10, 0.1, 0.2},
                {"spitter-spawner", 1, 10, 0.1, 0.2},
                {"turret", 1, 10, 0.8, 0.57},
                {"hive", 4, 10, 0.005, 0.03}
            }
        }
    }
}

if settings.startup["rampant-acidEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "acid",
        tint = {r=1, g=1, b=1, a=1},
        tint2 = {r=0, g=0.9, b=0, a=1},
        acceptRate = {1, 10, 0.1, 0.2},
        evo = 0,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                attributes = {},
                drops = {"greenArtifact"}
            },
            {
                type = "spitter",
                attackAttributes = {"spit", "acid"},
                name = "spitter",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                attributes = {},
                drops = {"greenArtifact"}
            }
        },
        buildings = {
            {
                type = "spitter-spawner",
                name = "spitter-spawner",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"greenArtifact"},
                buildSets = {
                    {"spitter", 1, 10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"greenArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                attackAttributes = {"spit", "acid"},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {},
                drops = {"greenArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                acceptRate = {2, 10, 0.005, 0.03},
                attributes = {},
                drops = {"greenArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.1, 0.2},
                    {"spitter-spawner", 1, 10, 0.1, 0.2},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 4, 10, 0.005, 0.03}
                }
            }
        }
    }
end

if settings.startup["rampant-laserEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "laser",
        tint = {r=0.3, g=0.3, b=0.42, a=1},
        tint2 = {r=0, g=0.6, b=0.8, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.10,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                majorResistances = {"laser", "electric"},
                attributes = {},
                drops = {"blueArtifact"}
            },
            {
                type = "spitter",
                attackAttributes = {"spit", "laser", "cluster"},
                name = "spitter",
                majorResistances = {"laser", "electric"},
                attributes = {},
                drops = {"blueArtifact"}
            }
        },
        buildings = {
            {
                type = "spitter-spawner",
                name = "spitter-spawner",
                majorResistances = {"laser", "electric"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {"spitter", 1, 10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"laser", "electric"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"laser", "electric"},
                attackAttributes = {"spit", "laser", "cluster"},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {},
                drops = {"blueArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"laser", "electric"},
                attributes = {},
                acceptRate = {2, 10, 0.005, 0.003},
                drops = {"blueArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.1, 0.2},
                    {"spitter-spawner", 1, 10, 0.1, 0.2},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 4, 10, 0.005, 0.03}
                }
            }
        }
    }
end

if settings.startup["rampant-fireEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "fire",
        tint = {r=1, g=1, b=1, a=1},
        tint2 = {r=0.9, g=0, b=0, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.12,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attributes = {},
                drops = {"redArtifact"}
            },
            {
                type = "spitter",
                attackAttributes = {"spit", "acid"},
                name = "spitter",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attributes = {},
                drops = {"redArtifact"}
            }
        },
        buildings = {
            {
                type = "spitter-spawner",
                name = "spitter-spawner",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"redArtifact"},
                buildSets = {
                    {"spitter", 1, 10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"fire", "acid"},
                acceptRate = {1, 10, 0.1, 0.2},
                minorResistances = {},
                attributes = {},
                drops = {"redArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attackAttributes = {"spit", "acid"},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {},
                drops = {"redArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attributes = {},
                acceptRate = {2, 10, 0.005, 0.03},
                drops = {"redArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.1, 0.2},
                    {"spitter-spawner", 1, 10, 0.1, 0.2},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 4, 10, 0.005, 0.03}
                }
            }
        }
    }
end

if settings.startup["rampant-infernoEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "inferno",
        tint = {r=0.7, g=0.45, b=0.5, a=1},
        tint2 = {r=0.9, g=0, b=0, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.2,
        units = {
            {
                type = "spitter",
                attackAttributes = {"stream", "acid"},
                name = "spitter",
                majorResistances = {"acid", "fire"},
                minorWeaknesses = {"poison"},
                attributes = {},
                drops = {"orangeArtifact"}
            }
        },
        buildings = {
            {
                type = "spitter-spawner",
                name = "spitter-spawner",
                majorResistances = {"acid", "fire"},
                minorWeaknesses = {"poison"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"orangeArtifact"},
                buildSets = {
                    {"spitter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"acid", "fire"},
                minorWeaknesses = {"poison"},
                acceptRate = {1, 10, 0.8, 0.6},
                attackAttributes = {"stream", "acid"},
                attributes = {},
                drops = {"orangeArtifact"}
            }
        }
    }
end

if settings.startup["rampant-waspEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "wasp",
        tint = {r=0.9, g=0.8, b=0.9, a=1},
        tint2 = {r=0.85, g=0.85, b=0, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.2,
        units = {
            {
                type = "drone",
                attackAttributes = {"spit", "acid"},
                name = "wasp",
                attributes = {"followsPlayer"},
                drops = {}
            },
            {
                type = "drone",
                attackAttributes = {"stream", "acid"},
                name = "worm-wasp",
                attributes = {"stationary"},
                drops = {}
            },
            {
                type = "spitter",
                attackAttributes = {"capsule", {"drone", "wasp"}},
                name = "spitter",
                attributes = {},
                drops = {"purpleArtifact"}
            }
        },
        buildings = {
            {
                type = "spitter-spawner",
                name = "spitter-spawner",
                attributes = {},
                acceptRate = {1, 10, 0.1, 0.2},
                drops = {"purpleArtifact"},
                buildSets = {
                    {"spitter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                attackAttributes = {"capsule", {"drone", "worm-wasp"}},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {},
                drops = {"purpleArtifact"}
            }
        }
    }
end

if settings.startup["rampant-spawnerEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "spawner",
        tint = {r=0.7, g=0, b=0.7, a=1},
        tint2 = {r=0.8, g=0, b=0.8, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.2,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "spawn",
                attributes = {"fragile", "unstable", "smallest"},
                drops = {}
            },
            {
                type = "drone",
                attackAttributes = {"touch", "acid"},
                name = "egg",
                attributes = {"stationary", {"clusterDeath", "spawn"}},
                drops = {}
            },
            {
                type = "drone",
                attackAttributes = {"touch", "acid"},
                name = "worm-egg",
                attributes = {"stationary", {"clusterDeath", "spawn"}},
                drops = {}
            },
            {
                type = "spitter",
                attackAttributes = {"capsule", {"drone", "egg"}},
                name = "spitter",
                attributes = {"selfDamaging"},
                drops = {"orangeArtifact"}
            }
        },
        buildings = {
            {
                type = "spitter-spawner",
                name = "spitter-spawner",
                attributes = {},
                drops = {"orangeArtifact"},
                acceptRate = {1, 10, 0.1, 0.2},
                buildSets = {
                    {"spitter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                attackAttributes = {"capsule", {"drone", "worm-egg"}},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {},
                drops = {"orangeArtifact"}
            }
        }
    }
end

if settings.startup["rampant-electricEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "electric",
        tint = {r=0.7, g=0.7, b=1.0, a=1.0},
        tint2 = {r=0, g=0, b=1, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.1,
        units = {
            {
                type = "biter",
                attackAttributes = {"beam", "electric"},
                name = "biter",
                majorResistances = {"electric"},
                minorResistances = {"laser"},
                attributes = {"lowHealth"},
                drops = {"blueArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"electric"},
                minorResistances = {"laser"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"electric"},
                minorResistances = {"laser"},
                acceptRate = {1, 10, 0.8, 0.6},
                attackAttributes = {"spit", "electric", "cluster"},
                attributes = {},
                drops = {"blueArtifact"}
            }
        }
    }
end

if settings.startup["rampant-physicalEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "physical",
        tint = {r=0.9, g=0.9, b=0.9, a=1},
        tint2 = {r=0, g=0, b=0, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.12,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                majorResistances = {"physical", "explosion"},
                minorWeaknesses = {"laser", "electric"},
                attributes = {"highHealth","big", "highRegen", "slowMovement"},
                drops = {"redArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"physical", "explosion"},
                minorWeaknesses = {"laser", "electric"},
                attributes = {"highHealth", "big", "highRegen"},
                acceptRate = {1, 10, 0.1, 0.2},
                drops = {"redArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"physical", "explosion"},
                minorWeaknesses = {"laser", "electric"},
                attackAttributes = {"spit", "physical"},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {"highHealth", "big", "highRegen"},
                drops = {"redArtifact"}
            }
        }
    }
end

if settings.startup["rampant-trollEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "troll",
        tint = {r=0.4, g=0.4, b=0.4, a=1},
        tint2 = {r=0.8, g=0.8, b=0.8, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.17,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                attributes = {"highestHealth", "bigger", "highestRegen", "slowMovement"},
                drops = {"greenArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {"highestHealth", "bigger", "highestRegen"},
                drops = {"greenArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                attackAttributes = {"spit", "physical"},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {"highestHealth", "bigger", "highestRegen"},
                drops = {"greenArtifact"}
            }
        }
    }
end

if settings.startup["rampant-poisonEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "poison",
        tint = {r=0.4, g=0.6, b=0.5, a=1},
        tint2 = {r=0, g=0.7, b=0, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.17,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                minorResistances = {"fire"},
                majorResistances = {"poison"},
                minorWeaknesses = {"electric", "explosion", "laser"},
                attributes = {"poisonDeathCloud"},
                drops = {"greenArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                minorResistances = {"fire"},
                majorResistances = {"poison"},
                minorWeaknesses = {"electric", "explosion", "laser"},
                attributes = {"poisonDeathCloud"},
                acceptRate = {1, 10, 0.1, 0.2},                
                drops = {"greenArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                minorResistances = {"fire"},
                majorResistances = {"poison"},
                minorWeaknesses = {"electric", "explosion", "laser"},
                acceptRate = {1, 10, 0.8, 0.6},
                attackAttributes = {"spit", "poison"},
                attributes = {"poisonDeathCloud"},
                drops = {"greenArtifact"}
            }
        }
    }
end

if settings.startup["rampant-suicideEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "suicide",
        tint = {r=1, g=1, b=1, a=1},
        tint2 = {r=0.85, g=0.85, b=0, a=1},
        acceptRate = {1, 10, 0.05, 0.15},
        evo = 0.12,
        units = {
            {
                type = "biter",
                attackAttributes = {"bomb"},
                name = "biter",
                majorWeaknesses = {"explosion"},
                minorResistances = {"poison"},
                attributes = {"lowestHealth", "quickSpawning", "quickMovement"},
                drops = {"yellowArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"explosion"},
                minorResistances = {"poison"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"yellowArtifact", "quickSpawning", "lowUnits"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"explosion"},
                minorResistances = {"poison"},
                attackAttributes = {"spit", "acid", "slow"},
                acceptRate = {1, 10, 0.8, 0.6},
                attributes = {},
                drops = {"yellowArtifact"}
            }
        }
    }
end

if settings.startup["rampant-nuclearEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "nuclear",
        tint = {r=0.6, g=0.6, b=0.4, a=1},
        tint2 = {r=0.95, g=0.95, b=0, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.2,
        units = {
            {
                type = "biter",
                attackAttributes = {"nuclear"},
                name = "biter",
                majorWeaknesses = {"explosion"},
                attributes = {"lowestHealth", "quickSpawning", "quickMovement"},
                drops = {"yellowArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"explosion"},
                minorResistances = {"fire"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"yellowArtifact", "quickSpawning", "lowUnits"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"explosion"},
                minorResistances = {"fire"},
                acceptRate = {1, 10, 0.8, 0.6},
                attackAttributes = {"spit", "acid", "slow"},
                attributes = {},
                drops = {"yellowArtifact"}
            }
        }
    }
end

if settings.startup["rampant-energyThiefEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "energy-thief",
        tint = {r=0.4, g=0.5, b=0.7, a=1},
        tint2 = {r=0, g=0, b=0.7, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.2,
        units = {
            {
                type = "biter",
                attackAttributes = {"beam", "electric", "drainCrystal"},
                name = "biter",
                majorResistances = {"electric", "laser"},
                minorResistances = {},
                attributes = {"lowHealth"},
                drops = {"blueArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"electric", "laser"},
                minorResistances = {},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"electric", "laser"},
                minorResistances = {},
                acceptRate = {1, 10, 0.8, 0.6},
                attackAttributes = {"spit", "electric", "cluster"},
                attributes = {},
                drops = {"blueArtifact"}
            }
        }
    }
end

if settings.startup["rampant-fastEnemy"].value then
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "fast",
        tint = {r=0.26, g=0.66, b=0.62, a=1},
        tint2 = {r=0, g=0.85, b=0.80, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.12,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                majorResistances = {},
                minorResistances = {"explosion"},
                attributes = {"quickCooldown", "quickMovement"},
                drops = {"purpleArtifact"}
            },
            {
                type = "spitter",
                attackAttributes = {"spit", "acid"},
                name = "spitter",
                majorResistances = {},
                minorResistances = {"explosion"},
                attributes = {"quickCooldown", "quickMovement"},
                drops = {"purpleArtifact"}
            }
        },
        buildings = {
            {
                type = "spitter-spawner",
                name = "spitter-spawner",
                majorResistances = {},
                minorResistances = {"explosion"},
                attributes = {"quickSpawning"},
                acceptRate = {1, 10, 0.1, 0.2},
                drops = {"purpleArtifact"},
                buildSets = {
                    {"spitter", 1, 10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {},
                minorResistances = {"explosion"},
                acceptRate = {1, 10, 0.1, 0.2},
                attributes = {"quickSpawning"},
                drops = {"purpleArtifact"},
                buildSets = {
                    {"biter", 1, 10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {},
                minorResistances = {"explosion"},
                acceptRate = {1, 10, 0.8, 0.6},
                attackAttributes = {"spit", "acid"},
                attributes = {"quickCooldown"},
                drops = {"purpleArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {},
                minorResistances = {"explosion"},
                attributes = {"quickSpawning"},
                acceptRate = {2, 10, 0.005, 0.03},
                drops = {"purpleArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.1, 0.2},
                    {"spitter-spawner", 1, 10, 0.1, 0.2},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 4, 10, 0.005, 0.03}
                }
            }
        }
    }
end

constants.HIVE_BUILDINGS_TYPES = {
    "trap",
    "turret",
    "utility",
    "spitter-spawner",
    "biter-spawner",
    "hive"
}

constants.HIVE_BUILDINGS_COST = {}
constants.HIVE_BUILDINGS_COST["trap"] = constants.BASE_WORM_UPGRADE * 0.5
constants.HIVE_BUILDINGS_COST["turret"] = constants.BASE_WORM_UPGRADE
constants.HIVE_BUILDINGS_COST["utility"] = constants.BASE_SPAWNER_UPGRADE * 1.5
constants.HIVE_BUILDINGS_COST["spitter-spawner"] = constants.BASE_SPAWNER_UPGRADE
constants.HIVE_BUILDINGS_COST["biter-spawner"] = constants.BASE_SPAWNER_UPGRADE
constants.HIVE_BUILDINGS_COST["hive"] = constants.BASE_SPAWNER_UPGRADE * 2

constantsG =  constants
return constants
