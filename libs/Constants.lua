-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


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
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN = (1 - 0.001)
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX = (1 - 0.800)

constants.TEMPERAMENT_RANGE_MAX = 10000
constants.TEMPERAMENT_RANGE_MIN = -constants.TEMPERAMENT_RANGE_MAX
constants.TEMPERAMENT_DIVIDER = 1 / (2 * constants.TEMPERAMENT_RANGE_MAX)

constants.PROCESS_QUEUE_SIZE = 105
constants.SCAN_QUEUE_SIZE = 2
constants.RESOURCE_QUEUE_SIZE = 2
constants.ENEMY_QUEUE_SIZE = 1
constants.PLAYER_QUEUE_SIZE = 2
constants.CLEANUP_QUEUE_SIZE = 8
constants.ATTACK_QUEUE_SIZE = 18
constants.BASE_QUEUE_SIZE = 1
constants.PROCESS_STATIC_QUEUE_SIZE = 20
constants.PROCESS_PLAYER_BOUND = 128
constants.VICTORY_SCENT_BOUND = 128

constants.TICKS_A_SECOND = 60
constants.TICKS_A_MINUTE = constants.TICKS_A_SECOND * 60

constants.CHUNK_PASS_THRESHOLD = 0.2

constants.COOLDOWN_RALLY = constants.TICKS_A_SECOND * 10
constants.COOLDOWN_RETREAT = constants.TICKS_A_SECOND * 10
constants.COOLDOWN_DRAIN = constants.TICKS_A_SECOND * 1

constants.RESOURCE_NORMALIZER = 1 / 1024

constants.PLAYER_PHEROMONE_MULTIPLER = 100

constants.DURATION_ACTIVE_NEST = 120 * constants.TICKS_A_SECOND

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

constants.RESOURCE_MINIMUM_FORMATION_DELTA = 15

constants.MINIMUM_AI_POINTS = 400
constants.AI_POINT_GENERATOR_AMOUNT = 15
constants.AI_SQUAD_COST = 175
constants.RECOVER_NEST_COST = constants.AI_SQUAD_COST
constants.RECOVER_WORM_COST = constants.AI_SQUAD_COST * 0.5
constants.AI_VENGENCE_SQUAD_COST = 45
constants.AI_SETTLER_COST = 300
constants.AI_BASE_BUILDING_COST = 500
constants.AI_TUNNEL_COST = 100
constants.AI_MAX_POINTS = 15500
constants.AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_POINTS * 3

constants.RAIDING_MINIMUM_BASE_THRESHOLD = 550

constants.AI_UNIT_REFUND = 3

constants.AI_MAX_BITER_GROUP_SIZE = 600

constants.AI_SQUAD_MERGE_THRESHOLD = constants.AI_MAX_BITER_GROUP_SIZE * 0.75

constants.AI_MAX_SQUADS_PER_CYCLE = 7

constants.BASE_AI_STATE_PEACEFUL = 1
constants.BASE_AI_STATE_AGGRESSIVE = 2
constants.BASE_AI_STATE_RAIDING = 4
constants.BASE_AI_STATE_MIGRATING = 5
constants.BASE_AI_STATE_SIEGE = 6
constants.BASE_AI_STATE_ONSLAUGHT = 7

constants.stateEnglish = {}
constants.stateEnglish[constants.BASE_AI_STATE_PEACEFUL] = "AI_STATE_PEACEFUL"
constants.stateEnglish[constants.BASE_AI_STATE_AGGRESSIVE] = "AI_STATE_AGGRESSIVE"
constants.stateEnglish[constants.BASE_AI_STATE_RAIDING] = "AI_STATE_RAIDING"
constants.stateEnglish[constants.BASE_AI_STATE_MIGRATING] = "AI_STATE_MIGRATING"
constants.stateEnglish[constants.BASE_AI_STATE_SIEGE] = "AI_STATE_SIEGE"
constants.stateEnglish[constants.BASE_AI_STATE_ONSLAUGHT] = "AI_STATE_ONSLAUGHT"

constants.BASE_GENERATION_STATE_DORMANT = 0
constants.BASE_GENERATION_STATE_ACTIVE = 1
-- constants.BASE_AI_STATE_OVERDRIVE = 2
-- constants.BASE_AI_STATE_MUTATE = 3

constants.ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS = 30
constants.ALL_NESTS_PER_SIEGE_GROUPS = 150
constants.AGGRESSIVE_CAN_ATTACK_WAIT_MIN_DURATION = 0.5
constants.AGGRESSIVE_CAN_ATTACK_WAIT_MAX_DURATION = 3

constants.BASE_AI_MIN_STATE_DURATION = 10
constants.BASE_AI_MAX_STATE_DURATION = 25

-- constants.AI_MIN_TEMPERAMENT_DURATION = 25
-- constants.AI_MAX_TEMPERAMENT_DURATION = 32

constants.BASE_GENERATION_MIN_STATE_DURATION = 12
constants.BASE_GENERATION_MAX_STATE_DURATION = 20

-- ai base

constants.BASE_CLEAN_DISTANCE = 13

constants.BASE_COLLECTION_THRESHOLD = constants.TICKS_A_MINUTE * 2

constants.BASE_DISTANCE_TO_EVO_INDEX = 1 / 9600

constants.BASE_SPAWNER_UPGRADE = 500
constants.BASE_WORM_UPGRADE = 400
constants.BASE_UPGRADE = 1500

constants.BASE_DISTANCE_THRESHOLD = 30 * constants.CHUNK_SIZE
constants.BASE_DISTANCE_LEVEL_BONUS = 7.5

constants.BASE_PROCESS_INTERVAL = constants.TICKS_A_SECOND * 20

-- ai retreat

constants.NO_RETREAT_BASE_PERCENT = 0.10
constants.NO_RETREAT_EVOLUTION_BONUS_MAX = 0.25

-- pheromone amounts

constants.DEATH_PHEROMONE_GENERATOR_AMOUNT = 0.001
constants.DOUBLE_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT * 2
constants.TEN_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT * 10
constants.FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT = constants.DEATH_PHEROMONE_GENERATOR_AMOUNT * 5
constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT = 300

constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT = 0

-- pheromone diffusion amounts

constants.MOVEMENT_GENERATOR_PERSISTANCE = 0.92

-- chunk attributes

constants.BASE_PHEROMONE = 1
constants.PLAYER_PHEROMONE = 2
constants.RESOURCE_PHEROMONE = 3

-- constants.PASSABLE = 5

constants.CHUNK_TICK = 4

-- constants.PATH_RATING = 7

-- Squad status

constants.SQUAD_RETREATING = 1 -- used during squad retreat
constants.SQUAD_GUARDING = 2 -- used when squad is idle
constants.SQUAD_BURROWING = 3
constants.SQUAD_RAIDING = 4 -- used when player stuff is close
constants.SQUAD_SETTLING = 5
constants.SQUAD_BUILDING = 6

-- Squad Related

constants.MINIMUM_EXPANSION_DISTANCE = 10

constants.RETREAT_GRAB_RADIUS = 24
constants.RETREAT_SPAWNER_GRAB_RADIUS = 75

constants.BASE_RALLY_CHANCE = 0.02
constants.BONUS_RALLY_CHANCE = 0.04

constants.RALLY_CRY_DISTANCE = 96
constants.SETTLER_DISTANCE = 224

constants.GROUP_MERGE_DISTANCE = 28

constants.MAX_PENALTY_BEFORE_PURGE = 36000

constants.COMMAND_TIMEOUT = 30 * 60

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

constants.BUILDING_PHEROMONES["splitter"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["pump"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["offshore-pump"] = constants.GENERATOR_PHEROMONE_LEVEL_3

constants.BUILDING_PHEROMONES["lamp"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["solar-panel"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["programmable-speaker"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["accumulator"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["assembling-machine"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["turret"] = constants.GENERATOR_PHEROMONE_LEVEL_3
constants.BUILDING_PHEROMONES["ammo-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_3

constants.BUILDING_PHEROMONES["furnace"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["lab"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["roboport"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["beacon"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["radar"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["electric-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_5

constants.BUILDING_PHEROMONES["boiler"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["generator"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["fluid-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_5
constants.BUILDING_PHEROMONES["mining-drill"] = constants.GENERATOR_PHEROMONE_LEVEL_5

constants.BUILDING_PHEROMONES["artillery-turret"] = constants.GENERATOR_PHEROMONE_LEVEL_6
constants.BUILDING_PHEROMONES["reactor"] = constants.GENERATOR_PHEROMONE_LEVEL_6
constants.BUILDING_PHEROMONES["rocket-silo"] = constants.GENERATOR_PHEROMONE_LEVEL_6

constants.VICTORY_SCENT = {}
constants.VICTORY_SCENT["wall"] = constants.BUILDING_PHEROMONES["wall"] * 10
constants.VICTORY_SCENT["transport-belt"] = constants.BUILDING_PHEROMONES["transport-belt"] * 10

constants.VICTORY_SCENT["splitter"] = constants.BUILDING_PHEROMONES["splitter"] * 10
constants.VICTORY_SCENT["pump"] = constants.BUILDING_PHEROMONES["pump"] * 10
constants.VICTORY_SCENT["offshore-pump"] = constants.BUILDING_PHEROMONES["offshore-pump"] * 10

constants.VICTORY_SCENT["lamp"] = constants.BUILDING_PHEROMONES["lamp"] * 10
constants.VICTORY_SCENT["solar-panel"] = constants.BUILDING_PHEROMONES["solar-panel"] * 10
constants.VICTORY_SCENT["programmable-speaker"] = constants.BUILDING_PHEROMONES["programmable-speaker"] * 10
constants.VICTORY_SCENT["accumulator"] = constants.BUILDING_PHEROMONES["accumulator"] * 10
constants.VICTORY_SCENT["assembling-machine"] = constants.BUILDING_PHEROMONES["assembling-machine"] * 10
constants.VICTORY_SCENT["turret"] = constants.BUILDING_PHEROMONES["turret"] * 10
constants.VICTORY_SCENT["ammo-turret"] = constants.BUILDING_PHEROMONES["ammo-turret"] * 10

constants.VICTORY_SCENT["furnace"] = constants.BUILDING_PHEROMONES["furnace"] * 10
constants.VICTORY_SCENT["lab"] = constants.BUILDING_PHEROMONES["lab"] * 10
constants.VICTORY_SCENT["roboport"] = constants.BUILDING_PHEROMONES["roboport"] * 10
constants.VICTORY_SCENT["beacon"] = constants.BUILDING_PHEROMONES["beacon"] * 10
constants.VICTORY_SCENT["radar"] = constants.BUILDING_PHEROMONES["radar"] * 10
constants.VICTORY_SCENT["electric-turret"] = constants.BUILDING_PHEROMONES["electric-turret"] * 10

constants.VICTORY_SCENT["boiler"] = constants.BUILDING_PHEROMONES["boiler"] * 10
constants.VICTORY_SCENT["generator"] = constants.BUILDING_PHEROMONES["generator"] * 10
constants.VICTORY_SCENT["fluid-turret"] = constants.BUILDING_PHEROMONES["fluid-turret"] * 10
constants.VICTORY_SCENT["mining-drill"] = constants.BUILDING_PHEROMONES["mining-drill"] * 10

constants.VICTORY_SCENT["artillery-turret"] = constants.BUILDING_PHEROMONES["artillery-turret"] * 10
constants.VICTORY_SCENT["reactor"] = constants.BUILDING_PHEROMONES["reactor"] * 10
constants.VICTORY_SCENT["rocket-silo"] = constants.BUILDING_PHEROMONES["rocket-silo"] * 10

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

constants.BASE_ALIGNMENT_NEUTRAL = 1
constants.BASE_ALIGNMENT_FIRE = 2
constants.BASE_ALIGNMENT_NUCLEAR = 3
constants.BASE_ALIGNMENT_SUICIDE = 4
constants.BASE_ALIGNMENT_INFEST = 5
constants.BASE_ALIGNMENT_ACID = 6
constants.BASE_ALIGNMENT_FIRE = 7
constants.BASE_ALIGNMENT_PHYSICAL = 8
constants.BASE_ALIGNMENT_LASER = 9
constants.BASE_ALIGNMENT_INFERNO = 10
constants.BASE_ALIGNMENT_POISON = 11
constants.BASE_ALIGNMENT_TROLL = 12
constants.BASE_ALIGNMENT_FAST = 13
constants.BASE_ALIGNMENT_WEB = 14
constants.BASE_ALIGNMENT_DECAYING = 15
constants.BASE_ALIGNMENT_UNDYING = 16
constants.BASE_ALIGNMENT_ENERGY_THIEF = 17
constants.BASE_ALIGNMENT_ELECTRIC = 18
constants.BASE_ALIGNMENT_WASP = 19
constants.BASE_ALIGNMENT_SPAWNER = 20

-- sentinels

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

local tierStart = settings.startup["rampant--tierStart"].value
local tierEnd = settings.startup["rampant--tierEnd"].value

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

local variations = settings.startup["rampant--newEnemyVariations"].value

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

constants.FACTIONS_BY_DAMAGE_TYPE = {
    ["physical"] = {
        -- "physical",
        -- "troll",
        -- "acid",
        -- "fast",
        -- "spawner"
    },
    ["impact"] = {
        -- "nuclear",
        -- "suicide",
        -- "spawner",
        -- "physical",
        -- "troll"
    },
    ["poison"] = {
        -- "poison",
        -- "suicide",
        -- "nuclear",
        -- "acid"
    },
    ["explosion"] = {
        -- "fast",
        -- "troll",
        -- "physical",
        -- "acid"
    },
    ["fire"] = {
        -- "fire",
        -- "inferno",
        -- "poison",
        -- "fast"
    },
    ["laser"] = {
        -- "laser",
        -- "energy-thief",
        -- "electric",
        -- "wasp",
        -- "spawner"
    },
    ["acid"] = {
        -- "acid",
        -- "inferno",
        -- "fire",
        -- "poison"
    },
    ["electric"] = {
        -- "laser",
        -- "energy-thief",
        -- "electric",
        -- "wasp",
        -- "spawner"
    }
}

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
            acceptRate = {1, 10, 0.3, 0.5},
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
            acceptRate = {1, 10, 0.3, 0.5},
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
            attributes = {"spawnDuringDays"},
            acceptRate = {2, 10, 0.001, 0.0175},
            drops = {"nilArtifact"},
            buildSets = {
                {"biter-spawner", 1, 10, 0.15, 0.3},
                {"spitter-spawner", 1, 10, 0.15, 0.3},
                {"turret", 1, 10, 0.8, 0.57},
                {"hive", 2, 10, 0.002, 0.02}
            }
        }
    }
}

if settings.startup["rampant--acidEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "acid"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "acid"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "acid"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "acid"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "acid",
        tint = {r=1, g=1, b=1, a=1},
        tint2 = {r=0.4, g=0.9, b=0.4, a=1},
        acceptRate = {1, 10, 0.1, 0.2},
        evo = 0,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee", "acidPool"},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                acceptRate = {2, 10, 0.001, 0.0175},
                attributes = {"spawnDuringDays"},
                drops = {"greenArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"spitter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--laserEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "laser"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "laser"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "laser",
        tint = {r=0.3, g=0.3, b=0.42, a=1},
        tint2 = {r=0, g=1, b=1, a=1},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"blueArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"spitter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--fireEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "fire"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "fire"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "fire",
        tint = {r=1, g=1, b=1, a=1},
        tint2 = {r=0.9, g=0.2, b=0.2, a=1},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"redArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"spitter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--infernoEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "inferno"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "inferno"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "inferno",
        tint = {r=0.5, g=0.1, b=0.1, a=1},
        tint2 = {r=0.9, g=0.1, b=0.1, a=1},
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
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"orangeArtifact"},
                buildSets = {
                    {"spitter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--waspEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "wasp"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "wasp"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "wasp",
        tint = {r=1, g=1, b=0, a=1},
        tint2 = {r=0, g=0, b=0, a=1},
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
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"purpleArtifact"},
                buildSets = {
                    {"spitter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--spawnerEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "spawner"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "spawner"
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "spawner"
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "spawner"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "spawner",
        tint = {r=0.7, g=0.1, b=0.7, a=1},
        tint2 = {r=1, g=0.4, b=1, a=1},
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
                attributes = {"stationary", "bigger", {"clusterDeath", "spawn"}},
                drops = {}
            },
            {
                type = "drone",
                attackAttributes = {"touch", "acid"},
                name = "worm-egg",
                attributes = {"stationary", "bigger", {"clusterDeath", "spawn"}},
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
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"orangeArtifact"},
                buildSets = {
                    {"spitter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--electricEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "electric"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "electric"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "electric",
        tint = {r=0.7, g=0.7, b=1.0, a=1},
        tint2 = {r=0.2, g=0.2, b=1, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.1,
        units = {
            {
                type = "biter",
                attackAttributes = {"scatterBeam", "electric"},
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
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"electric"},
                minorResistances = {"laser"},
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"blueArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--physicalEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "physical"
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "physical"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "physical"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "physical",
        tint = {r=0.9, g=0.9, b=0.9, a=1},
        tint2 = {r=0.8, g=0.8, b=0.8, a=1},
        acceptRate = {2, 10, 0.1, 0.15},
        evo = 0.12,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                majorResistances = {"physical", "explosion"},
                minorWeaknesses = {"laser", "electric"},
                attributes = {"highHealth", "longReach", "big", "slowMovement", "altBiterArmored"},
                drops = {"redArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"physical", "explosion"},
                minorWeaknesses = {"laser", "electric"},
                attributes = {"highHealth", "bigger"},
                acceptRate = {1, 10, 0.4, 0.6},
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
                attributes = {"highHealth", "bigger"},
                drops = {"redArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"physical", "explosion"},
                minorResistances = {"laser", "electric"},
                attributes = {"highHealth", "bigger", "spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"redArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--trollEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "troll"
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "troll"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "troll"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "troll",
        tint = {r=0.4, g=0.4, b=0.4, a=1},
        tint2 = {r=1, g=0.2, b=0.2, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.17,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "biter",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                attributes = {"highestHealth", "longReach", "bigger",
                              "highestRegen", "slowMovement", "altBiterArmored"},
                drops = {"greenArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                attributes = {"highestHealth", "bigger", "highRegen","spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"greenArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--poisonEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "poison"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "poison"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "poison"
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
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"poison"},
                minorResistances = {"fire"},
                minorWeaknesses = {"electric", "explosion", "laser"},
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"greenArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--suicideEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "suicide"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "suicide"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "suicide",
        tint = {r=0.8, g=0.8, b=0.8, a=1},
        tint2 = {r=1, g=0.5, b=0, a=1},
        acceptRate = {2, 10, 0.05, 0.15},
        evo = 0.35,
        units = {
            {
                type = "biter",
                attackAttributes = {"bomb"},
                name = "biter",
                majorWeaknesses = {"explosion"},
                minorResistances = {"poison"},
                attributes = {"lowestHealth", "quickSpawning", "quickMovement", "killsSelf"},
                drops = {"yellowArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"explosion"},
                minorResistances = {"poison"},
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"explosion"},
                minorResistances = {"poison"},
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"yellowArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--nuclearEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "nuclear"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "nuclear"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "nuclear",
        tint = {r=0.1, g=0.95, b=0.1, a=1},
        tint2 = {r=1, g=0.5, b=0, a=1},
        acceptRate = {4, 10, 0.1, 0.125},
        evo = 0.45,
        units = {
            {
                type = "biter",
                attackAttributes = {"nuclear"},
                name = "biter",
                majorWeaknesses = {"explosion"},
                attributes = {"lowestHealth", "quickSpawning", "quickMovement", "killsSelf"},
                drops = {"yellowArtifact"}
            }
        },
        buildings = {
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"explosion"},
                minorResistances = {"fire"},
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"explosion"},
                minorResistances = {"fire"},
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"yellowArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--energyThiefEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "energy-thief"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "energy-thief"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "energy-thief",
        tint = {r=0.2, g=0.2, b=0.4, a=1},
        tint2 = {r=0.1, g=0.1, b=0.1, a=1},
        acceptRate = {3, 10, 0.1, 0.125},
        evo = 0.2,
        units = {
            {
                type = "biter",
                attackAttributes = {"spit", "electric", "drainCrystal"},
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
                acceptRate = {1, 10, 0.4, 0.6},
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
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"electric", "laser"},
                attributes = {"spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"blueArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
                }
            }
        }
    }
end

if settings.startup["rampant--fastEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "fast"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "fast"
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "fast"
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "fast",
        tint = {r=0.9, g=0.9, b=0.9, a=1},
        tint2 = {r=1, g=1, b=0.1, a=1},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                acceptRate = {1, 10, 0.3, 0.5},
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
                attributes = {"quickSpawning", "spawnDuringDays"},
                acceptRate = {2, 10, 0.001, 0.0175},
                drops = {"purpleArtifact"},
                buildSets = {
                    {"biter-spawner", 1, 10, 0.15, 0.3},
                    {"spitter-spawner", 1, 10, 0.15, 0.3},
                    {"turret", 1, 10, 0.8, 0.57},
                    {"hive", 2, 10, 0.002, 0.02}
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

constants.VICTORY_SCENT_MULTIPLER = {}
for x=1,9 do
    for y=1,9 do
        local adjV
        local v
        if x <= 5 and y <= 5 then
            v = math.min(x, y)
        elseif x > 5 and y < 5 then
            v = math.min((10-x), y)
        elseif x < 5 and y > 5 then
            v = math.min(x, (10-y))
        else
            v = math.min((10-x), (10-y))
        end
        if v < 5 then
            adjV = v / 5
        else
            adjV = 1
        end
        constants.VICTORY_SCENT_MULTIPLER[#constants.VICTORY_SCENT_MULTIPLER+1] = adjV
    end
end

constants.HIVE_BUILDINGS_COST = {}
constants.HIVE_BUILDINGS_COST["trap"] = constants.BASE_WORM_UPGRADE
constants.HIVE_BUILDINGS_COST["turret"] = constants.BASE_WORM_UPGRADE
constants.HIVE_BUILDINGS_COST["utility"] = constants.BASE_SPAWNER_UPGRADE * 1.5
constants.HIVE_BUILDINGS_COST["spitter-spawner"] = constants.BASE_SPAWNER_UPGRADE
constants.HIVE_BUILDINGS_COST["biter-spawner"] = constants.BASE_SPAWNER_UPGRADE
constants.HIVE_BUILDINGS_COST["hive"] = constants.BASE_SPAWNER_UPGRADE * 2

constants.UNIT_DEATH_POINT_COST = 0.5

constants.MINIMUM_BUILDING_COST = constants.MAGIC_MAXIMUM_NUMBER
for _,cost in pairs(constants.HIVE_BUILDINGS_COST) do
    if cost < constants.MINIMUM_BUILDING_COST then
        constants.MINIMUM_BUILDING_COST = cost
    end
end

constants.FACTION_MUTATION_MAPPING = {}
constants.FACTION_MUTATION_MAPPING["spitter-spawner"] = {"biter-spawner", "hive"}
constants.FACTION_MUTATION_MAPPING["biter-spawner"] = {"spitter-spawner", "hive"}
constants.FACTION_MUTATION_MAPPING["hive"] = {"utility", "biter-spawner", "spitter-spawner"}
constants.FACTION_MUTATION_MAPPING["turret"] = {"trap"}
constants.FACTION_MUTATION_MAPPING["trap"] = {"turret"}
constants.FACTION_MUTATION_MAPPING["utility"] = {"hive", "biter-spawner", "spitter-spawner"}

function constants.gpsDebug(x, y, msg)
    game.print("[gps=".. x .. "," .. y .. "]" .. msg)
end

constantsG =  constants
return constants
