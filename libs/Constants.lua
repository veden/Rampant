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

local mathUtils = require("MathUtils")

-- misc

constants.ENEMY_SEED = settings.startup["rampant--enemySeed"].value
constants.ENEMY_VARIATIONS = settings.startup["rampant--newEnemyVariations"].value

constants.MAGIC_MAXIMUM_NUMBER = 1e99 -- used in loops trying to find the lowest/highest score
constants.MAGIC_MAXIMUM_BASE_NUMBER = 100000000
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN = (1 - 0.001)
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX = (1 - 0.800)

constants.TEMPERAMENT_RANGE_MAX = 10000
constants.TEMPERAMENT_RANGE_MIN = -constants.TEMPERAMENT_RANGE_MAX
constants.TEMPERAMENT_DIVIDER = 1 / (2 * constants.TEMPERAMENT_RANGE_MAX)

constants.PROCESS_QUEUE_SIZE = 40
constants.SCAN_QUEUE_SIZE = 2
constants.RESOURCE_QUEUE_SIZE = 2
constants.ENEMY_QUEUE_SIZE = 1
constants.PLAYER_QUEUE_SIZE = 2
constants.CLEANUP_QUEUE_SIZE = 8
constants.ATTACK_QUEUE_SIZE = 18
constants.BASE_QUEUE_SIZE = 1
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
constants.ENEMY_PHEROMONE_MULTIPLER = 500

constants.DURATION_ACTIVE_NEST = 120 * constants.TICKS_A_SECOND

constants.SETTLE_CLOUD_WARMUP = constants.TICKS_A_SECOND * 3

-- chunk properties

constants.CHUNK_SIZE = 32
constants.CHUNK_AND_HALF_SIZE = constants.CHUNK_SIZE * 1.5
constants.DOUBLE_CHUNK_SIZE = constants.CHUNK_SIZE * 2
constants.TRIPLE_CHUNK_SIZE = constants.CHUNK_SIZE * 3
constants.HALF_CHUNK_SIZE = constants.CHUNK_SIZE / 2
constants.QUARTER_CHUNK_SIZE = constants.HALF_CHUNK_SIZE / 2
constants.EIGHTH_CHUNK_SIZE = constants.QUARTER_CHUNK_SIZE / 2

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
constants.AI_VENGENCE_SETTLER_COST = 120
constants.AI_SETTLER_COST = 300
constants.AI_BASE_BUILDING_COST = 500
constants.AI_TUNNEL_COST = 100
constants.AI_MAX_POINTS = 15500
constants.AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_POINTS * 3

constants.RAIDING_MINIMUM_BASE_THRESHOLD = 50

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

constants.STATE_ENGLISH = {}
constants.STATE_ENGLISH[constants.BASE_AI_STATE_PEACEFUL] = "AI_STATE_PEACEFUL"
constants.STATE_ENGLISH[constants.BASE_AI_STATE_AGGRESSIVE] = "AI_STATE_AGGRESSIVE"
constants.STATE_ENGLISH[constants.BASE_AI_STATE_RAIDING] = "AI_STATE_RAIDING"
constants.STATE_ENGLISH[constants.BASE_AI_STATE_MIGRATING] = "AI_STATE_MIGRATING"
constants.STATE_ENGLISH[constants.BASE_AI_STATE_SIEGE] = "AI_STATE_SIEGE"
constants.STATE_ENGLISH[constants.BASE_AI_STATE_ONSLAUGHT] = "AI_STATE_ONSLAUGHT"

constants.BASE_GENERATION_STATE_DORMANT = 0
constants.BASE_GENERATION_STATE_ACTIVE = 1
-- constants.BASE_AI_STATE_OVERDRIVE = 2
-- constants.BASE_AI_STATE_MUTATE = 3

constants.ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS = 30
constants.ALL_NESTS_PER_EXPANSION_GROUPS = 700
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

constants.BASE_DISTANCE_TO_EVO_INDEX = 1 / (settings.startup["rampant--max-evo-dist"].value * constants.CHUNK_SIZE)

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
constants.PLAYER_PHEROMONE_GENERATOR_THRESHOLD = constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT * 0.85

constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT = 0

-- pheromone diffusion amounts

constants.MOVEMENT_GENERATOR_PERSISTANCE = 0.92
constants.PLAYER_GENERATOR_PERSISTANCE = 0.92

-- chunk attributes

constants.CHUNK_TICK = 1
constants.BASE_PHEROMONE = 2
constants.PLAYER_PHEROMONE = 3
constants.RESOURCE_PHEROMONE = 4
constants.ENEMY_PHEROMONE = 5
constants.KAMIKAZE_PHEROMONE = 6

-- constants.PATH_RATING = 7

-- Squad status

constants.SQUAD_RETREATING = 1 -- used during squad retreat
constants.SQUAD_GUARDING = 2 -- used when squad is idle
constants.SQUAD_BURROWING = 3
constants.SQUAD_RAIDING = 4 -- used when player stuff is close
constants.SQUAD_SETTLING = 5
constants.SQUAD_BUILDING = 6

-- Squad Related

constants.COMPRESSION_COOLDOWN = 600

constants.MINIMUM_EXPANSION_DISTANCE = 10

constants.RETREAT_GRAB_RADIUS = 24
constants.RETREAT_SPAWNER_GRAB_RADIUS = 75

constants.BASE_RALLY_CHANCE = 0.02
constants.BONUS_RALLY_CHANCE = 0.04

constants.RALLY_CRY_DISTANCE = 96
constants.SETTLER_DISTANCE = 224

constants.GROUP_MERGE_DISTANCE = 28

constants.MAX_PENALTY_BEFORE_PURGE = 36000

constants.COMMAND_TIMEOUT = 1 * constants.TICKS_A_MINUTE
constants.BUILD_COMMAND_TIMEOUT = 7 * constants.TICKS_A_MINUTE

-- player building pheromones

constants.GENERATOR_PHEROMONE_LEVEL_1 = 25
constants.GENERATOR_PHEROMONE_LEVEL_2 = 100
constants.GENERATOR_PHEROMONE_LEVEL_3 = 500
constants.GENERATOR_PHEROMONE_LEVEL_4 = 1000
constants.GENERATOR_PHEROMONE_LEVEL_5 = 1750
constants.GENERATOR_PHEROMONE_LEVEL_6 = 6000
constants.GENERATOR_PHEROMONE_VICTORY_NORMALIZER = 20000

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
for key, building in pairs(constants.BUILDING_PHEROMONES)  do
    constants.VICTORY_SCENT[key] = building / constants.GENERATOR_PHEROMONE_VICTORY_NORMALIZER
end

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

local tiersSet = {}
constants.TIERS = 10
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

buildTier(constants.TIERS, tiersSet)

constants.TIER_UPGRADE_SET = tiersSet

local variations = constants.ENEMY_VARIATIONS

constants.ENERGY_THIEF_LOOKUP = {}

for tier=1, constants.TIERS do
    for i=1,variations do
        constants.ENERGY_THIEF_LOOKUP["energy-thief-worm-v" .. i .. "-t" .. tier .. "-rampant"] = true
    end
end

for tier=1, constants.TIERS do
    for i=1,variations do
        constants.ENERGY_THIEF_LOOKUP["energy-thief-biter-v" .. i .. "-t" .. tier .. "-rampant"] = true
    end
end

local function hexToRGBA(hex)
    local index = 1
    local a = {"r","g","b","a"}
    local tint = {}
    string.gsub(
        hex,
        "..",
        function(cc)
            tint[a[index]] = tonumber(cc, 16) / 255
            index = index+1
        end
    )
    return tint
end

local function convertHexPairs(hexPairString)
    if #hexPairString ~= 26 then
        error(
            "Invalid rgba hex value for Rampant new enemy color, each color needs to have `rrggbbaa rrggbbaa rrggbbaa`:" .. hexPairString
        )
    end
    local index = 1
    local a = {"tint", "tint2", "tint3"}
    local tints = {}
    for hex in string.gmatch(hexPairString, "%x%x%x%x%x%x%x%x") do
        tints[a[index]] = hexToRGBA(hex)
        index = index + 1
    end
    return tints
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

local neutralTints = convertHexPairs(settings.startup["rampant--neutralTints"].value)
constants.FACTION_SET[#constants.FACTION_SET+1] = {
    type = "neutral",
    tint = neutralTints.tint,
    tint2 = neutralTints.tint2,
    tint3 = neutralTints.tint3,
    acceptRate = {tierStart=1, tierEnd=7, tierChanceStart=0.3, tierChanceEnd=0.1},
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
            minorResistances = {},
            acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
            attributes = {},
            drops = {"nilArtifact"},
            buildSets = {
                {name="spitter", tierStart=1, tierEnd=10}
            }
        },
        {
            type = "biter-spawner",
            name = "biter-spawner",
            majorResistances = {},
            minorResistances = {},
            acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
            attributes = {},
            drops = {"nilArtifact"},
            buildSets = {
                {name="biter", tierStart=1, tierEnd=10}
            }
        },
        {
            type = "turret",
            name = "worm",
            majorResistances = {},
            minorResistances = {},
            acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
            attackAttributes = {"spit", "acid"},
            attributes = {},
            drops = {"nilArtifact"}
        },
        {
            type = "hive",
            name = "hive",
            majorResistances = {},
            minorResistances = {},
            acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
            attributes = {"spawnDuringDays", "twoUnits"},
            drops = {"nilArtifact"},
            buildSets = {
                {name="biter", tierStart=1, tierEnd=10},
                {name="spitter", tierStart=1, tierEnd=10}
            }
        }
    }
}

if settings.startup["rampant--acidEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "acid"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "acid"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "acid"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "acid"
    local acidTints = convertHexPairs(settings.startup["rampant--acidTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "acid",
        tint = acidTints.tint,
        tint2 = acidTints.tint2,
        tint3 = acidTints.tint3,
        acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.2},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                attributes = {},
                drops = {"greenArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                attributes = {},
                drops = {"greenArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                attackAttributes = {"spit", "acid"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {},
                drops = {"greenArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"acid"},
                minorResistances = {"poison"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                attributes = {"spawnDuringDays", "twoUnits"},
                drops = {"greenArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10},
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--laserEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "laser"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "laser"
    local laserTints = convertHexPairs(settings.startup["rampant--laserTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "laser",
        tint = laserTints.tint,
        tint2 = laserTints.tint2,
        tint3 = laserTints.tint3,
        acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.15},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"laser", "electric"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"laser", "electric"},
                attackAttributes = {"spit", "laser", "cluster"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {},
                drops = {"blueArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"laser", "electric"},
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"blueArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10},
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--fireEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "fire"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "fire"
    local fireTints = convertHexPairs(settings.startup["rampant--fireTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "fire",
        tint = fireTints.tint,
        tint2 = fireTints.tint2,
        tint3 = fireTints.tint3,
        acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.15},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                attributes = {},
                drops = {"redArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {"fire", "acid"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                minorResistances = {},
                attributes = {},
                drops = {"redArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attackAttributes = {"spit", "acid"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {},
                drops = {"redArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"redArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10},
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--infernoEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "inferno"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "inferno"
    local infernoTints = convertHexPairs(settings.startup["rampant--infernoTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "inferno",
        tint = infernoTints.tint,
        tint2 = infernoTints.tint2,
        tint3 = infernoTints.tint3,
        acceptRate = {tierStart=3, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.125},
        evo = 0.2,
        units = {
            {
                type = "spitter",
                attackAttributes = {"spitFire", "acid"},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                attributes = {},
                drops = {"orangeArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"acid", "fire"},
                minorWeaknesses = {"poison"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attackAttributes = {"spitFire", "acid"},
                attributes = {},
                drops = {"orangeArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"fire", "acid"},
                minorResistances = {},
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"orangeArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--waspEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "wasp"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "wasp"
    local waspTints = convertHexPairs(settings.startup["rampant--waspTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "wasp",
        tint = waspTints.tint,
        tint2 = waspTints.tint2,
        tint3 = waspTints.tint3,
        acceptRate = {tierStart=3, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.125},
        evo = 0.2,
        units = {
            {
                type = "drone",
                attackAttributes = {"spit", "acid"},
                name = "spitter-wasp",
                attributes = {"followsPlayer", "skipKillCount"},
                drops = {}
            },
            {
                type = "drone",
                attackAttributes = {"stream", "acid"},
                name = "worm-wasp",
                attributes = {"stationary", "skipKillCount"},
                drops = {}
            },
            {
                type = "spitter",
                attackAttributes = {"capsule", {"drone", "spitter-wasp"}},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                drops = {"purpleArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                attackAttributes = {"capsule", {"drone", "worm-wasp"}},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {},
                drops = {"purpleArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"purpleArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
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
    local spawnerTints = convertHexPairs(settings.startup["rampant--spawnerTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "spawner",
        tint = spawnerTints.tint,
        tint2 = spawnerTints.tint2,
        tint3 = spawnerTints.tint3,
        acceptRate = {tierStart=3, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.125},
        evo = 0.2,
        units = {
            {
                type = "biter",
                attackAttributes = {"melee"},
                name = "spawn",
                attributes = {"fragile", "unstable", "smallest", "skipKillCount"},
                drops = {}
            },
            {
                type = "drone",
                attackAttributes = {"touch", "acid"},
                name = "spitter-egg",
                attributes = {"stationary", "bigger", "highestRegen", {"clusterDeath", "spawn"}, "skipKillCount"},
                drops = {}
            },
            {
                type = "drone",
                attackAttributes = {"touch", "acid"},
                name = "worm-egg",
                attributes = {"stationary", "bigger", "highestRegen", {"clusterDeath", "spawn"}, "skipKillCount"},
                drops = {}
            },
            {
                type = "spitter",
                attackAttributes = {"capsule", {"drone", "spitter-egg"}},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                attackAttributes = {"capsule", {"drone", "worm-egg"}},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {},
                drops = {"orangeArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"orangeArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--electricEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "electric"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "electric"
    local electricTints = convertHexPairs(settings.startup["rampant--electricTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "electric",
        tint = electricTints.tint,
        tint2 = electricTints.tint2,
        tint3 = electricTints.tint3,
        acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.15},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"electric"},
                minorResistances = {"laser"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attackAttributes = {"spit", "electric", "cluster"},
                attributes = {},
                drops = {"blueArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"electric"},
                minorResistances = {"laser"},
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"blueArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--physicalEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "physical"
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "physical"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "physical"
    local physicalTints = convertHexPairs(settings.startup["rampant--physicalTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "physical",
        tint = physicalTints.tint,
        tint2 = physicalTints.tint2,
        tint3 = physicalTints.tint3,
        acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.15},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                drops = {"redArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"physical", "explosion"},
                minorWeaknesses = {"laser", "electric"},
                attackAttributes = {"spit", "physical"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {"highHealth", "bigger"},
                drops = {"redArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"physical", "explosion"},
                minorResistances = {"laser", "electric"},
                attributes = {"highHealth", "bigger", "spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"redArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--trollEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "troll"
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "troll"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "troll"
    local trollTints = convertHexPairs(settings.startup["rampant--trollTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "troll",
        tint = trollTints.tint,
        tint2 = trollTints.tint2,
        tint3 = trollTints.tint3,
        acceptRate = {tierStart=3, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.125},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                attributes = {"highestHealth", "bigger", "highestRegen"},
                drops = {"greenArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                attackAttributes = {"spit", "physical"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {"highestHealth", "bigger", "highestRegen"},
                drops = {"greenArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                minorResistances = {"physical", "explosion"},
                majorWeaknesses = {"fire"},
                attributes = {"highestHealth", "bigger", "highRegen","spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"greenArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--poisonEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "poison"
    constants.FACTIONS_BY_DAMAGE_TYPE["acid"][#constants.FACTIONS_BY_DAMAGE_TYPE["acid"]+1] = "poison"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "poison"
    local poisonTints = convertHexPairs(settings.startup["rampant--poisonTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "poison",
        tint = poisonTints.tint,
        tint2 = poisonTints.tint2,
        tint3 = poisonTints.tint3,
        acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.15},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                drops = {"greenArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                minorResistances = {"fire"},
                majorResistances = {"poison"},
                minorWeaknesses = {"electric", "explosion", "laser"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
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
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"greenArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--suicideEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "suicide"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "suicide"
    local suicideTints = convertHexPairs(settings.startup["rampant--suicideTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "suicide",
        tint = suicideTints.tint,
        tint2 = suicideTints.tint2,
        tint3 = suicideTints.tint3,
        acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.05, tierChanceEnd=0.15},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                attributes = {},
                drops = {"yellowArtifact", "quickSpawning", "lowUnits"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"explosion"},
                minorResistances = {"poison"},
                attackAttributes = {"spit", "acid", "slow"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attributes = {},
                drops = {"yellowArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"explosion"},
                minorResistances = {"poison"},
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"yellowArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--nuclearEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["impact"][#constants.FACTIONS_BY_DAMAGE_TYPE["impact"]+1] = "nuclear"
    constants.FACTIONS_BY_DAMAGE_TYPE["poison"][#constants.FACTIONS_BY_DAMAGE_TYPE["poison"]+1] = "nuclear"
    local nuclearTints = convertHexPairs(settings.startup["rampant--nuclearTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "nuclear",
        tint = nuclearTints.tint,
        tint2 = nuclearTints.tint2,
        tint3 = nuclearTints.tint3,
        acceptRate = {tierStart=4, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.125},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                attributes = {},
                drops = {"yellowArtifact", "quickSpawning", "lowUnits"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"explosion"},
                minorResistances = {"fire"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attackAttributes = {"spit", "acid", "slow"},
                attributes = {},
                drops = {"yellowArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"explosion"},
                minorResistances = {"fire"},
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"yellowArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--energyThiefEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["laser"][#constants.FACTIONS_BY_DAMAGE_TYPE["laser"]+1] = "energy-thief"
    constants.FACTIONS_BY_DAMAGE_TYPE["electric"][#constants.FACTIONS_BY_DAMAGE_TYPE["electric"]+1] = "energy-thief"
    local energyThiefTints = convertHexPairs(settings.startup["rampant--energyThiefTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "energy-thief",
        tint = energyThiefTints.tint,
        tint2 = energyThiefTints.tint2,
        tint3 = energyThiefTints.tint3,
        acceptRate = {tierStart=3, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.125},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.4, tierChanceEnd=0.6},
                attributes = {},
                drops = {"blueArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {"electric", "laser"},
                minorResistances = {},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attackAttributes = {"spit", "electric", "cluster"},
                attributes = {},
                drops = {"blueArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {"electric", "laser"},
                attributes = {"spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"blueArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            }
        }
    }
end

if settings.startup["rampant--fastEnemy"].value then
    constants.FACTIONS_BY_DAMAGE_TYPE["physical"][#constants.FACTIONS_BY_DAMAGE_TYPE["physical"]+1] = "fast"
    constants.FACTIONS_BY_DAMAGE_TYPE["explosion"][#constants.FACTIONS_BY_DAMAGE_TYPE["explosion"]+1] = "fast"
    constants.FACTIONS_BY_DAMAGE_TYPE["fire"][#constants.FACTIONS_BY_DAMAGE_TYPE["fire"]+1] = "fast"
    local fastTints = convertHexPairs(settings.startup["rampant--fastTints"].value)
    constants.FACTION_SET[#constants.FACTION_SET+1] = {
        type = "fast",
        tint = fastTints.tint,
        tint2 = fastTints.tint2,
        tint3 = fastTints.tint3,
        acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.1, tierChanceEnd=0.15},
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
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                drops = {"purpleArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "biter-spawner",
                name = "biter-spawner",
                majorResistances = {},
                minorResistances = {"explosion"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.3, tierChanceEnd=0.5},
                attributes = {"quickSpawning"},
                drops = {"purpleArtifact"},
                buildSets = {
                    {name="biter", tierStart=1, tierEnd=10}
                }
            },
            {
                type = "turret",
                name = "worm",
                majorResistances = {},
                minorResistances = {"explosion"},
                acceptRate = {tierStart=1, tierEnd=10, tierChanceStart=0.8, tierChanceEnd=0.6},
                attackAttributes = {"spit", "acid"},
                attributes = {"quickCooldown"},
                drops = {"purpleArtifact"}
            },
            {
                type = "hive",
                name = "hive",
                majorResistances = {},
                minorResistances = {"explosion"},
                attributes = {"quickSpawning", "spawnDuringDays", "twoUnits"},
                acceptRate = {tierStart=2, tierEnd=10, tierChanceStart=0.001, tierChanceEnd=0.0175},
                drops = {"purpleArtifact"},
                buildSets = {
                    {name="spitter", tierStart=1, tierEnd=10},
                    {name="biter", tierStart=1, tierEnd=10}
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

constants.PENDING_UPGRADE_CREATION_THESHOLD = 100

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

constants.HIVE_MAX_NESTS = {
    2,
    3,
    3,
    4,
    4,
    4,
    5,
    5,
    6,
    6
}
constants.HIVE_MAX_TURRETS = {
    4,
    4,
    5,
    5,
    6,
    6,
    7,
    7,
    8,
    8
}
constants.HIVE_MAX_HIVES = {
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    2,
    2,
    2
}

constants.FACTION_MUTATION_MAPPING = {}
constants.FACTION_MUTATION_MAPPING["spitter-spawner"] = {"biter-spawner", "hive"}
constants.FACTION_MUTATION_MAPPING["biter-spawner"] = {"spitter-spawner", "hive"}
constants.FACTION_MUTATION_MAPPING["hive"] = {"utility", "biter-spawner", "spitter-spawner"}
constants.FACTION_MUTATION_MAPPING["turret"] = {}
constants.FACTION_MUTATION_MAPPING["utility"] = {"hive", "biter-spawner", "spitter-spawner"}

function constants.gpsDebug(x, y, msg)
    game.print("[gps=".. x .. "," .. y .. "]" .. msg)
end

function constants.gpsSurfaceDebug(surface, x, y, msg)
    surface.print("[gps=".. x .. "," .. y .. "]" .. msg)
end

constants.MAX_HIVE_TTL = 12400
constants.MIN_HIVE_TTL = 4800
constants.DEV_HIVE_TTL = 450

local rg = mathUtils.xorRandom(settings.startup["rampant--enemySeed"].value)

local alignmentSet = {}
constants.EVOLUTION_TABLE_ALIGNMENT = alignmentSet -- evolutionTableAlignment
local enemyAlignmentLookup = {}
constants.ENEMY_ALIGNMENT_LOOKUP = enemyAlignmentLookup -- enemyAlignmentLookup
local upgradeLookup = {}
constants.UPGRADE_LOOKUP = upgradeLookup --upgradeLookup
local buildingEvolveLookup = {}
constants.BUILDING_EVOLVE_LOOKUP = buildingEvolveLookup --buildingEvolveLookup
local costLookup = {}
constants.COST_LOOKUP = costLookup --costLookup
local buildingHiveTypeLookup = {}
constants.BUILDING_HIVE_TYPE_LOOKUP = buildingHiveTypeLookup --buildingHiveTypeLookup
local buildingHiveTierLookup = {}
constants.BUILDING_HIVE_TIER_LOOKUP = buildingHiveTierLookup --buildingHiveTierLookup
local vanillaEntityLookup = {}
constants.VANILLA_ENTITY_TYPE_LOOKUP = vanillaEntityLookup --vanillaEntityTypeLookup
local entitySkipCountLookup = {}
constants.ENTITY_SKIP_COUNT_LOOKUP = entitySkipCountLookup --entitySkipCountLookup

buildingHiveTypeLookup["biter-spawner"] = "biter-spawner"
buildingHiveTypeLookup["spitter-spawner"] = "spitter-spawner"
buildingHiveTypeLookup["small-worm-turret"] = "turret"
buildingHiveTypeLookup["medium-worm-turret"] = "turret"
buildingHiveTypeLookup["big-worm-turret"] = "turret"
buildingHiveTypeLookup["behemoth-worm-turret"] = "turret"

buildingHiveTierLookup["biter-spawner"] = 1
buildingHiveTierLookup["spitter-spawner"] = 1
buildingHiveTierLookup["small-worm-turret"] = 1
buildingHiveTierLookup["medium-worm-turret"] = 1
buildingHiveTierLookup["big-worm-turret"] = 1
buildingHiveTierLookup["behemoth-worm-turret"] = 1

vanillaEntityLookup["biter-spawner"] = true
vanillaEntityLookup["spitter-spawner"] = true
vanillaEntityLookup["small-worm-turret"] = true
vanillaEntityLookup["medium-worm-turret"] = true
vanillaEntityLookup["big-worm-turret"] = true
vanillaEntityLookup["behemoth-worm-turret"] = true

local function isMember(lst, x)
    for _,l in pairs(lst) do
        if l == x then
            return true
        end
    end
    return false
end

for i=1,#constants.FACTION_SET do
    local faction = constants.FACTION_SET[i]

    local factionUpgradeLookup = {}
    upgradeLookup[faction.type] = factionUpgradeLookup
    local factionBuildingPicker = {}
    buildingEvolveLookup[faction.type] = factionBuildingPicker

    for t=1,constants.TIERS do
        local alignments = alignmentSet[t]
        if not alignments then
            alignments = {}
            alignmentSet[t] = alignments
        end

        --[[
            alignments table is a table that is used for selecting what factions are available
            to pick given an evolution level.

            evolutionTable is a table that given a faction allows the selection of a building
            type based on the propabilities given. Once the the building type is selected given
            a faction, then the evolution decides what level of building to select
        --]]
        local factionAcceptRate = faction.acceptRate

        local low = factionAcceptRate.tierStart
        local high = factionAcceptRate.tierEnd
        if (low <= t) and (t <= high) then
            alignments[#alignments+1] = {
                mathUtils.distort(
                    rg,
                    mathUtils.linearInterpolation(
                        (t - low) / (high - low),
                        factionAcceptRate.tierChanceStart,
                        factionAcceptRate.tierChanceEnd
                    )
                ),
                faction.type
            }
        end

        local tieredUpgradeBuildingSet = factionUpgradeLookup[t]
        if not tieredUpgradeBuildingSet then
            tieredUpgradeBuildingSet = {}
            factionUpgradeLookup[t] = tieredUpgradeBuildingSet
        end

        local tieredBuildingPickerSet = factionBuildingPicker[t]
        if not tieredBuildingPickerSet then
            tieredBuildingPickerSet = {}
            factionBuildingPicker[t] = tieredBuildingPickerSet
        end

        for b=1,#faction.buildings do
            local building = faction.buildings[b]

            local buildingSet = tieredUpgradeBuildingSet[building.type]
            if not buildingSet then
                buildingSet = {}
                tieredUpgradeBuildingSet[building.type] = buildingSet
            end

            local variationSet = {}
            for v=1,constants.ENEMY_VARIATIONS
            do
                local entry = faction.type .. "-" .. building.name .. "-v" .. v .. "-t" .. t .. "-rampant"
                enemyAlignmentLookup[entry] = faction.type
                costLookup[entry] = constants.HIVE_BUILDINGS_COST[building.type]
                buildingHiveTypeLookup[entry] = building.type
                buildingHiveTierLookup[entry] = t
                variationSet[#variationSet+1] = entry
                for _,unit in pairs(faction.units) do
                    if isMember(unit.attributes, "skipKillCount") then
                        local name = faction.type .. "-" .. unit.name .. "-v" .. v .. "-t" .. t .. "-rampant"
                        constants.ENTITY_SKIP_COUNT_LOOKUP[name] = true
                    end
                end
            end

            local buildingAcceptRate = building.acceptRate

            local buildingLow = buildingAcceptRate.tierStart
            local buildingHigh = buildingAcceptRate.tierEnd
            if (buildingLow <= t) and (t <= buildingHigh) then
                for vi=1,#variationSet do
                    local variation = variationSet[vi]
                    buildingSet[#buildingSet+1] = variation
                end
                tieredBuildingPickerSet[#tieredBuildingPickerSet+1] = {
                    mathUtils.distort(
                        rg,
                        mathUtils.linearInterpolation(
                            (t - buildingLow) / (buildingHigh - buildingLow),
                            buildingAcceptRate.tierChanceStart,
                            buildingAcceptRate.tierChanceEnd
                        )
                    ),
                    variationSet,
                    building.type
                }
            end
        end
    end
end

for t=1,constants.TIERS do
    local alignments = alignmentSet[t]
    local totalAlignment = 0
    for i=1,#alignments do
        totalAlignment = totalAlignment + alignments[i][1]
    end
    for i=1,#alignments do
        alignments[i][1] = alignments[i][1] / totalAlignment
    end

    for fi=1,#constants.FACTION_SET do
        local faction = constants.FACTION_SET[fi]
        local factionBuildingSet = buildingEvolveLookup[faction.type][t]
        local totalBuildingSet = 0
        for i=1,#factionBuildingSet do
            totalBuildingSet = totalBuildingSet + factionBuildingSet[i][1]
        end
        for i=1,#factionBuildingSet do
            factionBuildingSet[i][1] = factionBuildingSet[i][1] / totalBuildingSet
        end
    end
end

local evoToTierMapping = {}
constants.EVO_TO_TIER_MAPPING = evoToTierMapping

for i=1,constants.TIERS do
    evoToTierMapping[#evoToTierMapping+1] = (((i - 1) * 0.1) ^ 0.5) - 0.05
end

constantsG =  constants
return constants
