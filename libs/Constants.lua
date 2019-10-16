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
constants.ATTACK_QUEUE_SIZE = 20
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
constants.BASE_AI_STATE_WORMS = 2
constants.BASE_AI_STATE_NESTS = 3
constants.BASE_AI_STATE_OVERDRIVE = 4
constants.BASE_AI_STATE_MUTATE = 5


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
-- constants.BASE_DISTANCE_TO_EVO_INDEX = 1 / 32000

constants.BASE_SPAWNER_UPGRADE = 250
constants.BASE_WORM_UPGRADE = 200
constants.BASE_UPGRADE = 1500

constants.BASE_DISTANCE_THRESHOLD = 30 * constants.CHUNK_SIZE
constants.BASE_DISTANCE_LEVEL_BONUS = 15

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
constants.BASE_ALIGNMENT_DEADZONE = 20
constants.BASE_ALIGNMENT_NE = 21
constants.BASE_ALIGNMENT_BOBS = 22
constants.BASE_ALIGNMENT_SPAWNER = 23
constants.BASE_ALIGNMENT_NE_BLUE = 24
constants.BASE_ALIGNMENT_NE_RED = 25
constants.BASE_ALIGNMENT_NE_YELLOW = 26
constants.BASE_ALIGNMENT_NE_GREEN = 27
constants.BASE_ALIGNMENT_NE_PINK = 28
-- constants.BASE_ALIGNMENT_BURROW = 3

constants.BASE_PROCESS_INTERVAL = constants.TICKS_A_SECOND * 2

-- neutralPath[constants.BASE_ALIGNMENT_DECAYING] = true
-- neutralPath[constants.BASE_ALIGNMENT_WEB] = true
-- neutralPath[constants.BASE_ALIGNMENT_WASP] = true

-- local acidPath = {}
-- acidPath[constants.BASE_ALIGNMENT_POISON] = true
-- acidPath[constants.BASE_ALIGNMENT_INFEST] = true

-- local decayingPath = {}
-- decayingPath[constants.BASE_ALIGNMENT_UNDYING] = true

constants.BASE_ALIGNMENT_EVOLUTION_BASELINE = {
    [constants.BASE_ALIGNMENT_NEUTRAL] = 0
}

constants.BASE_ALIGNMENT_PATHS = {}
-- constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_NEUTRAL] = {
--     constants.BASE_ALIGNMENT_ACID,
--     constants.BASE_ALIGNMENT_FIRE,
--     constants.BASE_ALIGNMENT_WASP,
--     constants.BASE_ALIGNMENT_PHYSICAL,
--     constants.BASE_ALIGNMENT_ELECTRIC,
--     constants.BASE_ALIGNMENT_SUICIDE,
--     constants.BASE_ALIGNMENT_TROLL,
--     constants.BASE_ALIGNMENT_FAST
-- }

local function pushBasePath(x)
    local tbl = constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_NEUTRAL]
    if not tbl then
        tbl = {}
        constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_NEUTRAL] = tbl
    end
    tbl[#tbl+1] = x
end

if settings.startup["rampant-acidEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_ACID)
    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_ACID] = 0.1
end

if settings.startup["rampant-physicalEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_PHYSICAL)
    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_PHYSICAL] = 0.4
end

if settings.startup["rampant-suicideEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_SUICIDE)
    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_SUICIDE] = 0.3
end

if settings.startup["rampant-fireEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_FIRE)
    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_FIRE] = 0.4
end

if settings.startup["rampant-electricEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_ELECTRIC)
    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_ELECTRIC] = 0.2
end

if settings.startup["rampant-nuclearEnemy"].value then
    if settings.startup["rampant-suicideEnemy"].value then
        constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_SUICIDE] = { constants.BASE_ALIGNMENT_NUCLEAR }
    end

    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_NUCLEAR] = 0.7
end

if settings.startup["rampant-fastEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_FAST)
    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_FAST] = 0.5
end

if settings.startup["rampant-trollEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_TROLL)
    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_TROLL] = 0.5
end

if settings.startup["rampant-laserEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_LASER)

    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_LASER] = 0.4
end

if settings.startup["rampant-waspEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_WASP)

    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_WASP] = 0.5
end

if settings.startup["rampant-energyThiefEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_ENERGY_THIEF)

    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_ENERGY_THIEF] = 0.4
end

if settings.startup["rampant-poisonEnemy"].value then
    pushBasePath(constants.BASE_ALIGNMENT_POISON)

    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_POISON] = 0.4
end

-- constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_FIRE] = { constants.BASE_ALIGNMENT_INFERNO }
-- constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_SUICIDE] = { constants.BASE_ALIGNMENT_NUCLEAR }
-- constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_WASP] = { constants.BASE_ALIGNMENT_SPAWNER }
-- constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_ACID] = acidPath
-- constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_DECAYING] = decayingPath
-- constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_ELECTRIC] = { constants.BASE_ALIGNMENT_LASER }

-- [constants.BASE_ALIGNMENT_WASP] = 0.5,
--     [constants.BASE_ALIGNMENT_SPAWNER] = 0.7,
--     [constants.BASE_ALIGNMENT_INFERNO] = 0.6,

if settings.startup["rampant-infernoEnemy"].value then
    if settings.startup["rampant-fireEnemy"].value then
        constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_FIRE] = { constants.BASE_ALIGNMENT_INFERNO }
    end

    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_INFERNO] = 0.6
end

if settings.startup["rampant-spawnerEnemy"].value then
    if settings.startup["rampant-waspEnemy"].value then
        constants.BASE_ALIGNMENT_PATHS[constants.BASE_ALIGNMENT_WASP] = { constants.BASE_ALIGNMENT_SPAWNER }
    end

    local tbl = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE
    tbl[constants.BASE_ALIGNMENT_SPAWNER] = 0.7
end




constants.ENABLED_NE_UNITS = settings.startup["rampant-enableNEUnits"].value and (settings.startup["NE_Difficulty"] ~= nil)

constants.ENABLED_BOBS_UNITS = settings.startup["rampant-enableBobsUnits"].value and (settings.startup["bobmods-enemies-enableartifacts"] ~= nil)

if constants.ENABLED_BOBS_UNITS then
    constants.BASE_ALIGNMENT_EVOLUTION_BASELINE[constants.BASE_ALIGNMENT_BOBS] = 0.1
end

if constants.ENABLED_NE_UNITS then
    constants.BASE_ALIGNMENT_EVOLUTION_BASELINE[constants.BASE_ALIGNMENT_NE] = 0.1

    if settings.startup["NE_Blue_Spawners"].value then
	constants.BASE_ALIGNMENT_EVOLUTION_BASELINE[constants.BASE_ALIGNMENT_NE_BLUE] = 0.1
    end
    if settings.startup["NE_Red_Spawners"].value then
	constants.BASE_ALIGNMENT_EVOLUTION_BASELINE[constants.BASE_ALIGNMENT_NE_RED] = 0.1
    end
    if settings.startup["NE_Pink_Spawners"].value then
	constants.BASE_ALIGNMENT_EVOLUTION_BASELINE[constants.BASE_ALIGNMENT_NE_PINK] = 0.1
    end
    if settings.startup["NE_Green_Spawners"].value then
	constants.BASE_ALIGNMENT_EVOLUTION_BASELINE[constants.BASE_ALIGNMENT_NE_GREEN] = 0.1
    end
    if settings.startup["NE_Yellow_Spawners"].value then
	constants.BASE_ALIGNMENT_EVOLUTION_BASELINE[constants.BASE_ALIGNMENT_NE_YELLOW] = 0.1
    end
end

-- ai retreat

constants.NO_RETREAT_BASE_PERCENT = 0.10
constants.NO_RETREAT_EVOLUTION_BONUS_MAX = 0.25
constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX = 0.40

-- pheromone amounts

constants.MOVEMENT_PENALTY_AMOUNT = 300000
constants.DEATH_PHEROMONE_GENERATOR_AMOUNT = 1300
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

constants.BUILDING_PHEROMONES = {}
constants.BUILDING_PHEROMONES["wall"] = 25
constants.BUILDING_PHEROMONES["transport-belt"] = 25 -- 1
constants.BUILDING_PHEROMONES["splitter"] = 100 
constants.BUILDING_PHEROMONES["pump"] = 100 
constants.BUILDING_PHEROMONES["offshore-pump"] = 100 -- 2
constants.BUILDING_PHEROMONES["lamp"] = 500
constants.BUILDING_PHEROMONES["generator"] = 500 
constants.BUILDING_PHEROMONES["solar-panel"] = 500
constants.BUILDING_PHEROMONES["programmable-speaker"] = 500
constants.BUILDING_PHEROMONES["accumulator"] = 500
constants.BUILDING_PHEROMONES["assembling-machine"] = 500
constants.BUILDING_PHEROMONES["turret"] = 500
constants.BUILDING_PHEROMONES["roboport"] = 500
constants.BUILDING_PHEROMONES["beacon"] = 500
constants.BUILDING_PHEROMONES["ammo-turret"] = 500 -- 3
constants.BUILDING_PHEROMONES["boiler"] = 500
constants.BUILDING_PHEROMONES["furnace"] = 500 
constants.BUILDING_PHEROMONES["lab"] = 500 
constants.BUILDING_PHEROMONES["reactor"] = 500 
constants.BUILDING_PHEROMONES["radar"] = 500 
constants.BUILDING_PHEROMONES["electric-turret"] = 500 -- 4
constants.BUILDING_PHEROMONES["fluid-turret"] = 1750 
constants.BUILDING_PHEROMONES["mining-drill"] = 1750 -- 5
constants.BUILDING_PHEROMONES["artillery-turret"] = 6000
constants.BUILDING_PHEROMONES["rocket-silo"] = 6000 -- 6


-- constants.RETREAT_FILTER = {}
-- constants.RETREAT_FILTER[constants.SQUAD_RETREATING] = true

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
-- constants.SENTINEL_IMPASSABLE_CHUNK[constants.PASSABLE] = constants.CHUNK_IMPASSABLE
constants.SENTINEL_IMPASSABLE_CHUNK[constants.CHUNK_TICK] = 0
-- constants.SENTINEL_IMPASSABLE_CHUNK[constants.PATH_RATING] = 0
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


-- unit spawners

local function roundToNearest(number, multiple)
    local num = number + (multiple * 0.5)
    return num - (num % multiple)
end

local tiers5 = {}
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

buildTier(5, tiers5)
buildTier(10, tiers10)

constants.TIER_UPGRADE_SET_5 = tiers5
constants.TIER_UPGRADE_SET_10 = tiers10

constants.TIER_NAMING_SET_5 = { 1, 3, 5, 7, 10 }
constants.TIER_NAMING_SET_10 = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }

local nestVariations = settings.startup["rampant-newEnemyNestVariations"].value
local nestTiers = settings.startup["rampant-newEnemyNestTiers"].value
local wormVariations = settings.startup["rampant-newEnemyWormVariations"].value
local wormTiers = settings.startup["rampant-newEnemyWormTiers"].value
local unitVariations = settings.startup["rampant-newEnemyUnitVariations"].value
local unitTiers = settings.startup["rampant-newEnemyUnitTiers"].value

constants.ENERGY_THIEF_LOOKUP = {}

for tier=1, wormTiers do
    local t = ((wormTiers == 5) and constants.TIER_NAMING_SET_5[tier]) or constants.TIER_NAMING_SET_10[tier]
    for i=1,wormVariations do
        constants.ENERGY_THIEF_LOOKUP["energy-thief-worm-v" .. i .. "-t" .. t .. "-rampant"] = true
    end
end

for tier=1, unitTiers do
    local t = ((unitTiers == 5) and constants.TIER_NAMING_SET_5[tier]) or constants.TIER_NAMING_SET_10[tier]
    for i=1,unitVariations do
        constants.ENERGY_THIEF_LOOKUP["energy-thief-biter-v" .. i .. "-t" .. t .. "-rampant"] = true
    end
end

constants.POISON_LOOKUP = {}

for tier=1, unitTiers do
    local t = ((unitTiers == 5) and constants.TIER_NAMING_SET_5[tier]) or constants.TIER_NAMING_SET_10[tier]
    local ct = ((unitTiers == 5) and constants.TIER_UPGRADE_SET_5[tier]) or constants.TIER_UPGRADE_SET_10[tier]
    for i=1,unitVariations do
        constants.POISON_LOOKUP["poison-biter-v" .. i .. "-t" .. t .. "-rampant"] = "poison-cloud-v" .. ct .. "-cloud-rampant"
    end
end


constants.SPAWNER_EGG_TIMEOUT = constants.TICKS_A_SECOND * 5

constants.NEUTRAL_NEST_TIERS = nestTiers
constants.NEUTRAL_NEST_VARIATIONS = nestVariations
constants.NEUTRAL_WORM_TIERS = wormTiers
constants.NEUTRAL_WORM_VARIATIONS = wormVariations
constants.NEUTRAL_UNIT_TIERS = unitTiers
constants.NEUTRAL_UNIT_VARIATIONS = unitVariations

constants.ACID_NEST_TIERS = nestTiers
constants.ACID_NEST_VARIATIONS = nestVariations
constants.ACID_WORM_TIERS = wormTiers
constants.ACID_WORM_VARIATIONS = wormVariations
constants.ACID_UNIT_TIERS = unitTiers
constants.ACID_UNIT_VARIATIONS = unitVariations

constants.FIRE_NEST_TIERS = nestTiers
constants.FIRE_NEST_VARIATIONS = nestVariations
constants.FIRE_WORM_TIERS = wormTiers
constants.FIRE_WORM_VARIATIONS = wormVariations
constants.FIRE_UNIT_TIERS = unitTiers
constants.FIRE_UNIT_VARIATIONS = unitVariations

constants.PHYSICAL_NEST_TIERS = nestTiers
constants.PHYSICAL_NEST_VARIATIONS = nestVariations
constants.PHYSICAL_WORM_TIERS = wormTiers
constants.PHYSICAL_WORM_VARIATIONS = wormVariations
constants.PHYSICAL_UNIT_TIERS = unitTiers
constants.PHYSICAL_UNIT_VARIATIONS = unitVariations

constants.TROLL_NEST_TIERS = nestTiers
constants.TROLL_NEST_VARIATIONS = nestVariations
constants.TROLL_WORM_TIERS = wormTiers
constants.TROLL_WORM_VARIATIONS = wormVariations
constants.TROLL_UNIT_TIERS = unitTiers
constants.TROLL_UNIT_VARIATIONS = unitVariations

constants.SPAWNER_NEST_TIERS = nestTiers
constants.SPAWNER_NEST_VARIATIONS = nestVariations
constants.SPAWNER_WORM_TIERS = wormTiers
constants.SPAWNER_WORM_VARIATIONS = wormVariations
constants.SPAWNER_UNIT_TIERS = unitTiers
constants.SPAWNER_UNIT_VARIATIONS = unitVariations

constants.FAST_NEST_TIERS = nestTiers
constants.FAST_NEST_VARIATIONS = nestVariations
constants.FAST_WORM_TIERS = wormTiers
constants.FAST_WORM_VARIATIONS = wormVariations
constants.FAST_UNIT_TIERS = unitTiers
constants.FAST_UNIT_VARIATIONS = unitVariations

constants.SUICIDE_NEST_TIERS = nestTiers
constants.SUICIDE_NEST_VARIATIONS = nestVariations
constants.SUICIDE_WORM_TIERS = wormTiers
constants.SUICIDE_WORM_VARIATIONS = wormVariations
constants.SUICIDE_UNIT_TIERS = unitTiers
constants.SUICIDE_UNIT_VARIATIONS = unitVariations

constants.WASP_NEST_TIERS = nestTiers
constants.WASP_NEST_VARIATIONS = nestVariations
constants.WASP_WORM_TIERS = wormTiers
constants.WASP_WORM_VARIATIONS = wormVariations
constants.WASP_UNIT_TIERS = unitTiers
constants.WASP_UNIT_VARIATIONS = unitVariations

constants.POISON_NEST_TIERS = nestTiers
constants.POISON_NEST_VARIATIONS = nestVariations
constants.POISON_WORM_TIERS = wormTiers
constants.POISON_WORM_VARIATIONS = wormVariations
constants.POISON_UNIT_TIERS = unitTiers
constants.POISON_UNIT_VARIATIONS = unitVariations

constants.DECAYING_NEST_TIERS = nestTiers
constants.DECAYING_NEST_VARIATIONS = nestVariations
constants.DECAYING_WORM_TIERS = wormTiers
constants.DECAYING_WORM_VARIATIONS = wormVariations
constants.DECAYING_UNIT_TIERS = unitTiers
constants.DECAYING_UNIT_VARIATIONS = unitVariations

constants.UNDYING_NEST_TIERS = nestTiers
constants.UNDYING_NEST_VARIATIONS = nestVariations
constants.UNDYING_WORM_TIERS = wormTiers
constants.UNDYING_WORM_VARIATIONS = wormVariations
constants.UNDYING_UNIT_TIERS = unitTiers
constants.UNDYING_UNIT_VARIATIONS = unitVariations

constants.ELECTRIC_NEST_TIERS = nestTiers
constants.ELECTRIC_NEST_VARIATIONS = nestVariations
constants.ELECTRIC_WORM_TIERS = wormTiers
constants.ELECTRIC_WORM_VARIATIONS = wormVariations
constants.ELECTRIC_UNIT_TIERS = unitTiers
constants.ELECTRIC_UNIT_VARIATIONS = unitVariations

constants.ENERGY_THIEF_NEST_TIERS = nestTiers
constants.ENERGY_THIEF_NEST_VARIATIONS = nestVariations
constants.ENERGY_THIEF_WORM_TIERS = wormTiers
constants.ENERGY_THIEF_WORM_VARIATIONS = wormVariations
constants.ENERGY_THIEF_UNIT_TIERS = unitTiers
constants.ENERGY_THIEF_UNIT_VARIATIONS = unitVariations

constants.LASER_NEST_TIERS = nestTiers
constants.LASER_NEST_VARIATIONS = nestVariations
constants.LASER_WORM_TIERS = wormTiers
constants.LASER_WORM_VARIATIONS = wormVariations
constants.LASER_UNIT_TIERS = unitTiers
constants.LASER_UNIT_VARIATIONS = unitVariations

constants.INFERNO_NEST_TIERS = nestTiers
constants.INFERNO_NEST_VARIATIONS = nestVariations
constants.INFERNO_WORM_TIERS = wormTiers
constants.INFERNO_WORM_VARIATIONS = wormVariations
constants.INFERNO_UNIT_TIERS = unitTiers
constants.INFERNO_UNIT_VARIATIONS = unitVariations

constants.NUCLEAR_NEST_TIERS = nestTiers
constants.NUCLEAR_NEST_VARIATIONS = nestVariations
constants.NUCLEAR_WORM_TIERS = wormTiers
constants.NUCLEAR_WORM_VARIATIONS = wormVariations
constants.NUCLEAR_UNIT_TIERS = unitTiers
constants.NUCLEAR_UNIT_VARIATIONS = unitVariations

constants.ENERGY_THIEF_NEST_TIERS = nestTiers
constants.ENERGY_THIEF_NEST_VARIATIONS = nestVariations
constants.ENERGY_THIEF_WORM_TIERS = wormTiers
constants.ENERGY_THIEF_WORM_VARIATIONS = wormVariations
constants.ENERGY_THIEF_UNIT_TIERS = unitTiers
constants.ENERGY_THIEF_UNIT_VARIATIONS = unitVariations

constants.POISON_NEST_TIERS = nestTiers
constants.POISON_NEST_VARIATIONS = nestVariations
constants.POISON_WORM_TIERS = wormTiers
constants.POISON_WORM_VARIATIONS = wormVariations
constants.POISON_UNIT_TIERS = unitTiers
constants.POISON_UNIT_VARIATIONS = unitVariations

constants.BUILDING_SPACE_LOOKUP = {}

for t = 1, 10 do
    local wormTier = t + 1
    for v = 1, 20 do
        constants.BUILDING_SPACE_LOOKUP["neutral-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["neutral-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["neutral-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"
        
        constants.BUILDING_SPACE_LOOKUP["acid-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["acid-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["acid-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["physical-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["physical-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["electric-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["electric-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["suicide-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["suicide-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["nuclear-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["nuclear-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["fire-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["fire-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["fire-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["inferno-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["inferno-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["troll-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["troll-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["troll-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["fast-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["fast-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["fast-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["laser-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["laser-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["laser-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["wasp-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["wasp-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["spawner-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["spawner-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["energy-thief-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["energy-thief-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

        constants.BUILDING_SPACE_LOOKUP["poison-biter-spawner-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. t .. "-nest-rampant"
        constants.BUILDING_SPACE_LOOKUP["poison-worm-v" .. v .. "-t" .. t .. "-rampant"] = "chunk-scanner-" .. wormTier .. "-nest-rampant"

    end
end

constantsG =  constants
return constants
