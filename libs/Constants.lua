local constants = {}

-- versions

constants.VERSION_5 = 5
constants.VERSION_9 = 9
constants.VERSION_10 = 10
constants.VERSION_11 = 11
constants.VERSION_12 = 12
constants.VERSION_13 = 13
constants.VERSION_14 = 14

-- misc

constants.MAGIC_MAXIMUM_NUMBER = 1e99 -- used in loops trying to find the lowest/highest score
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL = 10000

constants.PROCESS_QUEUE_SIZE = 500
constants.SCAN_QUEUE_SIZE = 8
constants.PROCESS_PLAYER_BOUND = 4

constants.TICKS_A_SECOND = 60
constants.TICKS_A_MINUTE = constants.TICKS_A_SECOND * 60

constants.INTERVAL_PROCESS = 20
constants.INTERVAL_LOGIC = 40

-- ai

constants.AI_POINT_GENERATOR_AMOUNT = 6
constants.AI_SCOUT_COST = 45
constants.AI_SQUAD_COST = 175
constants.AI_VENGENCE_SQUAD_COST = 45
constants.AI_SETTLER_COST = 75
constants.AI_BASE_BUILDING_COST = 500
constants.AI_TUNNEL_COST = 100
constants.AI_MAX_POINTS = 10000

constants.AI_MAX_SQUAD_COUNT = 40
constants.AI_MAX_BITER_GROUP_SIZE = 600

constants.AI_STATE_PEACEFUL = 1
constants.AI_STATE_AGGRESSIVE = 2

constants.AI_MIN_STATE_DURATION = 1
constants.AI_MAX_STATE_DURATION = 4
constants.AI_MIN_TEMPERAMENT_DURATION = 5
constants.AI_MAX_TEMPERAMENT_DURATION = 15

-- ai retreat

constants.NO_RETREAT_BASE_PERCENT = 0.10
constants.NO_RETREAT_EVOLUTION_BONUS_MAX = 0.25
constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX = 0.40

-- chunk properties

constants.CHUNK_SIZE = 32
constants.HALF_CHUNK_SIZE = constants.CHUNK_SIZE / 2
constants.QUARTER_CHUNK_SIZE = constants.HALF_CHUNK_SIZE / 2
constants.NORTH_SOUTH = 1
constants.EAST_WEST = 2

-- pheromone amounts

constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = 500
constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT = 30
constants.DEATH_PHEROMONE_GENERATOR_AMOUNT = 75
constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT = 100

-- pheromone diffusion amounts

constants.STANDARD_PHERONOME_DIFFUSION_AMOUNT = 0.1
constants.MOVEMENT_PHEROMONE_DIFFUSION_AMOUNT = 0.02

constants.MOVEMENT_PHEROMONE_PERSISTANCE = 0.98
constants.REDUCED_MOVEMENT_PHEROMONE_PERSISTANCE = 0.65
constants.STANDARD_PHEROMONE_PERSISTANCE = 0.98

-- chunk attributes

constants.MOVEMENT_PHEROMONE = 1
constants.BASE_PHEROMONE = 2
constants.PLAYER_PHEROMONE = 3

constants.ENEMY_BASE_GENERATOR = 4
constants.PLAYER_BASE_GENERATOR = 5

constants.NORTH_SOUTH_PASSABLE = 6
constants.EAST_WEST_PASSABLE = 7

constants.CHUNK_TICK = 8

-- Squad status

constants.SQUAD_RETREATING = 1 -- used during squad retreat
constants.SQUAD_GUARDING = 2 -- used when squad is idle
constants.SQUAD_BURROWING = 3
constants.SQUAD_RAIDING = 4 -- used when player stuff is close

-- Squad Related

constants.RETREAT_GRAB_RADIUS = 24

constants.BASE_RALLY_CHANCE = 0.01
constants.BONUS_RALLY_CHANCE = 0.01

constants.MAX_RETREATS = 7

constants.MAX_RALLY_CRIES = 2
constants.RALLY_CRY_DISTANCE = 3

constants.GROUP_MERGE_DISTANCE = 28
 
-- player building pheromones

constants.BUILDING_PHEROMONES = {}
constants.BUILDING_PHEROMONES["generator"] = 12
constants.BUILDING_PHEROMONES["pump"] = 5
constants.BUILDING_PHEROMONES["offshore-pump"] = 5
constants.BUILDING_PHEROMONES["transport-belt"] = 2
constants.BUILDING_PHEROMONES["accumulator"] = 14
constants.BUILDING_PHEROMONES["solar-panel"] = 12
constants.BUILDING_PHEROMONES["boiler"] = 16
constants.BUILDING_PHEROMONES["assembling-machine"] = 16
constants.BUILDING_PHEROMONES["roboport"] = 14
constants.BUILDING_PHEROMONES["beacon"] = 14
constants.BUILDING_PHEROMONES["furnace"] = 16
constants.BUILDING_PHEROMONES["mining-drill"] = 19

-- player defense pheromones

constants.BUILDING_PHEROMONES["ammo-turret"] = 5
constants.BUILDING_PHEROMONES["wall"] = 0.55
constants.BUILDING_PHEROMONES["electric-turret"] = 7
constants.BUILDING_PHEROMONES["fluid-turret"] = 9
constants.BUILDING_PHEROMONES["turret"] = 5

constants.retreatFilter = {}
constants.retreatFilter[constants.SQUAD_RETREATING] = true

return constants

--[[ types
    inserter
    loader

    offshore-pump
    accumulator
    power-switch
    generator
    pump
    boiler
    solar-panel

    constant-combinator
    arithmetic-combinator
    decider-combinator

    player-port
    rocket-silo
    roboport
    assembling-machine
    mining-drill
    lab
    beacon
    radar
    furnace
    unit-spawner

    lamp

    land-mine
    ammo-turret
    wall
    gate
    electric-turret
    fluid-turret
    turret

    resource

    logistic-robot
    construction-robot
    unit
    player
    combat-robot

    locomotive
    cargo-wagon
    car

    smart-container
    logistic-container
    container
    storage-tank

    transport-belt
    underground-belt
    splitter
    pipe-to-ground
    electric-pole
    curved-rail
    straight-rail
    train-stop
    rail-signal
    rail-chain-signal
    pipe
]]--
