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

-- misc

constants.MAGIC_MAXIMUM_NUMBER = 1e99 -- used in loops trying to find the lowest/highest score
constants.MAGIC_MAXIMUM_BASE_NUMBER = 100000000
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL = 10000

constants.PROCESS_QUEUE_SIZE = 450
constants.SCAN_QUEUE_SIZE = 6
constants.BASE_QUEUE_SIZE = 1
constants.SQUAD_QUEUE_SIZE = 5
constants.PROCESS_PLAYER_BOUND = 4

constants.TICKS_A_SECOND = 60
constants.TICKS_A_MINUTE = constants.TICKS_A_SECOND * 60

constants.INTERVAL_PROCESS = 19
constants.INTERVAL_LOGIC = 38

constants.PLAYER_PHEROMONE_MULTIPLER = 100

constants.DEV_CUSTOM_AI = true

-- chunk properties

constants.CHUNK_SIZE = 32
constants.DOUBLE_CHUNK_SIZE = constants.CHUNK_SIZE * 2
constants.TRIPLE_CHUNK_SIZE = constants.CHUNK_SIZE * 3
constants.HALF_CHUNK_SIZE = constants.CHUNK_SIZE / 2
constants.QUARTER_CHUNK_SIZE = constants.HALF_CHUNK_SIZE / 2

constants.CHUNK_IMPASSABLE = 0
constants.CHUNK_NORTH_SOUTH = 1
constants.CHUNK_EAST_WEST = 2
constants.CHUNK_ALL_DIRECTIONS = 3

-- ai

constants.AI_POINT_GENERATOR_AMOUNT = 6
constants.AI_SCOUT_COST = 45
constants.AI_SQUAD_COST = 175
constants.AI_NEST_COST = 10
constants.AI_WORM_COST = 2
constants.AI_VENGENCE_SQUAD_COST = 45
constants.AI_SETTLER_COST = 75
constants.AI_BASE_BUILDING_COST = 500
constants.AI_TUNNEL_COST = 100
constants.AI_MAX_POINTS = 10000
constants.AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_POINTS * 3

constants.AI_UNIT_REFUND = 3

constants.AI_MAX_SQUAD_COUNT = 30
constants.AI_MAX_BITER_GROUP_SIZE = 450

constants.AI_SQUAD_MERGE_THRESHOLD = constants.AI_MAX_BITER_GROUP_SIZE * 0.75

constants.AI_STATE_PEACEFUL = 1
constants.AI_STATE_AGGRESSIVE = 2
constants.AI_STATE_NOCTURNAL = 3

constants.AI_MIN_STATE_DURATION = 1
constants.AI_MAX_STATE_DURATION = 4
constants.AI_MIN_TEMPERAMENT_DURATION = 5
constants.AI_MAX_TEMPERAMENT_DURATION = 15

-- ai base

constants.BASE_DISTANCE_THRESHOLD = 15 * constants.CHUNK_SIZE

constants.BASE_ALIGNMENT_NEUTRAL = 1
constants.BASE_ALIGNMENT_FIRE = 2
constants.BASE_ALIGNMENT_BURROW = 3
constants.BASE_ALIGNMENT_SUICIDE = 4
constants.BASE_ALIGNMENT_INFEST = 5

-- ai retreat

constants.NO_RETREAT_BASE_PERCENT = 0.10
constants.NO_RETREAT_EVOLUTION_BONUS_MAX = 0.25
constants.NO_RETREAT_SQUAD_SIZE_BONUS_MAX = 0.40

-- pheromone amounts

constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = 500
constants.DEATH_PHEROMONE_GENERATOR_AMOUNT = 500
constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT = 150

constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT = -0.1

-- pheromone diffusion amounts

constants.MOVEMENT_PHEROMONE_PERSISTANCE = 0.9
constants.BASE_PHEROMONE_PERSISTANCE = 0.99
constants.PLAYER_PHEROMONE_PERSISTANCE = 0.98
constants.RESOURCE_PHEROMONE_PERSISTANCE = 0.85

-- chunk attributes

constants.MOVEMENT_PHEROMONE = 1
constants.BASE_PHEROMONE = 2
constants.PLAYER_PHEROMONE = 3
constants.RESOURCE_PHEROMONE = 4

constants.PLAYER_BASE_GENERATOR = 5
constants.RESOURCE_GENERATOR = 6

constants.PASSABLE = 7

constants.CHUNK_TICK = 8
constants.RETREAT_TRIGGERED = 9
constants.RALLY_TRIGGERED = 10
constants.NEST_BASE = 11
constants.WORM_BASE = 12
constants.NEST_COUNT = 13
constants.WORM_COUNT = 14
constants.PATH_RATING = 15

-- Squad status

constants.SQUAD_RETREATING = 1 -- used during squad retreat
constants.SQUAD_GUARDING = 2 -- used when squad is idle
constants.SQUAD_BURROWING = 3
constants.SQUAD_RAIDING = 4 -- used when player stuff is close

-- Squad Related

constants.RETREAT_GRAB_RADIUS = 24

constants.BASE_RALLY_CHANCE = 0.02
constants.BONUS_RALLY_CHANCE = 0.06

constants.RALLY_CRY_DISTANCE = 3

constants.GROUP_MERGE_DISTANCE = 28

constants.MAX_PENALTY_BEFORE_PURGE = 8000

-- player building pheromones

constants.BUILDING_PHEROMONES = {}
constants.BUILDING_PHEROMONES["generator"] = 8
constants.BUILDING_PHEROMONES["pump"] = 2
constants.BUILDING_PHEROMONES["reactor"] = 16
constants.BUILDING_PHEROMONES["offshore-pump"] = 2
constants.BUILDING_PHEROMONES["transport-belt"] = 1
constants.BUILDING_PHEROMONES["accumulator"] = 10
constants.BUILDING_PHEROMONES["solar-panel"] = 8
constants.BUILDING_PHEROMONES["boiler"] = 12
constants.BUILDING_PHEROMONES["assembling-machine"] = 10
constants.BUILDING_PHEROMONES["roboport"] = 10
constants.BUILDING_PHEROMONES["beacon"] = 10
constants.BUILDING_PHEROMONES["furnace"] = 12
constants.BUILDING_PHEROMONES["programmable-speaker"] = 8
constants.BUILDING_PHEROMONES["mining-drill"] = 35
constants.BUILDING_PHEROMONES["rocket-silo"] = 18
constants.BUILDING_PHEROMONES["lamp"] = 4
constants.BUILDING_PHEROMONES["radar"] = 20

-- player defense pheromones

constants.BUILDING_PHEROMONES["ammo-turret"] = 10
constants.BUILDING_PHEROMONES["wall"] = 0.5
constants.BUILDING_PHEROMONES["electric-turret"] = 20
constants.BUILDING_PHEROMONES["fluid-turret"] = 28
constants.BUILDING_PHEROMONES["turret"] = 10

constants.retreatFilter = {}
constants.retreatFilter[constants.SQUAD_RETREATING] = true

-- map settings tweaks

constants.PATH_FINDER_SHORT_REQUEST_RATIO = 0.8
constants.PATH_FINDER_SHORT_CACHE_SIZE = 25
constants.PATH_FINDER_LONG_REQUEST_RATIO = 5
constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH = 100

constants.MAX_FAILED_BEHAVIORS = 6

constants.UNIT_GROUP_DISOWN_DISTANCE = 10
constants.UNIT_GROUP_TICK_TOLERANCE = 360

constants.UNIT_GROUP_MAX_RADIUS = 20
constants.UNIT_GROUP_MAX_SPEED_UP = 1.1
constants.UNIT_GROUP_MAX_SLOWDOWN = 1.0
constants.UNIT_GROUP_SLOWDOWN_FACTOR = 0.9

return constants

--[[


    player
    furnace
    transport-belt
    fish
    boiler
    container
    electric-pole
    generator
    offshore-pump
    inserter
    pipe
    radar
    lamp
    pipe-to-ground
    assembling-machine
    wall
    mining-drill
    projectile
    resource
    turret
    ammo-turret
    unit
    unit-spawner
    tree
    smart-container
    underground-belt
    loader
    splitter
    car
    solar-panel
    locomotive
    cargo-wagon
    fluid-wagon
    gate
    player-port
    straight-rail
    curved-rail
    land-mine
    train-stop
    rail-signal
    rail-chain-signal
    lab
    logistic-robot
    construction-robot
    logistic-container
    rocket-silo
    roboport
    storage-tank
    pump
    market
    accumulator
    beacon
    combat-robot
    arithmetic-combinator
    decider-combinator
    constant-combinator
    power-switch
    programmable-speaker
    electric-energy-interface
    reactor
    heat-pipe

    electric-turret
    fluid-turret

    night-vision-equipment
    energy-shield-equipment
    battery-equipment
    solar-panel-equipment
    generator-equipment
    active-defense-equipment
    movement-bonus-equipment
    roboport-equipment
    belt-immunity-equipment

    tutorial

    rocket-defense

    item-with-entity-data
    rail-planner
    tool
    blueprint
    deconstruction-item
    blueprint-book
    selection-tool
    item-with-tags
    module
    technology

    custom-input
    sprite
    font
    noise-layer
    gui-style
    utility-constants
    utility-sounds
    utility-sprites
    ambient-sound
    character-corpse
    explosion
    smoke
    item-entity
    arrow
    flying-text
    corpse
    entity-ghost
    tile-ghost
    deconstructible-tile-proxy
    item-request-proxy
    particle
    particle-source
    leaf-particle
    rail-remnants
    simple-entity
    decorative
    ammo
    armor
    gun
    item
    capsule
    repair-tool
    mining-tool
    item-group
    item-subgroup
    recipe
    fluid
    virtual-signal
    autoplace-control
    map-settings
    map-gen-presets
    tile
    optimized-decorative
    simple-entity-with-force
    simple-entity-with-owner
    beam
    fire
    stream
    damage-type
    ammo-category
    rail-category
    fuel-category
    recipe-category
    resource-category
    module-category
    equipment-grid
    equipment-category
    build-entity-achievement
    research-achievement
    finish-the-game-achievement
    group-attack-achievement
    construct-with-robots-achievement
    deconstruct-with-robots-achievement
    deliver-by-robots-achievement
    train-path-achievement
    player-damaged-achievement
    produce-achievement
    produce-per-hour-achievement
    dont-use-entity-in-energy-production-achievement
    kill-achievement
    combat-robot-count
    dont-craft-manually-achievement
    dont-build-entity-achievement
    achievement
    rocket-silo-rocket
    rocket-silo-rocket-shadow
    smoke-with-trigger
    sticker

--]]--
