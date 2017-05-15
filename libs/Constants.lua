local constants = {}

-- versions

constants.VERSION_5 = 5
constants.VERSION_9 = 9
constants.VERSION_10 = 10
constants.VERSION_11 = 11
constants.VERSION_12 = 12
constants.VERSION_13 = 13
constants.VERSION_14 = 14
constants.VERSION_15 = 15
constants.VERSION_16 = 16
constants.VERSION_17 = 17
constants.VERSION_18 = 18
constants.VERSION_19 = 19
constants.VERSION_20 = 20
constants.VERSION_21 = 21

-- misc

constants.MAGIC_MAXIMUM_NUMBER = 1e99 -- used in loops trying to find the lowest/highest score
constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL = 10000

constants.PROCESS_QUEUE_SIZE = 500
constants.SCAN_QUEUE_SIZE = 6
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

constants.AI_UNIT_REFUND = 3

constants.AI_MAX_SQUAD_COUNT = 40
constants.AI_MAX_BITER_GROUP_SIZE = 450

constants.AI_STATE_PEACEFUL = 1
constants.AI_STATE_AGGRESSIVE = 2
constants.AI_STATE_NOCTURNAL = 3

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
constants.DEATH_PHEROMONE_GENERATOR_AMOUNT = 500
constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT = 150

constants.IMPASSABLE_TERRAIN_GENERATOR_AMOUNT = -30

-- pheromone diffusion amounts

constants.MOVEMENT_PHEROMONE_PERSISTANCE = 0.98
constants.BASE_PHEROMONE_PERSISTANCE = 0.99
constants.PLAYER_PHEROMONE_PERSISTANCE = 0.98

-- chunk attributes

constants.MOVEMENT_PHEROMONE = 1
constants.BASE_PHEROMONE = 2
constants.PLAYER_PHEROMONE = 3

constants.ENEMY_BASE_GENERATOR = 4
constants.PLAYER_BASE_GENERATOR = 5

constants.NORTH_SOUTH_PASSABLE = 6
constants.EAST_WEST_PASSABLE = 7

constants.CHUNK_TICK = 8
constants.RETREAT_TRIGGERED = 9

-- Squad status

constants.SQUAD_RETREATING = 1 -- used during squad retreat
constants.SQUAD_GUARDING = 2 -- used when squad is idle
constants.SQUAD_BURROWING = 3
constants.SQUAD_RAIDING = 4 -- used when player stuff is close

-- Squad Related

constants.RETREAT_GRAB_RADIUS = 24

constants.BASE_RALLY_CHANCE = 0.01
constants.BONUS_RALLY_CHANCE = 0.01

constants.MAX_RALLY_CRIES = 2
constants.RALLY_CRY_DISTANCE = 3

constants.GROUP_MERGE_DISTANCE = 28

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
constants.BUILDING_PHEROMONES["mining-drill"] = 15
constants.BUILDING_PHEROMONES["rocket-silo"] = 18
constants.BUILDING_PHEROMONES["lamp"] = 4
constants.BUILDING_PHEROMONES["radar"] = 10

-- player defense pheromones

constants.BUILDING_PHEROMONES["ammo-turret"] = 2.5
constants.BUILDING_PHEROMONES["wall"] = 0.25
constants.BUILDING_PHEROMONES["electric-turret"] = 7
constants.BUILDING_PHEROMONES["fluid-turret"] = 9
constants.BUILDING_PHEROMONES["turret"] = 2.5

constants.retreatFilter = {}
constants.retreatFilter[constants.SQUAD_RETREATING] = true

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
