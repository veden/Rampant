local constants = {}

-- misc

constants.MAGIC_MAXIMUM_NUMBER = 1e99 -- used in loops trying to find the lowest score
-- constants.MAX_PHEROMONE = 20000
constants.RETREAT_LEVEL_DEATH_PHEROMONE = 15000

-- ai

constants.AI_POINT_GENERATOR_AMOUNT = 10
constants.AI_SCOUT_COST = 45
constants.AI_SQUAD_COST = 150
constants.AI_SETTLER_COST = 75
constants.AI_BASE_BUILDING_COST = 500

constants.AI_MAX_SQUAD_SIZE = 150
constants.AI_MAX_SQUAD_COUNT = 30

-- chunk properties

constants.CHUNK_SIZE = 32
constants.HALF_CHUNK_SIZE = constants.CHUNK_SIZE / 2
constants.QUARTER_CHUNK_SIZE = constants.HALF_CHUNK_SIZE / 2
constants.NORTH_SOUTH = 1
constants.EAST_WEST = 2

-- pheromone amounts

constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = 500
constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT = 35
constants.DEATH_PHEROMONE_GENERATOR_AMOUNT = 35
constants.PLAYER_PHEROMONE_GENERATOR_AMOUNT = 75

-- pheromone diffusion amounts

constants.STANDARD_PHERONOME_DIFFUSION_AMOUNT = 0.03
constants.DEATH_PHEROMONE_DIFFUSION_AMOUNT = 0.00125

-- chunk attributes

constants.DEATH_PHEROMONE = 1
constants.ENEMY_BASE_PHEROMONE = 2
constants.PLAYER_PHEROMONE = 3
constants.PLAYER_BASE_PHEROMONE = 4
constants.PLAYER_DEFENSE_PHEROMONE = 5
constants.MOVEMENT_PHEROMONE = 6

constants.ENEMY_BASE_GENERATOR = 7
constants.PLAYER_BASE_GENERATOR = 8
constants.PLAYER_DEFENSE_GENERATOR = 9

constants.NORTH_SOUTH_PASSABLE = 10
constants.EAST_WEST_PASSABLE = 11

-- Squad status

constants.SQUAD_RETREATING = 1 -- used during squad retreat
constants.SQUAD_GUARDING = 2 -- used when squad is idle
constants.SQUAD_ATTACKING = 3 -- used as an attack state to be transitioned into hunt, raid, siege, burrow
-- constants.SQUAD_SIEGE = 3 
constants.SQUAD_HUNTING = 4 -- used when player is close to unit group
constants.SQUAD_SUICIDE_HUNT = 5 -- used when player is close with no retreat
-- constants.SQUAD_BURROWING = 6 
-- constants.SQUAD_SCOUTING = 7
constants.SQUAD_RAIDING = 8 -- used when player stuff is close
constants.SQUAD_SUICIDE_RAID = 9 -- when player stuff is close with no retreat

-- player building pheromones

constants.buildingPheromones = {}
-- constants.buildingPheromones["container"] = 1
-- constants.buildingPheromones["storage-tank"] = 1
constants.buildingPheromones["generator"] = 30
constants.buildingPheromones["pump"] = 4
constants.buildingPheromones["offshore-pump"] = 4
-- constants.buildingPheromones["constant-combinator"] = 1
-- constants.buildingPheromones["train-stop"] = 2
-- constants.buildingPheromones["rail-signal"] = 1
constants.buildingPheromones["electric-pole"] = 2
constants.buildingPheromones["transport-belt"] = 2
constants.buildingPheromones["accumulator"] = 20
constants.buildingPheromones["solar-panel"] = 16
constants.buildingPheromones["boiler"] = 30
constants.buildingPheromones["assembling-machine"] = 24
constants.buildingPheromones["roboport"] = 20
constants.buildingPheromones["beacon"] = 20
constants.buildingPheromones["furnace"] = 30
constants.buildingPheromones["mining-drill"] = 40

-- player defense pheromones

constants.defensePheromones = {}
constants.defensePheromones["ammo-turret"] = 5
constants.defensePheromones["electric-turret"] = 7.5
constants.defensePheromones["fluid-turret"] = 10
constants.defensePheromones["turret"] = 3

-- enemy units

-- constants.deathPheromones = {}
-- constants.deathPheromones[""] 

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