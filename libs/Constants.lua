local constants = {}

constants.MAGIC_MAXIMUM_NUMBER = 1e99 -- used in loops trying to find the lowest score

constants.MAX_PHEROMONE = 7000
constants.BASE_PHEROMONE_PRODUCTION = 35
constants.DIFFUSION_AMOUNT = 0.02
constants.DEATH_DIFFUSION_AMOUNT = 0.005
constants.CHUNK_SIZE = 32
constants.HALF_CHUNK_SIZE = constants.CHUNK_SIZE / 2
constants.NORTH_SOUTH = true
constants.EAST_WEST = false

constants.DEATH_PHEROMONE = 1
constants.ENEMY_BASE_PHEROMONE = 2
constants.PLAYER_PHEROMONE = 3

constants.SQUAD_RETREATING = 1
constants.SQUAD_GUARDING = 2
constants.SQUAD_SIEGE = 3
constants.SQUAD_HUNTING = 4
constants.SQUAD_SUICIDE = 5
constants.SQUAD_ATTACKING = 6
constants.SQUAD_BURROWING = 7
constants.SQUAD_SCOUTING = 8

return constants