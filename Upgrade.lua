local upgrade = {}

-- imports

local constants = require("libs/Constants")
local mathUtils = require("libs/MathUtils")

-- constants

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PROCESS = constants.INTERVAL_PROCESS

-- imported functions

local roundToNearest = mathUtils.roundToNearest

-- module code

function upgrade.attempt(natives, world)
    local starting = global.version
    if (global.version == nil) then
        natives.squads = {}
        natives.scouts = {}
        natives.tunnels = {}
        natives.points = 0

        global.version = constants.VERSION_5
    end
    if (global.version < constants.VERSION_10) then
    	for _,squad in pairs(natives.squads) do
    	    squad.frenzy = false
    	    squad.frenzyPosition = {x=0,y=0}
    	    squad.rabid = false
    	end
	
    	global.version = constants.VERSION_10
    end
    if (global.version < constants.VERSION_11) then
    	natives.state = constants.AI_STATE_AGGRESSIVE
    	natives.temperament = 0
	
    	global.version = constants.VERSION_11
    end
    if (global.version < constants.VERSION_12) then
    	for _,squad in pairs(natives.squads) do
    	    squad.status = constants.SQUAD_GUARDING
    	    squad.kamikaze = false
    	end
	
    	-- reset ai build points due to error in earning points
    	natives.points = 0
	
    	global.version = constants.VERSION_12
    end
    if (global.version < constants.VERSION_16) then

    	natives.lastShakeMessage = 0
    	--remove version 14 retreat limit, it has been made redundant
    	natives.retreats = nil
	
    	game.surfaces[1].print("Rampant - Version 0.14.13")
    	global.version = constants.VERSION_16
    end
    if (global.version < constants.VERSION_18) then
	
	natives.safeEntities = {}
	natives.safeEntityName = {}

	game.surfaces[1].print("Rampant - Version 0.15.5")
	global.version = constants.VERSION_18
    end
    if (global.version < constants.VERSION_20) then
	
	natives.aiPointsScaler = settings.global["rampant-aiPointsScaler"].value
	natives.aiNocturnalMode = settings.global["rampant-permanentNocturnal"].value
	
	game.surfaces[1].print("Rampant - Version 0.15.8")
	global.version = constants.VERSION_20
    end
    if (global.version < constants.VERSION_22) then

	-- been made redundant
	natives.rallyCries = nil

	-- needs to be on inner logic tick loop interval
	natives.stateTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
	natives.temperamentTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)

	--[[
	    For making changes to maps that haven't had Rampant loaded and aren't starting from a brand new map
	    Was causing desyncs when client connected before having the below settings saved into the map 0.15.15 factorio
	--]]
	game.map_settings.path_finder.short_request_ratio = constants.PATH_FINDER_SHORT_REQUEST_RATIO
	game.map_settings.path_finder.short_cache_size = constants.PATH_FINDER_SHORT_CACHE_SIZE
	game.map_settings.path_finder.long_cache_size = constants.PATH_FINDER_LONG_REQUEST_RATIO

	game.map_settings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS
	game.map_settings.unit_group.max_member_speedup_when_behind = constants.UNIT_GROUP_MAX_SPEED_UP
	game.map_settings.unit_group.max_member_slowdown_when_ahead = constants.UNIT_GROUP_MAX_SLOWDOWN
	game.map_settings.unit_group.max_group_slowdown_factor = constants.UNIT_GROUP_SLOWDOWN_FACTOR

	game.surfaces[1].print("Rampant - Version 0.15.10")
	global.version = constants.VERSION_22
    end
    if (global.version < constants.VERSION_23) then

	-- used to precompute some values per logic cycle
	natives.retreatThreshold = 0
	natives.maxSquads = 0
	natives.rallyThreshold = 0
	natives.formSquadThreshold = 0
	natives.attackWaveSize = 0
	natives.attackWaveDeviation = 0
	natives.attackWaveLowerBound = 0
	natives.attackWaveUpperBound = 0
	natives.unitRefundAmount = 0
	natives.attackWaveThreshold = 0

	game.map_settings.unit_group.member_disown_distance = constants.UNIT_GROUP_DISOWN_DISTANCE
	game.map_settings.unit_group.tick_tolerance_when_member_arrives = constants.UNIT_GROUP_TICK_TOLERANCE
	
	-- used for breaking up how many squads are processing per logic cycle
	natives.regroupIndex = 1

	natives.bases = {}
	natives.baseDistanceMin = 0
	natives.baseIndex = 1
	natives.randomGenerator = game.create_random_generator()

	game.surfaces[1].print("Rampant - Version 0.15.11")
	global.version = constants.VERSION_23
    end
    if (global.version < constants.VERSION_25) then

	game.map_settings.path_finder.min_steps_to_check_path_find_termination = constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH
	
	game.surfaces[1].print("Rampant - Version 0.15.15")
	global.version = constants.VERSION_25
    end
    if (global.version < constants.VERSION_26) then

	game.map_settings.max_failed_behavior_count = constants.MAX_FAILED_BEHAVIORS
	
	game.surfaces[1].print("Rampant - Version 0.15.16")
	global.version = constants.VERSION_26
    end
    if (global.version < constants.VERSION_27) then

	natives.useCustomAI = constants.DEV_CUSTOM_AI
	-- natives.useCustomAI = settings.startup["rampant-useCustomAI"].value
	if natives.useCustomAI then
	    game.forces.enemy.ai_controllable = false
	else
	    game.forces.enemy.ai_controllable = true
	end	
	
	game.surfaces[1].print("Rampant - Version 0.15.17")
	global.version = constants.VERSION_27
    end
    if (global.version < constants.VERSION_28) then

	if (world == nil) then
	    global.world = {}
	    world = global.world
	end

	world.itemCollectorLookup = {}
	world.itemCollectorEvents = {}
	
	game.surfaces[1].print("Rampant - Version 0.15.18")
	global.version = constants.VERSION_28
    end
    if (global.version < constants.VERSION_29) then

	game.surfaces[1].print("Rampant - Version 0.15.19")
	global.version = constants.VERSION_29
    end
    return starting ~= global.version, natives, world
end

function upgrade.compareTable(entities, option, new)
    local changed = false
    if (entities[option] ~= new) then
	entities[option] = new
	changed = true
    end
    return changed, new
end

return upgrade
