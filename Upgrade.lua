local upgrade = {}

-- imports

local constants = require("libs/Constants")
local mathUtils = require("libs/MathUtils")

-- constants

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PROCESS = constants.INTERVAL_PROCESS

local MAX_RALLY_CRIES = constants.MAX_RALLY_CRIES

-- imported functions

local roundToNearest = mathUtils.roundToNearest

-- module code

function upgrade.attempt(natives, regionMap)
    local starting = global.version
    if (global.version == nil) then

        -- removed in version 9
        -- regionMap.pQ = {{}} -- processing queue
        -- regionMap.pI = 1 -- insertion location for chunk processing
        -- regionMap.pP = 1 -- index for the chunk set to process
        -- regionMap.pR = -1 -- current processing roll
        natives.squads = {}
        natives.scouts = {}
        natives.tunnels = {}
        natives.points = 0

        global.version = constants.VERSION_5
    end
    if (global.version < constants.VERSION_9) then
	
        -- remove version 5 references
        regionMap.pQ = nil
        regionMap.pI = nil
        regionMap.pP = nil
        regionMap.pR = nil

    	global.version = constants.VERSION_9
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
	print(global.version)
	
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

	-- switched over to tick event
	regionMap.logicTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
	regionMap.processTick = roundToNearest(game.tick + INTERVAL_PROCESS, INTERVAL_PROCESS)
	-- needs to be on inner logic tick loop interval
	natives.stateTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
	natives.temperamentTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)

	--[[
	    For making changes to maps that haven't had Rampant loaded and aren't starting from a brand new map
	    Was causing desyncs when client connected before having the below settings saved into the map
	--]]
	local mapSettings = game.map_settings
	
	mapSettings.path_finder.short_request_ratio = constants.PATH_FINDER_SHORT_REQUEST_RATIO
	mapSettings.path_finder.short_cache_size = constants.PATH_FINDER_SHORT_CACHE_SIZE
	mapSettings.path_finder.long_cache_size = constants.PATH_FINDER_LONG_REQUEST_RATIO
	mapSettings.path_finder.min_steps_to_check_path_find_termination = constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH

	mapSettings.max_failed_behavior_count = constants.MAX_FAILED_BEHAVIORS

	mapSettings.unit_group.member_disown_distance = constants.UNIT_GROUP_DISOWN_DISTANCE
	mapSettings.unit_group.tick_tolerance_when_member_arrives = constants.UNIT_GROUP_TICK_TOLERANCE

	mapSettings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS
	mapSettings.unit_group.max_member_speedup_when_behind = constants.UNIT_GROUP_MAX_SPEED_UP
	mapSettings.unit_group.max_member_slowdown_when_ahead = constants.UNIT_GROUP_MAX_SLOWDOWN
	mapSettings.unit_group.max_group_slowdown_factor = constants.UNIT_GROUP_SLOWDOWN_FACTOR

	game.surfaces[1].print("Rampant - Version 0.15.10")
	global.version = constants.VERSION_22
    end
    return starting ~= global.version
end

return upgrade
