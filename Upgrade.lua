local upgrade = {}

-- imports

local constants = require("libs/Constants")
local mathUtils = require("libs/MathUtils")

-- constants

local BASE_AI_STATE_DORMANT = constants.BASE_AI_STATE_DORMANT

local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local CHUNK_SIZE = constants.CHUNK_SIZE

local ATTACK_SCORE = constants.ATTACK_SCORE

local SQUAD_GUARDING = constants.SQUAD_GUARDING

-- imported functions

local roundToNearest = mathUtils.roundToNearest

-- module code

function upgrade.attempt(natives)
    local starting = global.version
    if (global.version == nil) then
        natives.squads = {}
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

    	game.surfaces[natives.activeSurface].print("Rampant - Version 0.14.13")
    	global.version = constants.VERSION_16
    end
    if (global.version < constants.VERSION_18) then

	natives.safeEntities = {}
	natives.safeEntityName = {}

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.5")
	global.version = constants.VERSION_18
    end
    if (global.version < constants.VERSION_20) then

	natives.aiPointsScaler = settings.global["rampant-aiPointsScaler"].value
	natives.aiNocturnalMode = settings.global["rampant-permanentNocturnal"].value

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.8")
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
	-- game.map_settings.path_finder.short_request_ratio = constants.PATH_FINDER_SHORT_REQUEST_RATIO
	-- game.map_settings.path_finder.short_cache_size = constants.PATH_FINDER_SHORT_CACHE_SIZE
	-- game.map_settings.path_finder.long_cache_size = constants.PATH_FINDER_LONG_REQUEST_RATIO

	game.map_settings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS
	game.map_settings.unit_group.max_member_speedup_when_behind = constants.UNIT_GROUP_MAX_SPEED_UP
	game.map_settings.unit_group.max_member_slowdown_when_ahead = constants.UNIT_GROUP_MAX_SLOWDOWN
	game.map_settings.unit_group.max_group_slowdown_factor = constants.UNIT_GROUP_SLOWDOWN_FACTOR

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.10")
	global.version = constants.VERSION_22
    end
    if (global.version < constants.VERSION_23) then

	-- used to precompute some values per logic cycle
	natives.retreatThreshold = 0
	-- natives.maxSquads = 0
	natives.rallyThreshold = 0
	natives.formSquadThreshold = 0
	natives.attackWaveSize = 0
	natives.attackWaveDeviation = 0
	natives.attackWaveUpperBound = 0
	natives.unitRefundAmount = 0
	-- natives.attackWaveThreshold = 0

	game.map_settings.unit_group.member_disown_distance = constants.UNIT_GROUP_DISOWN_DISTANCE
	game.map_settings.unit_group.tick_tolerance_when_member_arrives = constants.UNIT_GROUP_TICK_TOLERANCE

	-- used for breaking up how many squads are processing per logic cycle
	natives.regroupIndex = 1

	natives.randomGenerator = game.create_random_generator()

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.11")
	global.version = constants.VERSION_23
    end
    if (global.version < constants.VERSION_25) then

	game.map_settings.path_finder.min_steps_to_check_path_find_termination = constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.15")
	global.version = constants.VERSION_25
    end
    if (global.version < constants.VERSION_26) then

	game.map_settings.max_failed_behavior_count = constants.MAX_FAILED_BEHAVIORS

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.16")
	global.version = constants.VERSION_26
    end
    if (global.version < constants.VERSION_27) then

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.17")
	global.version = constants.VERSION_27
    end
    if (global.version < constants.VERSION_33) then

	global.world = nil

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.15.23")
	global.version = constants.VERSION_33
    end
    if (global.version < constants.VERSION_38) then

	for _,squad in pairs(natives.squads) do
    	    squad.chunk = nil
    	end

	global.regionMap = nil

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.3")
	global.version = constants.VERSION_38
    end
    if (global.version < constants.VERSION_41) then

	natives.evolutionTableUnitSpawner = {}
	natives.evolutionTableWorm = {}
	natives.evolutionTableAlignment = {}
	natives.bases = {}
	natives.baseIndex = 1
	natives.baseIncrement = 0

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.6")
	global.version = constants.VERSION_41
    end
    if (global.version < constants.VERSION_44) then

	natives.kamikazeThreshold = 0

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.9")
	global.version = constants.VERSION_44
    end
    if (global.version < constants.VERSION_51) then

	natives.scouts = nil
        natives.tunnels = nil
	natives.baseLookup = nil

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.16")
	global.version = constants.VERSION_51
    end
    if (global.version < constants.VERSION_57) then

	natives.attackWaveLowerBound = 1

	for _,squad in pairs(natives.squads) do
	    squad.maxDistance = 0
	    squad.originPosition = {
		x = 0,
		y = 0
	    }
	    squad.settlers = false
    	end

	natives.expansion = game.map_settings.enemy_expansion.enabled
	natives.expansionMaxDistance = game.map_settings.enemy_expansion.max_expansion_distance * CHUNK_SIZE
	natives.expansionMaxDistanceDerivation = natives.expansionMaxDistance * 0.33
	natives.expansionMinTime = game.map_settings.enemy_expansion.min_expansion_cooldown
	natives.expansionMaxTime = game.map_settings.enemy_expansion.max_expansion_cooldown
	natives.expansionMinSize = game.map_settings.enemy_expansion.settler_group_min_size
	natives.expansionMaxSize = game.map_settings.enemy_expansion.settler_group_max_size

	natives.settlerCooldown = 0
	natives.settlerWaveDeviation = 0
	natives.settlerWaveSize = 0

	game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.22")
	global.version = constants.VERSION_57
    end
    if (global.version < constants.VERSION_72) then

	for _,squad in pairs(natives.squads) do
	    squad.status = SQUAD_GUARDING
            squad.cycles = 0
    	end

        for _,base in pairs(natives.bases) do
            base.temperament = 0
            base.temperamentTick = 0
            base.state = BASE_AI_STATE_DORMANT
            base.stateTick = 0
            base.alignment = {base.alignment}
        end

        natives.nextChunkSort = 0
        natives.nextChunkSortTick = 0

        game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.37")
	global.version = constants.VERSION_72
    end
    if (global.version < constants.VERSION_75) then

        for _,squad in pairs(natives.squads) do
            squad.attackScoreFunction = ATTACK_SCORE
    	end

        game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.40")
	global.version = constants.VERSION_75
    end
    if (global.version < constants.VERSION_76) then

        natives.drainPylons = {}

        game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.41")
	global.version = constants.VERSION_76
    end
    if (global.version < constants.VERSION_77) then

        natives.attackWaveThreshold = nil
        natives.attackWav = nil

        game.surfaces[natives.activeSurface].print("Rampant - Version 0.16.42")
        global.version = constants.VERSION_77
    end
    if (global.version < constants.VERSION_85) then

        natives.building = {}
        natives.pendingAttack = {}

        natives.cleanBuildingIndex = 1
        natives.attackIndex = 1
        
        game.surfaces[natives.activeSurface].print("Rampant - Version 0.17.4")
        global.version = constants.VERSION_85
    end
    if (global.version < constants.VERSION_86) then

        natives.expansion = game.map_settings.enemy_expansion.enabled
        natives.enabledMigration = natives.expansion and settings.global["rampant-enableMigration"].value
        
        game.surfaces[natives.activeSurface].print("Rampant - Version 0.17.5")
        global.version = constants.VERSION_86
    end
    if (global.version < constants.VERSION_87) then

        natives.enemyAlignmentLookup = {}
        
        game.surfaces[natives.activeSurface].print("Rampant - Version 0.17.6")
        global.version = constants.VERSION_87
    end
    if (global.version < constants.VERSION_88) then
        
        game.surfaces[natives.activeSurface].print("Rampant - Version 0.17.18")
        global.version = constants.VERSION_88
    end
    if (global.version < 89) then

        natives.canAttackTick = 0
            
        game.surfaces[natives.activeSurface].print("Rampant - Version 0.17.22")
        global.version = 89
    end
    if (global.version < 92) then

        game.surfaces[natives.activeSurface].print("Rampant - Version 0.17.26")
        global.version = 92
    end
    
    return starting ~= global.version, natives
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
