local upgrade = {}

-- imports

local constants = require("libs/Constants")
local mathUtils = require("libs/MathUtils")

-- constants

local CONVERSION_TABLE = constants.CONVERSION_TABLE

local BASE_AI_STATE_DORMANT = constants.BASE_AI_STATE_DORMANT

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local CHUNK_SIZE = constants.CHUNK_SIZE

local ATTACK_SCORE = constants.ATTACK_SCORE

local SQUAD_GUARDING = constants.SQUAD_GUARDING

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

-- imported functions

local roundToNearest = mathUtils.roundToNearest

-- module code

function upgrade.attempt(natives, setNewSurface)
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
        -- natives.temperament = 0

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

        global.version = constants.VERSION_16
    end
    if (global.version < constants.VERSION_18) then

        natives.safeEntities = {}
        natives.safeEntityName = {}

        global.version = constants.VERSION_18
    end
    if (global.version < constants.VERSION_20) then

        natives.aiPointsScaler = settings.global["rampant-aiPointsScaler"].value
        natives.aiNocturnalMode = settings.global["rampant-permanentNocturnal"].value

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

        -- game.map_settings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS

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

        -- game.map_settings.unit_group.member_disown_distance = constants.UNIT_GROUP_DISOWN_DISTANCE
        -- game.map_settings.unit_group.tick_tolerance_when_member_arrives = constants.UNIT_GROUP_TICK_TOLERANCE

        -- used for breaking up how many squads are processing per logic cycle
        natives.regroupIndex = 1

        natives.randomGenerator = game.create_random_generator(settings.startup["rampant-enemySeed"].value+1024)

        global.version = constants.VERSION_23
    end
    if (global.version < constants.VERSION_25) then

        game.map_settings.path_finder.min_steps_to_check_path_find_termination = constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH

        global.version = constants.VERSION_25
    end
    if (global.version < constants.VERSION_33) then

        global.world = nil

        global.version = constants.VERSION_33
    end
    if (global.version < constants.VERSION_38) then

        for _,squad in pairs(natives.squads) do
            squad.chunk = nil
        end

        global.regionMap = nil

        global.version = constants.VERSION_38
    end
    if (global.version < constants.VERSION_41) then

        natives.evolutionTableAlignment = {}
        natives.bases = {}
        natives.baseIndex = 1
        natives.baseIncrement = 0

        global.version = constants.VERSION_41
    end
    if (global.version < constants.VERSION_44) then

        natives.kamikazeThreshold = 0

        global.version = constants.VERSION_44
    end
    if (global.version < constants.VERSION_51) then

        natives.scouts = nil
        natives.tunnels = nil
        natives.baseLookup = nil

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

        global.version = constants.VERSION_72
    end
    if (global.version < constants.VERSION_75) then

        for _,squad in pairs(natives.squads) do
            squad.attackScoreFunction = ATTACK_SCORE
        end

        global.version = constants.VERSION_75
    end
    if (global.version < constants.VERSION_76) then

        natives.drainPylons = {}

        global.version = constants.VERSION_76
    end
    if (global.version < constants.VERSION_77) then

        natives.attackWaveThreshold = nil
        natives.attackWav = nil

        global.version = constants.VERSION_77
    end
    if (global.version < constants.VERSION_85) then

        natives.building = {}
        natives.pendingAttack = {}

        natives.cleanBuildingIndex = 1
        natives.attackIndex = 1

        global.version = constants.VERSION_85
    end
    if (global.version < constants.VERSION_86) then

        natives.expansion = game.map_settings.enemy_expansion.enabled
        natives.enabledMigration = natives.expansion and settings.global["rampant-enableMigration"].value

        global.version = constants.VERSION_86
    end
    if (global.version < constants.VERSION_87) then

        natives.enemyAlignmentLookup = {}

        global.version = constants.VERSION_87
    end
    if (global.version < 89) then

        natives.canAttackTick = 0

        global.version = 89
    end
    if (global.version < 95) then

        natives.randomGenerator = game.create_random_generator(settings.startup["rampant-enemySeed"].value+1024)

        global.version = 95
    end
    if (global.version < 100) then

        game.map_settings.unit_group.min_group_radius = constants.UNIT_GROUP_MAX_RADIUS * 0.5
        game.map_settings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS

        game.map_settings.unit_group.member_disown_distance = constants.UNIT_GROUP_DISOWN_DISTANCE
        game.map_settings.unit_group.tick_tolerance_when_member_arrives = constants.UNIT_GROUP_TICK_TOLERANCE

        game.map_settings.unit_group.max_member_speedup_when_behind = constants.UNIT_GROUP_MAX_SPEED_UP
        game.map_settings.unit_group.max_member_slowdown_when_ahead = constants.UNIT_GROUP_MAX_SLOWDOWN
        game.map_settings.unit_group.max_group_slowdown_factor = constants.UNIT_GROUP_SLOWDOWN_FACTOR

        game.map_settings.max_failed_behavior_count = 3

        for i=#natives.squads,1,-1 do
            natives.squads[i].penalties = {}

            if not natives.squads[i].chunk then
                natives.squads[i].chunk = SENTINEL_IMPASSABLE_CHUNK
            end
        end

        for i=#natives.bases,1,-1 do
            local base = natives.bases[i]

            for x=1,#base.alignment do
                local c = CONVERSION_TABLE[base.alignment[x]]
                if (x == 1) and not c then
                    base.alignment = {}
                else
                    base.alignment[x] = c
                end
            end
        end

        natives.ENEMY_VARIATIONS = settings.startup["rampant-newEnemyVariations"].value

        natives.nextChunkSort = nil
        natives.nextChunkSortTick = nil

        natives.evolutionLevel = game.forces.enemy.evolution_factor

        natives.evolutionTableUnitSpawner = nil
        natives.evolutionTableWorm = nil

        natives.pendingAttack.len = #natives.pendingAttack
        natives.squads.len = #natives.squads

        natives.activeRaidNests = 0
        natives.activeNests = 0
        natives.destroyPlayerBuildings = 0
        natives.lostEnemyUnits = 0
        natives.lostEnemyBuilding = 0
        natives.rocketLaunched = 0
        natives.builtEnemyBuilding = 0
        natives.ionCannonBlasts = 0
        natives.artilleryBlasts = 0

        natives.temperament = 0
        natives.temperamentScore = 0
        natives.stateTick = 0

        global.version = 100
    end
    if (global.version < 101) then

        local subTypes = constants.HIVE_BUILDINGS_TYPES

        local acc = {}

        for t=1,11 do
            for si=1,#subTypes do
                local st = subTypes[si]

                acc[#acc+1] = "entity-proxy-" .. st .. "-t" .. t .. "-rampant"
            end
        end

        local es = game.surfaces[natives.activeSurface].find_entities_filtered({
                name=acc
                                                                              })

        for i=1,#es do
            es[i].die()
        end

        global.version = 101
    end
    if (global.version < 103) then
        if not setNewSurface then
            game.surfaces[natives.activeSurface].print("Rampant - Version 0.18.3")
        end
        global.version = 103
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
