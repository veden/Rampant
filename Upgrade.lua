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

function upgrade.attempt(natives, setNewSurface, gameSurfaces)
    local starting = global.version
    if not global.version or global.version < 106 then
        global.version = 106
        game.forces.enemy.kill_all_units()
        natives.squads = {}
        natives.points = 0
        natives.state = constants.AI_STATE_AGGRESSIVE

        natives.safeEntities = {}

        natives.aiPointsScaler = settings.global["rampant-aiPointsScaler"].value
        natives.aiNocturnalMode = settings.global["rampant-permanentNocturnal"].value

        -- needs to be on inner logic tick loop interval
        natives.stateTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
        natives.temperamentTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)

        -- used to precompute some values per logic cycle
        natives.retreatThreshold = 0
        natives.rallyThreshold = 0
        natives.formSquadThreshold = 0
        natives.attackWaveSize = 0
        natives.attackWaveDeviation = 0
        natives.attackWaveUpperBound = 0
        natives.unitRefundAmount = 0
        natives.regroupIndex = 1
        natives.randomGenerator = game.create_random_generator(settings.startup["rampant-enemySeed"].value+1024)

        game.map_settings.path_finder.min_steps_to_check_path_find_termination = constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH

        natives.evolutionTableAlignment = {}
        natives.bases = {}
        natives.baseIndex = 1
        natives.baseIncrement = 0

        natives.kamikazeThreshold = 0
        natives.attackWaveLowerBound = 1

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

        natives.drainPylons = {}

        natives.building = {}
        natives.pendingAttack = {}

        natives.cleanBuildingIndex = 1
        natives.attackIndex = 1

        natives.enabledMigration = natives.expansion and settings.global["rampant-enableMigration"].value

        natives.enemyAlignmentLookup = {}

        natives.canAttackTick = 0

        game.map_settings.unit_group.min_group_radius = constants.UNIT_GROUP_MAX_RADIUS * 0.5
        game.map_settings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS

        game.map_settings.unit_group.member_disown_distance = constants.UNIT_GROUP_DISOWN_DISTANCE
        game.map_settings.unit_group.tick_tolerance_when_member_arrives = constants.UNIT_GROUP_TICK_TOLERANCE

        game.map_settings.unit_group.max_member_speedup_when_behind = constants.UNIT_GROUP_MAX_SPEED_UP
        game.map_settings.unit_group.max_member_slowdown_when_ahead = constants.UNIT_GROUP_MAX_SLOWDOWN
        game.map_settings.unit_group.max_group_slowdown_factor = constants.UNIT_GROUP_SLOWDOWN_FACTOR

        game.map_settings.max_failed_behavior_count = 3

        natives.ENEMY_VARIATIONS = settings.startup["rampant-newEnemyVariations"].value

        natives.evolutionLevel = game.forces.enemy.evolution_factor

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

        natives.pendingStealGroups = {}
        natives.pendingStealGroups.len = 1
    end
    if (global.version < 110) then
        global.version = 110

        local gameSurfs
        if not gameSurfaces then
            gameSurfs = {}
            global.gameSurfaces = gameSurfs
            if natives.activeSurface == 1 then
                natives.activeSurface = "nauvis"
            end
        else
            gameSurfs = gameSurfaces
        end

        for _,player in pairs(game.connected_players) do
            if player and player.valid and not settings.get_player_settings(player)["rampant-suppress-surface-change-warnings"].value then
                local surface = player.surface
                if (natives.activeSurface ~= surface.name) then
                    local playerName = player.name
                    local surfaceName = surface.name
                    local playerSurfaces = gameSurfs[surfaceName]
                    if not playerSurfaces then
                        playerSurfaces = {}
                        gameSurfs[surfaceName] = playerSurfaces
                        player.print({"description.rampant-change-surface", surfaceName, natives.activeSurface})
                        playerSurfaces[playerName] = true
                    elseif not playerSurfaces[playerName] then
                        player.print({"description.rampant-change-surface", surfaceName, natives.activeSurface})
                        playerSurfaces[playerName] = true
                    end
                end
            end
        end

        for i=1,natives.squads.len do
            natives.squads[i].chunk = -1
        end

        for i=1,natives.pendingAttack.len do
            natives.pendingAttack[i].chunk = -1
        end

        for i=1,#natives.building do
            natives.building[i].chunk = -1
        end

        if not setNewSurface then
            game.get_surface(natives.activeSurface).print("Rampant - Version 0.18.11")
        end
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
