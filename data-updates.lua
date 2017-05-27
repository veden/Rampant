local vanillaUpdates = require("prototypes/enemies/UpdatesVanilla")
local bobsUpdates = require("prototypes/enemies/UpdatesBobs")
local NEUpdates = require("prototypes/enemies/UpdatesNE")
local constants = require("libs/Constants")

local function bobsDetected()
    return data.raw["turret"]["bob-big-explosive-worm-turret"] ~= nil
end

local function NEDetected()
    return data.raw["unit"]["medium-spitter-Mk3"] ~= nil
end

if settings.startup["rampant-useDumbProjectiles"].value then
    vanillaUpdates.useDumbProjectiles()
    if bobsDetected() then
    	require("prototypes/enemies/AttackBobs")
    	bobsUpdates.useDumbProjectiles()
    end
    if NEDetected() then
    	require("prototypes/enemies/AttackNE")
    	NEUpdates.useDumbProjectiles()
    	if settings.startup["rampant-useNEUnitLaunchers"].value then
    	    NEUpdates.useNEUnitLaunchers()
    	end
    end
end

--[[
    try to make sure new maps use the correct map settings without having to completely load the mod.
    done because seeing desync issues with dynamic map-settings changes before re-saving the map.
--]]
local mapSettings = data.raw["map-settings"]["map-settings"]

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



