local config = require("config")

local vanillaUpdates = require("prototypes/enemies/UpdatesVanilla")
local bobsUpdates = require("prototypes/enemies/UpdatesBobs")

local function bobsDetected()
    return data.raw["turret"]["bob-big-explosive-worm-turret"] ~= nil
end

if config.useDumbProjectiles then
    vanillaUpdates.useDumbProjectiles()
    if bobsDetected() then
	require("prototypes/enemies/AttackBobs")
	bobsUpdates.useDumbProjectiles()
    end
end




