-- Generate complete settings mock for testing

local settings = {
    startup = {},
    global = {}
}

-- Basic settings
settings.startup["rampant--enemySeed"] = { value = 12345 }
settings.startup["rampant--newEnemyVariations"] = { value = 1 }
settings.startup["rampant--max-evo-dist"] = { value = 100 }
settings.startup["rampant--baseAlignment"] = { value = 0.5 }
settings.startup["rampant--baseAlignmentChance"] = { value = 0.1 }
settings.startup["rampant--baseMutation"] = { value = 0.01 }
settings.startup["rampant--baseMutationChance"] = { value = 0.05 }
settings.startup["rampant--baseMutationRate"] = { value = 0.1 }
settings.startup["rampant--baseMutationRate2"] = { value = 0.15 }
settings.startup["rampant--baseMutationRate3"] = { value = 0.2 }
settings.startup["rampant--baseMutationRate4"] = { value = 0.25 }
settings.startup["rampant--baseMutationRate5"] = { value = 0.3 }
settings.startup["rampant--baseMutationRate6"] = { value = 0.35 }
settings.startup["rampant--baseMutationRate7"] = { value = 0.4 }
settings.startup["rampant--baseMutationRate8"] = { value = 0.45 }
settings.startup["rampant--baseMutationRate9"] = { value = 0.5 }
settings.startup["rampant--baseMutationRate10"] = { value = 0.55 }
settings.startup["rampant--tierStart"] = { value = 0.1 }
settings.startup["rampant--tierEnd"] = { value = 1.0 }

-- Faction tint settings (string settings)
local factions = {"acid", "electric", "energyThief", "fast", "fire", "inferno", "laser", "neutral", "nuclear", "physical", "poison", "spawner", "suicide", "troll", "wasp"}
for _, faction in ipairs(factions) do
    settings.startup["rampant--" .. faction .. "Enemy"] = { value = faction }
    settings.startup["rampant--" .. faction .. "Tints"] = { value = "ffffffff ffffffff ffffffff" }
end

-- Output as Lua code
print("-- Auto-generated settings mock")
print("return " .. require("inspect")(settings))
