--[[
    Game Adapter - Factorio `game` global mock

    Implements a subset of Factorio's `game` global for simulation.
]]

local TickSystem = require("tests/sim.TickSystem")

-- Helper to create a force table
local function createForce(name)
    return {
        name = name,
        entities = {},
        stations = {},
        cargo_wagons = {},
        items = {},
        recipes = {},
        technologies = {},
        manuals = {},
        upgrades = {},
        turret_ammo = {},
        ammo_categories = {},
        character_crafting_speed_modifier = 0,
        character_mining_speed_modifier = 0,
        character_run_speed_modifier = 0,
        character_build_distance_bonus = 0,
        character_item_drop_distance_bonus = 0,
        character_reach_distance_bonus = 0,
        character_resource_reach_distance_bonus = 0,
        character_inventory_slots_bonus = 0,
        health_bonus = 0,
        worker_robot_speed_modifier = 0,
        worker_robot_storage_bonus = 0,
        ghost_time_to_live = nil,
        max_number_of_owned_rail_segments = nil,
        max_rail_junction_count = nil,
        time_to_live = nil,
        evolution_factor = 0,
        pollution_statistics = {
            input = {},
            output = {}
        },
        enemySpawningPool = {},
        players = {}
    }
end

local game = {
    forces = {
        enemy = createForce("enemy"),
        player = createForce("player"),
        neutral = createForce("neutral")
    },
    surfaces = {},
    players = {},
    connected_players = {}
}

-- Map settings (for evolution calculations)
game.map_settings = {
    enemy_expansion = {
        enemy_building_influence_radius = 0,
        friendly_base_influence_radius = 0,
    },
    unit_group = {
        max_group_gathering_time = 600,
    }
}

-- Tick property with GameState delegation
-- When GameState exists (tests), reads from GameState._tick
-- Otherwise reads from TickSystem (legacy support)
local tickGetter = function()
    local GameState = rawget(_G, "GameState")
    if GameState then
        return GameState._tick
    end
    return TickSystem.now()
end

local tickSetter = function(v)
    local GameState = rawget(_G, "GameState")
    if GameState then
        GameState._tick = v
    else
        TickSystem.set(v)
    end
end

setmetatable(game, {
    __index = function(_, k)
        if k == "tick" then
            return tickGetter()
        end
        return nil
    end,
    __newindex = function(_, k, v)
        if k == "tick" then
            tickSetter(v)
        end
    end
})

function game.get_tick()
    return tickGetter()
end

-- Server functions (mostly no-ops for simulation)
game.server_port = 0
game.maximum_compression_ratio = nil

function game.save_server_state()
    -- No-op for simulation
end

function game.write_file()
    -- No-op for simulation
end

function game.kill_entity(entity)
    entity.alive = false
    entity.health = 0
    if entity._surface then
        for i, e in ipairs(entity._surface.entities) do
            if e == entity then
                table.remove(entity._surface.entities, i)
                break
            end
        end
    end
end

function game.get_entity_by_tag()
    return nil  -- Not implemented
end

function game.get_surface(name)
    return game.surfaces[name]
end

function game.set_surface_cheat()
    -- No-op for simulation
end

-- Register as global
_G.game = game

-- Utility functions (commonly used in Factorio scripts)
local function table_size(t)
    local count = 0
    for _ in pairs(t) do
        count = count + 1
    end
    return count
end
_G.table_size = table_size

local function deep_copy(t)
    if type(t) ~= "table" then return t end
    local copy = {}
    for k, v in pairs(t) do
        copy[k] = deep_copy(v)
    end
    return copy
end
_G.util = {
    deepcopy = deep_copy,
    table_size = table_size,
    inspect = function() end,  -- No-op for simulation
}

-- bit32 library mock (used by MathUtils.lua for bit operations)
local function to32bits(x)
    return x % 0x100000000
end

local function getBit(x, n)
    return math.floor(x / (2 ^ n)) % 2
end

local bit32 = {
    band = function(a, b)
        a = to32bits(a)
        b = to32bits(b)
        local result = 0
        for n = 0, 31 do
            if getBit(a, n) == 1 and getBit(b, n) == 1 then
                result = result + (2 ^ n)
            end
        end
        return result
    end,
    bor = function(a, b)
        a = to32bits(a)
        b = to32bits(b)
        local result = 0
        for n = 0, 31 do
            if getBit(a, n) == 1 or getBit(b, n) == 1 then
                result = result + (2 ^ n)
            end
        end
        return result
    end,
    bxor = function(a, b)
        a = to32bits(a)
        b = to32bits(b)
        local result = 0
        for n = 0, 31 do
            if getBit(a, n) ~= getBit(b, n) then
                result = result + (2 ^ n)
            end
        end
        return result
    end,
    bnot = function(a)
        return to32bits(bit32.bxor(a, 0xFFFFFFFF))
    end,
    rshift = function(a, n) return math.floor(to32bits(a) / (2 ^ n)) end,
    lshift = function(a, n) return to32bits(a * (2 ^ n)) end,
    arshift = function(a, n)
        a = to32bits(a)
        if a >= 0x80000000 then
            a = a - 0x100000000
        end
        return math.floor(a / (2 ^ n))
    end,
}
if package.config:sub(1,1) == "/" then
    -- On Linux, try to load real bit32 (Lua 5.2)
    pcall(function() bit32 = require("bit32") end)
end
_G.bit32 = bit32

-- Settings mock (for Constants.lua startup settings access)
_G.settings = {
    startup = {
        ["rampant--enemySeed"] = { value = 12345 },
        ["rampant--newEnemyVariations"] = { value = 1 },
        ["rampant--enemyTurretDPS"] = { value = 1.0 },
        ["rampant--overwriteDifficulty"] = { value = false },
        ["rampant--enemyBuildSpeedMultiplier"] = { value = 1.0 },
        ["rampant--enemyRangeMultiplier"] = { value = 1.0 },
        ["rampant--enemyHealthMultiplier"] = { value = 1.0 },
        ["rampant--enemySpeedMultiplier"] = { value = 1.0 },
        ["rampant--max-evo-dist"] = { value = 1.0 },
        ["rampant--tierStart"] = { value = 0 },
        ["rampant--tierEnd"] = { value = 5 },
        ["rampant--neutralTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--acidEnemy"] = { value = false },
        ["rampant--acidTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--laserEnemy"] = { value = false },
        ["rampant--laserTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--fireEnemy"] = { value = false },
        ["rampant--fireTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--infernoEnemy"] = { value = false },
        ["rampant--infernoTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--waspEnemy"] = { value = false },
        ["rampant--waspTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--spawnerEnemy"] = { value = true },
        ["rampant--spawnerTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--electricEnemy"] = { value = false },
        ["rampant--electricTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--physicalEnemy"] = { value = false },
        ["rampant--physicalTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--trollEnemy"] = { value = false },
        ["rampant--trollTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--poisonEnemy"] = { value = false },
        ["rampant--poisonTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--suicideEnemy"] = { value = false },
        ["rampant--suicideTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--nuclearEnemy"] = { value = false },
        ["rampant--nuclearTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--energyThiefEnemy"] = { value = false },
        ["rampant--energyThiefTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
        ["rampant--fastEnemy"] = { value = false },
        ["rampant--fastTints"] = { value = "00ff00ff 00ff00ff 00ff00ff" },
    },
    global = {
        ["rampant--debug"] = { value = false },
    }
}

return game