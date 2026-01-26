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

return game