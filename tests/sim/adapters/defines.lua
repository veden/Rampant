--[[
    Defines Adapter - Factorio `defines` global mock

    Provides simulation versions of Factorio's defines table
    for events, directions, distraction modes, and group states.
]]

local defines = {}

-- Direction enum (matches Factorio)
defines.direction = {
    north = 0,
    north_east = 1,
    east = 2,
    south_east = 3,
    south = 4,
    south_west = 5,
    west = 6,
    north_west = 7,
    up = 0,
    right = 2,
    down = 4,
    left = 6
}

-- Event enum (most common events used by Rampant)
defines.events = {
    on_tick = 0,
    on_settlement = 1,
    on_unit_group_created = 2,
    on_group_finished_gathering = 3,
    on_chunk_generated = 4,
    on_entity_created = 5,
    on_surface_created = 6,
    on_surface_deleted = 7,
    on_resource_depleted = 8,
    on_rocket_launch_started = 9,
    on_rocket_launch_rocket_body_removed = 10,
    on_player_changed_force = 11,
    on_force_created = 12,
    on_force_merged = 13,
    on_brush_layer = 14,
    on_area_clicked = 15,
    on_selected_entity_changed = 16,
    on_entity_marked_for_deconstruction = 17,
    on_entity_canceled_deconstruction = 18,
    on_pre_entity_died = 19,
    on_player_used_capsule = 20,
    on_robot_cliff_mined = 21,
    on_chunk_deleted = 22,
    on_surface_cleared = 23,
    on_trigger_created_entity = 24,
    on_chunk_chart_tag = 25,
    on_electric_network_changed = 26,
    on_team_produced = 27,
    on_player_placed_equipment = 28,
    on_player_removed_equipment = 29,
    on_robot_built_entity = 30,
    on_cancelled_deconstruction = 31,
    on_cancelled_upgrade = 32,
    on_trigger_fired = 33,
    on_ai_command_completed = 34,
    on_rain = 35,
    on_actor_constructed = 36,
    on_player_repaired_entity = 37,
    on_post_entity_died = 38,
    on_player_used_item = 39,
    on_picked_up_item = 40,
    on_drop_item = 41,
    on_player_main_tool_changed = 42,
    on_research_started = 43,
    on_research_finished = 44,
    on_research_reversed = 45,
    on_pheromone = 46,
    on_biter_base_built = 47,
    on_dolly_moved = 48,
    on_ai_minimal_reached = 49,
    on_working_visual_updated = 50,
}

-- Distraction enum for unit groups
defines.distraction = {
    none = "none",
    by_enemy = "by_enemy",
    by_anything = "by_anything"
}

-- Group state enum
defines.group_state = {
    gathering = "gathering",
    moving = "moving",
    attacking_distraction = "attacking_distraction",
    attacking_target = "attacking_target",
    finished = "finished",
    wandering = "wandering"
}

-- Command result enum
defines.command = {
    finished = "finished",
    failed = "failed",
    inferring = "inferring"
}

-- Entity status enum
defines.entity_status = {
    working = "working",
    idle = "idle",
    disabled = "disabled",
    destroyed = "destroyed"
}

-- Circuit connector ID enum
defines.circuit_connector_id = {
    none = 0,
    green_wire = 1,
    red_wire = 2
}

-- Wire type enum
defines.wire_type = {
    red = 1,
    green = 2
}

-- Compound command enum
defines.compound_command = {
    return_last = 1,
    return_first = 2,
    sequence = 3
}

-- Inventory enum (common ones)
defines.inventory = {
    character_guns = 1,
    character_ammo = 2,
    character_trash = 3,
    character_main = 4,
    god_controller = 5,
    factory_guns = 6,
    factory_ammo = 7,
    robot_inventory = 8,
    robot_cargo = 9,
    robot_recharge = 10,
    chest = 11,
    furnace_source = 12,
    furnace_result = 13,
    furnace_modules = 14,
    inbox = 15,
    outgoing_trash = 16,
    cargo_content = 17,
    equipment_grid = 18
}

-- Item stack status
defines.item_stack_status = {
    empty = "empty",
    normal = "normal",
    hidden = "hidden"
}

-- Train state
defines.train_state = {
    no_path = 1,
    on_the_path = 2,
    arrived_at_station = 3,
    waiting_for_path = 4,
    manually_stopped = 5
}

-- Rail pathfinder status
defines.rail_pathfinder_status = {
    no_path = 1,
    path_found = 2,
    path_lost = 3
}

-- Register as global
_G.defines = defines

return defines