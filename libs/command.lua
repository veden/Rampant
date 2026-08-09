-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>

--[[
    Command - AI decision types and factory functions.

    Pure module. Zero Factorio dependencies.

    Every AI decision produces one or more command tables.
    An executor applies them against Factorio / storage state.
]]

local Command = {}

-- Squad lifecycle

function Command.createSquad(groupRef, baseId, settler, reason)
    return {
        type = "CREATE_SQUAD",
        groupRef = groupRef,
        baseId = baseId,
        settler = settler or false,
        reason = reason or "vanilla_spawn",
    }
end

function Command.destroySquad(squadId, reason)
    return {
        type = "DESTROY_SQUAD",
        squadId = squadId,
        reason = reason or "cleanup",
    }
end

function Command.destroyGroup(reason)
    return {
        type = "DESTROY_GROUP",
        reason = reason or "adopted",
    }
end

function Command.dispatchSquad(squadId, status, targetChunk, reason)
    return {
        type = "DISPATCH_SQUAD",
        squadId = squadId,
        status = status,       -- RAIDING, SETTLING, RETREATING, BUILDING, GUARDING
        targetChunk = targetChunk,
        reason = reason or "pheromone_guided",
    }
end

function Command.rallySquads(chunkId, cause, radius)
    return {
        type = "RALLY_SQUADS",
        chunkId = chunkId,
        cause = cause or "death_cry",
        radius = radius or 0,
    }
end

function Command.retreatUnits(chunkId, reason, radius)
    return {
        type = "RETREAT_UNITS",
        chunkId = chunkId,
        -- reason is a LABEL ("player_damage", "structure_destroyed").
        -- The entity to flee FROM is threaded separately by the death
        -- handler (cmd.causeRef) — the flee command requires a real
        -- LuaEntity, never a string.
        reason = reason or "outnumbered",
        radius = radius or 0,
    }
end

function Command.decompressSquad(squadId)
    return {
        type = "DECOMPRESS_SQUAD",
        squadId = squadId,
    }
end

-- Base lifecycle

function Command.createBase(chunkId, chunkX, chunkY)
    return {
        type = "CREATE_BASE",
        chunkId = chunkId,
        chunkX = chunkX,
        chunkY = chunkY,
    }
end

function Command.changeBaseState(baseId, newState, reason)
    return {
        type = "CHANGE_BASE_STATE",
        baseId = baseId,
        newState = newState,   -- RAIDING, SIEGE, MIGRATION, ONSLAUGHT, PEACEFUL
        reason = reason or "evolution_threshold",
    }
end

function Command.recycleBase(baseId, reason)
    return {
        type = "RECYCLE_BASE",
        baseId = baseId,
        reason = reason or "inactive",
    }
end

function Command.modifyUnitPoints(baseId, amount, source)
    return {
        type = "MODIFY_UNIT_POINTS",
        baseId = baseId,
        amount = amount,
        source = source or "unknown",
    }
end

function Command.queueUpgrade(entityName, baseId, evolve)
    return {
        type = "QUEUE_UPGRADE",
        entityName = entityName,
        baseId = baseId,
        evolve = evolve or false,
    }
end

-- Chunk / structure

function Command.registerStructure(chunkId, baseId, entityName)
    return {
        type = "REGISTER_STRUCTURE",
        chunkId = chunkId,
        baseId = baseId,
        entityName = entityName,
    }
end

function Command.unregisterStructure(chunkId, entityName, damageType)
    return {
        type = "UNREGISTER_STRUCTURE",
        chunkId = chunkId,
        entityName = entityName,
        damageType = damageType,
    }
end

function Command.markChunkDirty(chunkId, priority)
    return {
        type = "MARK_CHUNK_DIRTY",
        chunkId = chunkId,
        priority = priority or 4,
    }
end

function Command.triggerChunkGeneration(chunkX, chunkY)
    return {
        type = "TRIGGER_CHUNK_GENERATION",
        chunkX = chunkX,
        chunkY = chunkY,
    }
end

return Command
