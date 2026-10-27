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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if SquadG then
    return SquadG
end
local Squad = {}

--

local Universe
local TargetPosition
local SearchPath
local Queries

-- imports

local Constants = require("Constants")
local MapUtils = require("MapUtils")
local BaseUtils = require("BaseUtils")
local MathUtils = require("MathUtils")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local Utils = require("Utils")
local SquadLogic = require("squad_logic")
local Executor = require("runtime/Executor")

-- Constants

local BASE_AI_STATE_ONSLAUGHT = Constants.BASE_AI_STATE_ONSLAUGHT
local BASE_AI_STATE_RAIDING = Constants.BASE_AI_STATE_RAIDING
local MAGIC_MAXIMUM_NUMBER = Constants.MAGIC_MAXIMUM_NUMBER
local MINIMUM_EXPANSION_DISTANCE = Constants.MINIMUM_EXPANSION_DISTANCE
local COMMAND_TIMEOUT = Constants.COMMAND_TIMEOUT
local BUILD_COMMAND_TIMEOUT = Constants.BUILD_COMMAND_TIMEOUT
local SQUAD_DISPATCH_INTERVAL = Constants.SQUAD_DISPATCH_INTERVAL
local SQUAD_RETREAT_DWELL = Constants.SQUAD_RETREAT_DWELL

-- Decay-on-read getters: the pure math lives in SquadLogic; the game.tick
-- read here is the impure boundary threaded into it (squad_logic never
-- reads the clock itself).
local function getPlayerPheromone(chunk)
    return SquadLogic.playerPheromone(chunk, game.tick)
end

local function getBasePheromone(chunk)
    return SquadLogic.basePheromone(chunk, game.tick)
end

function Squad.getBasePheromone(chunk)
    return getBasePheromone(chunk)
end

function Squad.getPlayerPheromone(chunk)
    return getPlayerPheromone(chunk)
end

local COMPRESSION_COOLDOWN = Constants.COMPRESSION_COOLDOWN
local EMPTY_RECRUIT_COOLDOWN = 600  -- 10 game-seconds

local HALF_CHUNK_SIZE = Constants.HALF_CHUNK_SIZE

local FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT = Constants.FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT

local SQUAD_BUILDING = Constants.SQUAD_BUILDING

local SQUAD_RAIDING = Constants.SQUAD_RAIDING
local SQUAD_SETTLING = Constants.SQUAD_SETTLING
local SQUAD_GUARDING = Constants.SQUAD_GUARDING
local SQUAD_RETREATING = Constants.SQUAD_RETREATING

local BASE_AI_STATE_SIEGE = Constants.BASE_AI_STATE_SIEGE
local BASE_AI_STATE_AGGRESSIVE = Constants.BASE_AI_STATE_AGGRESSIVE

local PLAYER_PHEROMONE_MULTIPLER = Constants.PLAYER_PHEROMONE_MULTIPLER

local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything
local COOLDOWN_RETREAT = Constants.COOLDOWN_RETREAT

local CHUNK_SIZE = Constants.CHUNK_SIZE
local COOLDOWN_RALLY = Constants.COOLDOWN_RALLY
local RALLY_CRY_DISTANCE = Constants.RALLY_CRY_DISTANCE

local AI_SQUAD_COST = Constants.AI_SQUAD_COST
local AI_SETTLER_COST = Constants.AI_SETTLER_COST
local AI_VENGENCE_SQUAD_COST = Constants.AI_VENGENCE_SQUAD_COST
local AI_VENGENCE_SETTLER_COST = Constants.AI_VENGENCE_SETTLER_COST

local ATTACKING_DISTRACTION = defines.group_state.attacking_distraction
local ATTACKING_TARGET = defines.group_state.attacking_target
local GATHERING = defines.group_state.gathering

-- Members compressed or decompressed per tick: entity create/destroy is the
-- per-member cost, so a large squad materialises over several ticks.
local SQUAD_MATERIALIZE_SQUADS_PER_TICK = 2
local SQUAD_MATERIALIZE_PER_TICK = 25

-- imported functions

local setPositionInQuery = Utils.setPositionInQuery

local getRallyTick = ChunkPropertyUtils.getRallyTick
local setRallyTick = ChunkPropertyUtils.setRallyTick
local modifyBaseUnitPoints = BaseUtils.modifyBaseUnitPoints

local tableRemove = table.remove
local tableInsert = table.insert
local mCeil = math.ceil

local isActiveNest = ChunkPropertyUtils.isActiveNest
local isActiveRaidNest = ChunkPropertyUtils.isActiveRaidNest

local gaussianRandomRangeRG = MathUtils.gaussianRandomRangeRG
local getEnemyStructureCount = ChunkPropertyUtils.getEnemyStructureCount
local getRetreatTick = ChunkPropertyUtils.getRetreatTick

local findNearbyBase = ChunkPropertyUtils.findNearbyBase
local tableSize = table_size
local setPositionInCommand = Utils.setPositionInCommand

local euclideanDistancePoints = MathUtils.euclideanDistancePoints
local canMoveChunkDirection = MapUtils.canMoveChunkDirection

local setRetreatTick = ChunkPropertyUtils.setRetreatTick

local removeSquadFromChunk = ChunkPropertyUtils.removeSquadFromChunk
local addDeathGenerator = ChunkPropertyUtils.addDeathGenerator

local getNeighborChunks = MapUtils.getNeighborChunks
local addSquadToChunk = ChunkPropertyUtils.addSquadToChunk
local getChunkByXY = MapUtils.getChunkByXY
local positionToChunkXY = MapUtils.positionToChunkXY
local positionFromScaledDirections = MapUtils.positionFromScaledDirections
local positionFromScaledSearchPath = MapUtils.positionFromScaledSearchPath

local euclideanDistanceNamed = MathUtils.euclideanDistanceNamed

-- module code
local function scoreRetreatLocation(neighborChunk)
    return (-SquadLogic.kamikazePheromone(neighborChunk, game.tick) +
            -((neighborChunk.playerBaseGenerator or 0) * 1000))
end

-- Distraction decision keys (pure) mapped to the engine's defines values.
-- The keys are owned by SquadLogic; this map is the single impure bridge.
local DISTRACTION_BY_KEY = {
    [SquadLogic.DISTRACTION_NONE] = DEFINES_DISTRACTION_NONE,
    [SquadLogic.DISTRACTION_BY_ENEMY] = DEFINES_DISTRACTION_BY_ENEMY,
}

local function scoreAttackLocation(neighborChunk)
    local preferred = false
    if (
        neighborChunk.playerBaseGenerator
        or neighborChunk.playerGenerator
    )
    then
        preferred = true
    end
    local damage = getBasePheromone(neighborChunk) +
        (getPlayerPheromone(neighborChunk) * PLAYER_PHEROMONE_MULTIPLER)
    if preferred then
        damage = damage * 2
    end
    return damage, preferred
end

local function scoreAttackKamikazeLocation(neighborChunk)
    local preferred = false
    if (
        neighborChunk.playerBaseGenerator
        or neighborChunk.playerGenerator
    )
    then
        preferred = true
    end
    local damage = SquadLogic.kamikazePheromone(neighborChunk, game.tick)
    if preferred then
        damage = damage * 2
    end
    return damage, preferred
end

local function findMovementPosition(surface, position)
    return surface.find_non_colliding_position(
        "behemoth-biter",
        position,
        10,
        2,
        false
    )
end

local function findDeploymentPosition(surface, position)
    -- Ground-unit prototype (the units must be able to stand there): the
    -- old "biter-spawner" prototype only checked building fit, which
    -- allowed land pockets behind deep-water barriers the units can't
    -- reach — the formation froze there.
    return surface.find_non_colliding_position(
        "small-biter",
        position,
        CHUNK_SIZE,
        4,
        true
    )
end

local function calculateSettlerMaxDistance()
    local targetDistance
    local distanceRoll = Universe.random()
    if distanceRoll < 0.05 then
        return 0
    elseif distanceRoll < 0.30 then
        targetDistance = Universe.expansionLowTargetDistance
    elseif distanceRoll < 0.70 then
        targetDistance = Universe.expansionMediumTargetDistance
    elseif distanceRoll < 0.95 then
        targetDistance = Universe.expansionHighTargetDistance
    else
        return Universe.expansionMaxDistance
    end
    return gaussianRandomRangeRG(targetDistance,
                                 Universe.expansionDistanceDeviation,
                                 MINIMUM_EXPANSION_DISTANCE,
                                 Universe.expansionMaxDistance,
                                 Universe.random)
end

local function addMovementPenalty(squad, chunk)
    if (chunk == -1) then
        return
    end
    local penalties = squad.penalties
    local penaltyCount = #penalties
    for i=1,penaltyCount do
        local penalty = penalties[i]
        if (penalty.c.id == chunk.id) then
            penalty.v = penalty.v + 1
            if penalty.v >= 7 then
                if Universe.enabledMigration
                    and (Universe.random() < 0.05)
                    and (Universe.builderCount < Universe.AI_MAX_BUILDER_COUNT)
                then
                    squad.settler = true
                    squad.originPosition.x = squad.group.position.x
                    squad.originPosition.y = squad.group.position.y
                    squad.maxDistance = calculateSettlerMaxDistance()

                    squad.status = SQUAD_SETTLING
                elseif not squad.kamikaze then
                    squad.kamikaze = true
                    squad.penalties = {}
                else
                    for _, entity in pairs(squad.group.members) do
                        if entity.valid then
                            entity.destroy()
                        end
                    end
                    squad.group.destroy()
                end
            end
            return
        end
    end
    if (penaltyCount == 10) then
        tableRemove(penalties, 10)
    end
    tableInsert(penalties,
                1,
                { v = 1,
                  c = chunk })
end

local function compressSquad(squad, tick)
    if squad.compressionSet
        or (Universe.squadCompressionThreshold == -1)
        or (squad.canBeCompressed >= tick)
    then
        return
    end

    local group = squad.group
    local groupState = group.state
    if (groupState == ATTACKING_DISTRACTION)
        or (groupState == ATTACKING_TARGET)
        or (groupState == GATHERING)
        or (squad.status == SQUAD_BUILDING)
    then
        return
    end
    local members = group.members
    if (#members <= Universe.squadCompressionThreshold) then
        return
    end
    -- Collect what has to go, then destroy a budget per tick. Compressing a
    -- 200-member squad used to be 200 entity.destroy() calls in one tick.
    local compressionSet = {}
    local compressedTotal = 0
    local totalTypes = 0
    local entityToTag
    local toDestroy = {}
    for _, entity in pairs(members) do
        local entityName = entity.name
        local count = compressionSet[entityName]
        if not count then
            totalTypes = totalTypes + 1
            if totalTypes > 6 then
                toDestroy[#toDestroy + 1] = entity
                compressedTotal = compressedTotal + 1
                compressionSet[entityName] = 1
            else
                entityToTag = entity
                compressionSet[entityName] = 0
            end
        else
            compressionSet[entityName] = count + 1
            compressedTotal = compressedTotal + 1
            toDestroy[#toDestroy + 1] = entity
        end
    end
    squad.compressQueue = toDestroy
    squad.compressSetPending = compressionSet
    squad.compressTotalPending = compressedTotal
    squad.compressTag = entityToTag
    squad.canBeCompressed = false
    local materializing = Universe.materializing or {}
    Universe.materializing = materializing
    materializing[squad.id] = squad
end

-- Drain the compression queue on a per-tick budget. Called from cleanSquads so
-- a large squad materialises over several ticks instead of one 100ms tick.
function Squad.processCompression(squad)
    local queue = squad.compressQueue
    if not queue then return end
    local done = 0
    while done < SQUAD_MATERIALIZE_PER_TICK and #queue > 0 do
        local entity = table.remove(queue)
        if entity and entity.valid then entity.destroy() end
        done = done + 1
    end
    if #queue == 0 then
        squad.compressQueue = nil
        if TelemetryG then
            if TelemetryG.isEnabled() then
                TelemetryG.emit("squad.compress", { squad = squad.id, members = squad.compressTotalPending })
            end
        end
        if Universe.materializing then Universe.materializing[squad.id] = nil end
        local query = Queries.renderText
        query.surface = squad.group.surface
        query.text = squad.compressTotalPending
        query.target = squad.compressTag
        squad.compressionText = rendering.draw_text(query)
        squad.compressionSet = squad.compressSetPending
        squad.compressedTotal = squad.compressTotalPending
        squad.compressSetPending = nil
    end
end

-- Members compressed or decompressed per tick: entity create/destroy is the
-- per-member cost, so a large squad materialises over several ticks.
-- Split a squad operation in the diagnostic timeline: our scoring versus the
-- engine call that issues the group command. Only logs inside a diagnose
-- window, so it costs nothing in normal play.
-- Cached metric names for the settle reason: the set is tiny and bounded.
local SETTLE_METRIC = {}
local function settleMetric(reason)
    local name = SETTLE_METRIC[reason]
    if not name then
        name = "squad.settle." .. reason
        SETTLE_METRIC[reason] = name
    end
    return name
end

local function timedSquad(name, fn)
    if not (Universe and Universe.diagTicks) then
        return fn()
    end
    local profiler = helpers.create_profiler()
    local result = fn()
    log({"", "[SUB] tick=", game.tick, " name=", name, " ", profiler})
    return result
end


-- Is this squad actually fighting? Decompressing is only worth its kernel cost
-- when the members are about to be needed; a squad that merely lost a straggler
-- while marching can stay compressed.
local function squadIsEngaged(squad)
    local group = squad.group
    if not (group and group.valid) then return false end
    local state = group.state
    return state == ATTACKING_DISTRACTION
        or state == ATTACKING_TARGET
        or state == GATHERING
        or squad.status == SQUAD_BUILDING
end

function Squad.decompressSquad(squad, tick, force)
    if not squad.compressionSet then
        return
    end
    if not force and not squadIsEngaged(squad) and squad.compressedTotal and squad.compressedTotal > 0
        and not squad.decompressQueue then
        if MetricsG then MetricsG.inc("squad.decompressSkipped") end
        if TelemetryG then
            if TelemetryG.isEnabled() then
                TelemetryG.emit("squad.decompressSkipped", { squad = squad.id, members = squad.compressedTotal })
            end
        end
        return
    end

    local group = squad.group
    local query = Queries.createEntityQuery
    local add_member = group.add_member
    local create_entity = squad.map.surface.create_entity
    query.position = {group.position.x, group.position.y}  -- fresh table each time
    -- Materialise a budget per tick: re-creating 200 members in one tick was
    -- the other half of the 100ms+ squad dispatch.
    if not squad.decompressQueue then
        local queue = {}
        for name, count in pairs(squad.compressionSet) do
            queue[#queue + 1] = { name = name, count = count }
        end
        squad.decompressQueue = queue
    end
    local done = 0
    local queue = squad.decompressQueue
    while done < SQUAD_MATERIALIZE_PER_TICK and #queue > 0 do
        local entry = queue[#queue]
        query.name = entry.name
        add_member(create_entity(query))
        entry.count = entry.count - 1
        if entry.count <= 0 then table.remove(queue) end
        done = done + 1
    end
    if #queue > 0 then
        local materializing = Universe.materializing or {}
        Universe.materializing = materializing
        materializing[squad.id] = squad
        return
    end
    squad.decompressQueue = nil
    if TelemetryG then
        if TelemetryG.isEnabled() then
            TelemetryG.emit("squad.decompress", { squad = squad.id, members = squad.compressedTotal })
        end
    end
    if Universe.materializing then Universe.materializing[squad.id] = nil end
    -- 2.x API: rendering.destroy(id) no longer exists on LuaRendering —
    -- destruction moved onto the LuaRenderObject (its destroy() does not
    -- error on invalid). Found at session scale: the long-run's natural
    -- idle->compressed->re-engaged squad cycle hit this at tick ~290k
    -- (80 game-minutes) and took the server down (non-recoverable).
    if squad.compressionText then
        squad.compressionText.destroy()
    end
    squad.compressionSet = nil
    squad.canBeCompressed = tick + COMPRESSION_COOLDOWN
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
local function scoreNeighbors(map, chunk, neighborDirectionChunks, scoreFunction, tick)
    local highest = SearchPath[1]
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestPreferred = false

    highest.chunk = -1

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) then
            if (chunk == -1) or canMoveChunkDirection(x, chunk, neighborChunk) then
                local score, preferred = scoreFunction(neighborChunk, tick)
                if (highestPreferred and preferred and (score > highestScore))
                    or (not highestPreferred and (score > highestScore))
                    or (not highestPreferred and preferred)
                then
                    highest.chunk = neighborChunk
                    highest.direction = x
                    highestScore = score
                    highestPreferred = preferred
                end
            end
        end
    end

    if (highest.chunk == -1) then
        return SearchPath
    end

    if highestPreferred then
        SearchPath[2].chunk = -1
        return SearchPath
    end

    for i = 2, 4 do
        local lastChunk = SearchPath[i-1].chunk
        highest = SearchPath[i]
        highest.chunk = -1
        highestPreferred = false

        neighborDirectionChunks = getNeighborChunks(map, lastChunk.x, lastChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]
            if ((neighborChunk ~= -1) and (neighborChunk.id ~= lastChunk.id) and
                canMoveChunkDirection(x, lastChunk, neighborChunk))
            then
                local score, preferred = scoreFunction(neighborChunk, tick)
                if (highestPreferred and preferred and (score > highestScore))
                    or (not highestPreferred and (score > highestScore))
                    or (not highestPreferred and preferred)
                then
                    highestScore = score
                    highest.chunk = neighborChunk
                    highest.direction = x
                    highestPreferred = preferred
                end
            end
        end
        if highest.chunk == -1 then
            return SearchPath
        end
        if highestPreferred and (i ~= 4) then
            SearchPath[i+1].chunk = -1
            return SearchPath
        end
    end

    return SearchPath
end

local function settleMove(squad, tick)
    local group = squad.group
    local map = squad.map

    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local chunk = getChunkByXY(map, x, y)
    -- squad.type is the creation-time snapshot of the base's stateAI;
    -- nil (legacy squads) is non-siege.
    local scoreFunction = SquadLogic.selectSettleScorer(squad.type or 0, squad.kamikaze)
    local squadChunk = squad.chunk
    if squadChunk ~= -1 then
        addDeathGenerator(squadChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
    end
    if chunk ~= -1 then
        addSquadToChunk(chunk, squad)
        addMovementPenalty(squad, chunk)
        if not squad.group.valid then
            return
        end
    end
    local originPosition = squad.originPosition
    local distance = euclideanDistancePoints(groupPosition.x,
                                             groupPosition.y,
                                             originPosition.x,
                                             originPosition.y)
    local cmd
    local position
    local surface = map.surface

    -- In-place settle: the march decision (T_SettleInPlace). pathEndIndex
    -- is nil here — the lookahead path has not been evaluated yet, so the
    -- path rule is inert and the in-place rules decide.
    if chunk ~= -1 then
        local settleDecision = SquadLogic.evaluateSettleTrigger({
            distanceToOrigin = distance,
            maxDistance = squad.maxDistance,
            nestCount = chunk.nestCount,
            hiveCount = chunk.hiveCount,
            resourceGenerator = chunk.resourceGenerator,
        })
        if settleDecision.settle then
            if MetricsG then
                MetricsG.inc(settleMetric(settleDecision.reason))
            end
            timedSquad("squad.decompress", function() Squad.decompressSquad(squad, tick) end)

            position = timedSquad("squad.findPosition", function()
                return findMovementPosition(surface, groupPosition)
            end) or groupPosition

            cmd = Queries.settleCommand
            cmd.distraction = DISTRACTION_BY_KEY[SquadLogic.selectSettleDistraction(squad.kamikaze)]

            setPositionInCommand(cmd, position)

            squad.status = SQUAD_BUILDING
            squad.commandTick = tick + BUILD_COMMAND_TIMEOUT

            timedSquad("squad.command", function() group.set_command(cmd) end)
            return
        end
    end

    local searchPath = timedSquad("squad.score", function()
        return scoreNeighbors(map, chunk, getNeighborChunks(map, x, y), scoreFunction, tick)
    end)

    if (searchPath[1].chunk == -1) then
        cmd = Queries.wanderCommand
        timedSquad("squad.command", function() group.set_command(cmd) end)
        return
    end

    local lastChunk = 0
    for i = 1, 4 do
        local moveChunk = searchPath[i].chunk
        if (moveChunk ~= -1) then
            distance = euclideanDistancePoints(
                moveChunk.x + HALF_CHUNK_SIZE,
                moveChunk.y + HALF_CHUNK_SIZE,
                originPosition.x,
                originPosition.y
            )
            if distance >= squad.maxDistance then
                searchPath[i].chunk = -1
                if i < 4 then
                    searchPath[i+1].chunk = -1
                end
            else
                lastChunk = i
                addDeathGenerator(moveChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
            end
        end
    end

    position = findMovementPosition(
        surface,
        positionFromScaledSearchPath(
            groupPosition,
            1,
            searchPath
        )
    )

    if not position then
        cmd = Queries.wanderCommand
        timedSquad("squad.command", function() group.set_command(cmd) end)
        return
    end

    TargetPosition.x = position.x
    TargetPosition.y = position.y

    -- Path-end decision (T_SettlePathEnd / T_ContinueMarch): the lookahead
    -- ended early -> build at the reached position; otherwise keep moving.
    -- The in-place rules are inert here (they were checked above); only
    -- the path rule can fire. nest/hive/resource are not passed: the chunk
    -- may be -1 (unregistered) and the path rule decides before they
    -- would be read.
    local settleDecision = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = distance,
        maxDistance = squad.maxDistance,
        pathEndIndex = lastChunk,
    })
    if settleDecision.settle then
        if MetricsG then
            MetricsG.inc(settleMetric(settleDecision.reason))
        end
        cmd = Queries.settleCommand
        squad.status = SQUAD_BUILDING
        squad.commandTick = tick + BUILD_COMMAND_TIMEOUT
        Squad.decompressSquad(squad, tick, true)
        cmd.distraction = DISTRACTION_BY_KEY[SquadLogic.selectSettleDistraction(squad.kamikaze)]
    else
        cmd = Queries.moveCommand
        cmd.distraction = DISTRACTION_BY_KEY[SquadLogic.selectMoveDistraction(squad.rabid, squad.kamikaze)]
    end

    setPositionInCommand(cmd, TargetPosition)

    group.set_command(cmd)
end

local function attackMove(squad, tick)
    local group = squad.group

    local groupPosition = group.position
    local x, y = positionToChunkXY(groupPosition)
    local map = squad.map
    local chunk = getChunkByXY(map, x, y)
    local squadChunk = squad.chunk
    if squadChunk ~= -1 then
        addDeathGenerator(squadChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
    end
    if chunk ~= -1 then
        addSquadToChunk(chunk, squad)
        addMovementPenalty(squad, chunk)
        if not squad.group.valid then
            return
        end
    end

    local attackScorer = scoreAttackLocation
    if squad.kamikaze then
        attackScorer = scoreAttackKamikazeLocation
    end

    squad.frenzy = (squad.frenzy and (euclideanDistanceNamed(groupPosition, squad.frenzyPosition) < 100))
    local searchPath = scoreNeighbors(
        map,
        chunk,
        getNeighborChunks(map, x, y),
        attackScorer,
        tick
    )
    local cmd
    if (searchPath[1].chunk == -1) then
        -- Anti-stuck guard (was in cleanSquads' wander fallback): a squad
        -- that repeatedly finds no path wanders and eventually cleans up.
        squad.wanders = squad.wanders + 1
        if squad.wanders > 5 then
            if MetricsG then MetricsG.inc("squad.wanderDestroy") end
            for _, entity in pairs(squad.group.members) do
                if entity.valid then
                    entity.destroy()
                end
            end
            squad.group.destroy()
            return
        end
        if MetricsG then MetricsG.inc("squad.wander") end
        cmd = Queries.wanderCommand
        timedSquad("squad.command", function() group.set_command(cmd) end)
        return
    end

    local position = findMovementPosition(
        map.surface,
        positionFromScaledSearchPath(
            groupPosition,
            1,
            searchPath
        )
    )

    if not position then
        cmd = Queries.wanderCommand
        timedSquad("squad.command", function() group.set_command(cmd) end)
        return
    end

    local attack = false
    for i = 1, 4 do
        local attackChunk = searchPath[i].chunk
        if (attackChunk ~= -1) then
            addDeathGenerator(attackChunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
            if attackChunk.playerBaseGenerator or
                (getPlayerPheromone(attackChunk) >= Universe.attackPlayerThreshold)
            then
                attack = true
            end
        end
    end

    if attack then
        Squad.decompressSquad(squad, tick)
        cmd = Queries.attackCommand

        if not squad.rabid then
            squad.frenzy = true
            squad.frenzyPosition.x = groupPosition.x
            squad.frenzyPosition.y = groupPosition.y
        end
    else
        cmd = Queries.moveCommand
        if squad.rabid or squad.frenzy then
            cmd.distraction = DEFINES_DISTRACTION_BY_ANYTHING
        else
            cmd.distraction = DEFINES_DISTRACTION_BY_ENEMY
        end
    end

    TargetPosition.x = position.x
    TargetPosition.y = position.y
    setPositionInCommand(cmd, TargetPosition)

    group.set_command(cmd)
end

local function buildMove(squad, tick)
    local group = squad.group
    local groupPosition = group.position
    local position = findMovementPosition(squad.map.surface, groupPosition) or groupPosition

    setPositionInCommand(Queries.settleCommand, position)

    Squad.decompressSquad(squad, tick)

    group.set_command(Queries.settleCommand)
end

-- Members per dispatch "batch": a squad larger than this is re-commanded less
-- often, in proportion. 50 keeps a 200-unit squad to one dispatch every four
-- intervals, which amortises the per-member cost without changing its orders.
local SQUAD_DISPATCH_BATCH = 50

function Squad.cleanSquads(tick)
    -- Materialisation work (large squads compressing/decompressing) is drained
    -- first, so it keeps progressing even when the round-robin does not reach
    -- that squad for a while.
    local materializing = Universe.materializing
    if materializing then
        local drained = 0
        for id, squad in pairs(materializing) do
            if squad.compressQueue then
                Squad.processCompression(squad)
            elseif squad.decompressQueue then
                Squad.decompressSquad(squad, tick)
            else
                materializing[id] = nil
            end
            drained = drained + 1
            if drained >= SQUAD_MATERIALIZE_SQUADS_PER_TICK then break end
        end
    end
    local squads = Universe.groupNumberToSquad
    local groupId = Universe.squadIterator
    local squad
    if not groupId then
        groupId, squad = next(squads, groupId)
    else
        squad = squads[groupId]
    end
    if not groupId then
        Universe.squadIterator = nil
        if (next(squads) == nil) then
            -- this is needed as the next command remembers the max length a table has been
            Universe.groupNumberToSquad = {}
        end
    else
        Universe.squadIterator = next(squads, groupId)
        local group = squad.group
        if not group.valid then
            if squad.chunk ~= -1 then
                addDeathGenerator(squad.chunk, -FIVE_DEATH_PHEROMONE_GENERATOR_AMOUNT)
            end
            removeSquadFromChunk(squad)
            if squad.settlers then
                Universe.builderCount = Universe.builderCount - 1
                if Universe.builderCount < 0 then
                    Universe.builderCount = 0
                end
            else
                Universe.squadCount = Universe.squadCount - 1
                if Universe.squadCount < 0 then
                    Universe.squadCount = 0
                end
                if squad.type == BASE_AI_STATE_AGGRESSIVE then
                    local base = squad.base
                    base.sentAggressiveGroups = base.sentAggressiveGroups - 1
                    if base.sentAggressiveGroups < 0 then
                        base.sentAggressiveGroups = 0
                    end
                end
            end
            squads[groupId] = nil
            if MetricsG then
                MetricsG.inc("squad.destroyed")
            end
        elseif (group.state == 4) then
            squad.wanders = 0
            if MetricsG then MetricsG.inc("squad.reachedState4") end
            timedSquad("squad.dispatch", function() Squad.squadDispatch(squad, tick) end)
            if group.valid then
                timedSquad("squad.compress", function() compressSquad(squad, tick) end)
            end
        elseif (squad.commandTick and (squad.commandTick < tick)) then
            -- Mod-owned dispatch cadence: refresh the path regardless of
            -- the vanilla group state (the group may be stuck gathering —
            -- the old fallback wandered blindly and never re-dispatched).
            -- The dispatch's no-path branch handles the wander + the
            -- anti-stuck guard.
            -- A dispatch is one group.set_command plus per-member engine work,
            -- so its cost scales with the group. Measured 138ms for a late-game
            -- squad. Refresh big groups less often so the same total work is
            -- spread over more ticks instead of landing in one.
            local members = (group and group.valid and #group.members) or 0
            local scale = math.max(1, math.ceil(members / SQUAD_DISPATCH_BATCH))
            squad.commandTick = tick + SQUAD_DISPATCH_INTERVAL * scale
            timedSquad("squad.dispatch", function() Squad.squadDispatch(squad, tick) end)
        end
    end
end

function Squad.squadDispatch(squad, tick)
    local group = squad.group
    if not (group and group.valid) then
        return
    end

    -- Pure evaluation: what should this squad do?
    local squadData = {
        id = squad.id,
        status = squad.status,
        settlers = squad.settlers,
    }
    local commands
    timedSquad("squad.evaluate", function()
        commands = SquadLogic.evaluate(squadData)
    end)
    if #commands > 0 then
        timedSquad("squad.commands", function() Executor.run(commands) end)
    end

    -- Execute the movement command for current status.
    -- Status transitions (RETREATING/GUARDING) are applied by the Executor
    -- via DISPATCH_SQUAD above; only movement remains here.
    local status = squad.status
    if (status == SQUAD_RAIDING) then
        squad.commandTick = tick + COMMAND_TIMEOUT
        timedSquad("squad.attackMove", function() attackMove(squad, tick) end)
    elseif (status == SQUAD_SETTLING) then
        squad.commandTick = tick + COMMAND_TIMEOUT
        timedSquad("squad.settleMove", function() settleMove(squad, tick) end)
    elseif (status == SQUAD_BUILDING) then
        squad.commandTick = tick + BUILD_COMMAND_TIMEOUT
        timedSquad("squad.buildMove", function()
            removeSquadFromChunk(squad)
            buildMove(squad, tick)
        end)
    end
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
local function scoreNeighborsForRetreat(chunk, neighborDirectionChunks, scoreFunction, map)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection

    for x=1,8 do
        local neighborChunk = neighborDirectionChunks[x]
        if (neighborChunk ~= -1) then
            if (chunk == -1) or canMoveChunkDirection(x, chunk, neighborChunk) then
                local score = scoreFunction(neighborChunk)
                if (score > highestScore) then
                    highestScore = score
                    highestChunk = neighborChunk
                    highestDirection = x
                end
            end
        end
    end

    local nextHighestChunk = -1
    local nextHighestScore = highestScore
    local nextHighestDirection

    if (highestChunk ~= -1) then
        neighborDirectionChunks = getNeighborChunks(map, highestChunk.x, highestChunk.y)
        for x=1,8 do
            local neighborChunk = neighborDirectionChunks[x]

            if ((neighborChunk ~= -1) and ((chunk == -1) or (neighborChunk.id ~= chunk.id)) and
                canMoveChunkDirection(x, highestChunk, neighborChunk)) then
                local score = scoreFunction(neighborChunk)
                if (score > nextHighestScore) then
                    nextHighestScore = score
                    nextHighestChunk = neighborChunk
                    nextHighestDirection = x
                end
            end
        end
    end

    if (nextHighestChunk == nil) then
        nextHighestChunk = -1
    end

    return highestChunk, highestDirection, nextHighestChunk, nextHighestDirection
end

-- Reuse a squad already retreating in this chunk or its neighbors instead
-- of creating a new squad per retreat. Without the reuse, every retreat
-- spawns a fresh squad and the kamikaze escalation (existingSquad.retreats
-- >= 3) is unreachable — the refactor dropped the legacy findNearby-
-- RetreatingSquad, leaving the existingSquad param permanently nil.
local function findNearbyRetreatingSquad(map, chunk)
    local function check(c)
        if c and c.squads then
            for _, squad in pairs(c.squads) do
                if (squad.status == SQUAD_RETREATING) and squad.group and squad.group.valid then
                    return squad
                end
            end
        end
        return nil
    end
    local found = check(chunk)
    if found then
        return found
    end
    local neighbors = getNeighborChunks(map, chunk.x, chunk.y)
    for i = 1, 8 do
        local neighbor = neighbors[i]
        if neighbor ~= -1 then
            found = check(neighbor)
            if found then
                return found
            end
        end
    end
    return nil
end

function Squad.retreatUnits(chunk, cause, tick, existingSquad, radius)
    -- The flee command requires the aggressor LuaEntity (defines.command.flee
    -- from). A missing/invalid cause means the retreat cannot execute — the
    -- emitter must have threaded causeRef. Count it; never crash the engine.
    if not cause or not cause.valid then
        MetricsG.inc("squad.retreatBail.noCause")
        return
    end
    -- getRetreatTick returns 0 for "never retreated", which as a cooldown
    -- check means "retreated at tick 0" and suppresses every retreat during the
    -- first COOLDOWN_RETREAT ticks of a save. That is how death-response and
    -- squad-cycle failed: units reached the wall and were killed inside those
    -- first 10 seconds, so the retreat that should follow the first deaths was
    -- gated out. Split the two conditions and let a chunk with no recorded
    -- retreat act immediately.
    local lastRetreat = getRetreatTick(chunk)
    local cooled = (lastRetreat == 0) or ((tick - lastRetreat) > COOLDOWN_RETREAT)
    if getEnemyStructureCount(chunk) ~= 0 then
        MetricsG.inc("squad.retreatBail.structures")
        MetricsG.inc("squad.retreatBail.gate")
        return
    end
    if not cooled then
        MetricsG.inc("squad.retreatBail.cooldown")
        MetricsG.inc("squad.retreatBail.gate")
        return
    end

    if existingSquad and
        (
            (not existingSquad.group.valid)
            or existingSquad.kamikaze
        )
    then
        MetricsG.inc("squad.retreatBail.invalidSquad")
        return
    end

    local map = chunk.map

    setRetreatTick(chunk, tick)
    local exitPath,exitDirection,
        nextExitPath,nextExitDirection  = scoreNeighborsForRetreat(chunk,
                                                                   getNeighborChunks(map,
                                                                                     chunk.x,
                                                                                     chunk.y),
                                                                   scoreRetreatLocation,
                                                                   map)
    local position = {
        x = chunk.x + 16,
        y = chunk.y + 16
    }
    local retreatPosition
    local surface = map.surface
    if (exitPath == -1) then
        MetricsG.inc("squad.retreatBail.noExit")
        return
    elseif (nextExitPath ~= -1) then
        retreatPosition = findMovementPosition(
            surface,
            positionFromScaledDirections(
                position,
                1,
                exitDirection,
                nextExitDirection
            )
        )
    else
        retreatPosition = findMovementPosition(
            surface,
            positionFromScaledDirections(
                position,
                1,
                exitDirection
            )
        )
    end

    if retreatPosition then
        position.x = retreatPosition.x
        position.y = retreatPosition.y
    else
        MetricsG.inc("squad.retreatBail.noPosition")
        return
    end

    if not existingSquad then
        existingSquad = findNearbyRetreatingSquad(map, chunk)
    end

    if not existingSquad then
        if (Universe.squadCount >= Universe.AI_MAX_SQUAD_COUNT) then
            MetricsG.inc("squad.retreatBail.squadLimit")
            return
        end

        local base = findNearbyBase(chunk)
        if not base then
            MetricsG.inc("squad.retreatBail.noBase")
            return
        end
        existingSquad = Squad.createSquad(position, map, nil, false, base)

        Queries.fleeCommand.from = cause
        Queries.retreatCommand.group = existingSquad.group

        Queries.formRetreatCommand.unit_search_distance = radius

        local foundUnits = surface.set_multi_command(Queries.formRetreatCommand)

        if (foundUnits == 0) then
            existingSquad.group.destroy()
            MetricsG.inc("squad.retreatBail.noUnits")
            return
        end

        Universe.groupNumberToSquad[existingSquad.id] = existingSquad
        Universe.squadCount = Universe.squadCount + 1
    else
        Queries.moveCommand.distraction = DEFINES_DISTRACTION_NONE
        setPositionInCommand(Queries.moveCommand, position)
        existingSquad.group.set_command(Queries.moveCommand)
    end

    existingSquad.retreats = existingSquad.retreats + 1
    if (existingSquad.retreats >= 3) then
        existingSquad.kamikaze = true
    end

    existingSquad.status = SQUAD_RETREATING
    -- Hold the retreat posture: commandTick is the next-dispatch deadline.
    -- The retreat site is the only status transition that never refreshed
    -- it, so the dispatch cadence (commandTick 0 at creation, expired) hit
    -- on the same tick and evaluate() collapsed RETREATING -> RAIDING
    -- before any observer (or the flee walk itself) could act — retreats
    -- became instant re-engages (scenario squad-cycle regression).
    existingSquad.commandTick = tick + SQUAD_RETREAT_DWELL
    if MetricsG then
        MetricsG.inc("squad.retreated")
    end

    addSquadToChunk(chunk, existingSquad)

    existingSquad.frenzy = true
    existingSquad.frenzyPosition.x = position.x
    existingSquad.frenzyPosition.y = position.y
end

function Squad.createSquad(position, map, group, settlers, base)
    local unitGroup = group
    if not unitGroup then
        local query = Queries.createUnitGroup
        setPositionInQuery(query, position)
        unitGroup = map.surface.create_unit_group(query)
    end

    local squad = {
        group = unitGroup,
        id = unitGroup.unique_id,
        canBeCompressed = 0,
        compressionSet = nil,
        compressionText = nil,
        status = SQUAD_GUARDING,
        rabid = false,
        penalties = {},
        base = base,
        type = base.stateAI,
        retreats = 0,
        frenzy = false,
        map = map,
        wanders = 0,
        settlers = settlers or false,
        kamikaze = false,
        frenzyPosition = {x = 0,
                          y = 0},
        maxDistance = 0,
        originPosition = {x = 0,
                          y = 0},
        commandTick = 0,
        chunk = -1
    }

    if MetricsG then
        MetricsG.inc("squad.created")
    end

    if settlers then
        squad.maxDistance = calculateSettlerMaxDistance()
    end

    if position then
        squad.originPosition.x = position.x
        squad.originPosition.y = position.y
    elseif group then
        squad.originPosition.x = group.position.x
        squad.originPosition.y = group.position.y
    end

    return squad
end

local function settlerWaveScaling()
    return mCeil(gaussianRandomRangeRG(Universe.settlerWaveSize,
                                       Universe.settlerWaveDeviation,
                                       Universe.expansionMinSize,
                                       Universe.expansionMaxSize,
                                       Universe.random))
end

local function attackWaveScaling()
    return mCeil(gaussianRandomRangeRG(Universe.attackWaveSize,
                                       Universe.attackWaveDeviation,
                                       1,
                                       Universe.attackWaveUpperBound,
                                       Universe.random))
end

local function attackWaveValidCandidate(chunk)
    if isActiveNest(chunk) then
        return true
    end
    local base = chunk.base
    if (base.stateAI == BASE_AI_STATE_RAIDING) or
        (base.stateAI == BASE_AI_STATE_SIEGE) or
        (base.stateAI == BASE_AI_STATE_ONSLAUGHT)
    then
        return isActiveRaidNest(chunk)
    end
    return false
end

local function visitPattern(o, cX, cY, distance)
    local startX
    local endX
    local stepX
    local startY
    local endY
    local stepY
    if (o == 0) then
        startX = cX - distance
        endX = cX + distance
        stepX = 32
        startY = cY - distance
        endY = cY + distance
        stepY = 32
    elseif (o == 1) then
        startX = cX + distance
        endX = cX - distance
        stepX = -32
        startY = cY + distance
        endY = cY - distance
        stepY = -32
    elseif (o == 2) then
        startX = cX - distance
        endX = cX + distance
        stepX = 32
        startY = cY + distance
        endY = cY - distance
        stepY = -32
    elseif (o == 3) then
        startX = cX + distance
        endX = cX - distance
        stepX = -32
        startY = cY - distance
        endY = cY + distance
        stepY = 32
    end
    return startX, endX, stepX, startY, endY, stepY
end

--[[
    Expects all neighbors adjacent to a chunk
--]]
local function scoreNeighborsForFormation(chunk, validFunction, scoreFunction, tick)
    local highestChunk = -1
    local highestScore = -MAGIC_MAXIMUM_NUMBER
    local highestDirection
    local neighborChunks = getNeighborChunks(chunk.map, chunk.x, chunk.y)
    for x=1,8 do
        local neighborChunk = neighborChunks[x]
        if (neighborChunk ~= -1)
            and canMoveChunkDirection(x, chunk, neighborChunk)
            and validFunction(neighborChunk)
        then
            local score = scoreFunction(neighborChunk, tick)
            if (score > highestScore) then
                highestScore = score
                highestChunk = neighborChunk
                highestDirection = x
            end
        end
    end

    return highestChunk, highestDirection
end

function Squad.rallyUnits(chunk, tick)
    if (tick - getRallyTick(chunk)) < COOLDOWN_RALLY then
        return
    end
    setRallyTick(chunk, tick)
    local cX = chunk.x
    local cY = chunk.y
    local startX, endX, stepX, startY, endY, stepY = visitPattern(tick % 4, cX, cY, RALLY_CRY_DISTANCE)
    local vengenceQueue = Universe.vengenceQueue
    local map = chunk.map
    for x=startX, endX, stepX do
        for y=startY, endY, stepY do
            if (x ~= cX) and (y ~= cY) then
                local rallyChunk = getChunkByXY(map, x, y)
                if rallyChunk ~= -1 then
                    local base = rallyChunk.base
                    if rallyChunk.nestCount
                        and (base.unitPoints >= AI_VENGENCE_SQUAD_COST)
                    then
                        vengenceQueue[rallyChunk.id] = true
                    end
                end
            end
        end
    end
end

local function deploySquad(name, chunk, cost, vengence, attacker)
    local base = chunk.base
    -- Back-off after an empty recruitment: when no units were found, retrying
    -- every 3 ticks creates a create/destroy group storm (hundreds of empty
    -- groups per minute) that collapses UPS. Wait for units to accumulate.
    if base.nextFormAttemptTick and game.tick < base.nextFormAttemptTick then
        if MetricsG then
            MetricsG.inc("squad.formSkip.cooldown")
        end
        return
    end
    if MetricsG then
        MetricsG.inc("squad.formAttempt")
    end

    -- Formation gate (pure): the full rejection chain with its Metrics
    -- counter names. The roll is drawn per form attempt; the gate decides
    -- with it (roll == threshold passes).
    local gate = SquadLogic.evaluateFormationGate({
        attacker = attacker,
        vengence = vengence,
        unitPoints = base.unitPoints,
        cost = cost,
        basePheromone = getBasePheromone(chunk),
        playerPheromone = getPlayerPheromone(chunk),
        squadCount = Universe.squadCount,
        squadLimit = Universe.AI_MAX_SQUAD_COUNT,
        builderCount = Universe.builderCount,
        builderLimit = Universe.AI_MAX_BUILDER_COUNT,
        sentExpansionGroups = base.sentExpansionGroups,
        maxExpansionGroups = base.maxExpansionGroups,
        hasActiveCandidate = attackWaveValidCandidate(chunk),
        formThreshold = Universe.formSquadThreshold,
        roll = Universe.random(),
    })
    if not gate.accepted then
        if MetricsG then MetricsG.inc(gate.metric) end
        return
    end

    local scoringFunction = SquadLogic.selectFormationScorer(attacker, base.stateAI)

    local squadPath, squadDirection = scoreNeighborsForFormation(
        chunk,
        SquadLogic.validUnitGroupLocation,
        scoringFunction,
        game.tick
    )
    if (squadPath == -1) then
        return
    end

    local map = chunk.map
    local surface = map.surface
    TargetPosition.x = chunk.x + HALF_CHUNK_SIZE
    TargetPosition.y = chunk.y + HALF_CHUNK_SIZE

    -- Deploy NEAR THE UNITS the formation will gather: the ungrouped units
    -- in the chunk + neighbor area, the group forms at their centroid. The
    -- old fixed 1.25-chunk offset (scored direction, spawner-fit check)
    -- could land the group in a unit-unreachable pocket (the gather would
    -- stall forever — the squad froze with no dispatch). Centroid
    -- deployment makes the gathering distance ~0.
    local squadPosition
    local unitArea = {
        { chunk.x - CHUNK_SIZE, chunk.y - CHUNK_SIZE },
        { chunk.x + 2 * CHUNK_SIZE, chunk.y + 2 * CHUNK_SIZE },
    }
    local units = surface.find_entities_filtered{
        force = "enemy",
        type = "unit",
        area = unitArea,
    }
    local cx, cy, unitCount = 0, 0, 0
    for _, u in ipairs(units) do
        if u.valid then
            cx = cx + u.position.x
            cy = cy + u.position.y
            unitCount = unitCount + 1
        end
    end
    if unitCount > 0 then
        -- The centroid fit must use the SQUAD's unit size (small-biter):
        -- findMovementPosition uses the behemoth-biter prototype, and the
        -- gathered units themselves block a behemoth-sized spot at the
        -- centroid — the find failed and the fallback deployed into
        -- unreachable water pockets.
        squadPosition = surface.find_non_colliding_position(
            "small-biter",
            { x = cx / unitCount, y = cy / unitCount },
            10,
            2,
            false
        )
        if not squadPosition then
            -- Wider search (the chunk area) before the scored-direction
            -- fallback: the tight find can fail when the centroid sits on
            -- a unit's collision box (the single-unit case).
            squadPosition = surface.find_non_colliding_position(
                "small-biter",
                { x = cx / unitCount, y = cy / unitCount },
                CHUNK_SIZE,
                4,
                true
            )
        end
    end

    -- Fallback: the scored-direction deployment, validated with the ground
    -- unit prototype (the units can stand there — the spawner prototype
    -- only checked building fit and allowed deep-water pockets).
    if not squadPosition then
        if MetricsG then MetricsG.inc("squad.deploy.centroidFailed") end
        squadPosition = findDeploymentPosition(
            surface,
            positionFromScaledDirections(
                TargetPosition,
                1.25,
                squadDirection
            )
        )
        if squadPosition and MetricsG then
            MetricsG.inc("squad.deploy.fallback")
        end
    elseif MetricsG then
        MetricsG.inc("squad.deploy.centroid")
    end

    if not squadPosition then
        return
    end

    local squad = Squad.createSquad(squadPosition, map, nil, not attacker, base)

    local scaledWaveSize
    if attacker then
        scaledWaveSize = attackWaveScaling()
    else
        scaledWaveSize = settlerWaveScaling()
    end
    Queries.formGroupCommand.group = squad.group
    Queries.formCommand.unit_count = scaledWaveSize
    local foundUnits = surface.set_multi_command(Queries.formCommand)
    if (foundUnits == 0) then
        if squad.group.valid then
            squad.group.destroy()
        end
        -- Empty recruitment: back off so units can accumulate (nest spawns
        -- ~1 unit per 4 game-seconds); prevents the empty-group retry storm
        base.nextFormAttemptTick = game.tick + EMPTY_RECRUIT_COOLDOWN
        if MetricsG then
            MetricsG.inc("squad.formEmpty")
            -- Armings counter: the skip counter aggregates across every
            -- active-nest chunk of the base (per-base cooldown, per-chunk
            -- attempts), so skips/armings ~= 200 * activeNests — the
            -- ratio that reconciles the two counters.
            MetricsG.inc("squad.formCooldownArmed")
        end
        return
    end

    -- Kamikaze roll (pure decision): threshold scales with squad fill and
    -- evolution; SIEGE settlers get the ×KAMIKAZE_SIEGE_MULTIPLIER.
    if attacker then
        local kamikazeDecision = SquadLogic.evaluateKamikazeRoll({
            attacker = true,
            siege = (base.stateAI == BASE_AI_STATE_SIEGE),
            memberCount = foundUnits,
            maxSize = Universe.attackWaveMaxSize,
            evolutionLevel = Universe.evolutionLevel,
            roll = Universe.random(),
        })
        squad.kamikaze = kamikazeDecision.kamikaze
        Universe.squadCount = Universe.squadCount + 1
        if not vengence and (base.stateAI == BASE_AI_STATE_AGGRESSIVE) then
            base.sentAggressiveGroups = base.sentAggressiveGroups + 1
        end
    else
        local kamikazeDecision = SquadLogic.evaluateKamikazeRoll({
            attacker = false,
            siege = (base.stateAI == BASE_AI_STATE_SIEGE),
            memberCount = foundUnits,
            maxSize = Universe.expansionMaxSize,
            evolutionLevel = Universe.evolutionLevel,
            roll = Universe.random(),
        })
        squad.kamikaze = kamikazeDecision.kamikaze
        base.sentExpansionGroups = base.sentExpansionGroups + 1
        Universe.builderCount = Universe.builderCount + 1
    end

    squad.rabid = Universe.random() < 0.03

    Universe.groupNumberToSquad[squad.id] = squad
    modifyBaseUnitPoints(base, -cost, name, squadPosition.x, squadPosition.y)
end

function Squad.formSettlers(chunk)
    deploySquad("Settler", chunk, AI_SETTLER_COST, false, false)
end

function Squad.formVengenceSettler(chunk)
    deploySquad("Vengence Settler", chunk, AI_VENGENCE_SETTLER_COST, true, false)
end

function Squad.formSquads(chunk)
    deploySquad("Squad", chunk, AI_SQUAD_COST, false, true)
end

function Squad.formVengenceSquad(chunk)
    deploySquad("Vengence Squad", chunk, AI_VENGENCE_SQUAD_COST, true, true)
end

function Squad.init(universe)
    Universe = universe
    Queries = universe.squadQueries
    if Queries then
        TargetPosition = Queries.targetPosition
        SearchPath = Queries.searchPath
    end
end

SquadG = Squad
return Squad
