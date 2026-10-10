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

-- Pure SquadLogic tests: decay core, raw scorers, predicates, chunk
-- scorers, and decision evaluators. Every function is pure — game.tick is
-- an explicit argument, so tests pin it instead of relying on tick 0.

local TestCollection = require("tests/framework")
local Constants = require("Constants")
local SquadLogic = require("libs/squad_logic")

local my_collection = TestCollection:new()

local DECAY_098 = Constants.DECAY_098
local DECAY_TABLE_DEPTH = Constants.DECAY_TABLE_DEPTH
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE
local PLAYER_PHEROMONE_MULTIPLER = Constants.PLAYER_PHEROMONE_MULTIPLER
local BASE_AI_STATE_SIEGE = Constants.BASE_AI_STATE_SIEGE

-- Module API

my_collection:add('squad_logic_module_loads', function()
    assert(SquadLogic ~= nil, "squad_logic should load")
    assert(type(SquadLogic.evaluateFormationGate) == "function", "should have evaluateFormationGate")
    assert(type(SquadLogic.evaluateSettleTrigger) == "function", "should have evaluateSettleTrigger")
    assert(type(SquadLogic.evaluateKamikazeRoll) == "function", "should have evaluateKamikazeRoll")
end)

-- Pheromone accessors

my_collection:add('playerPheromone_reads_the_source_versioned_field', function()
    SquadLogic.setSourceVersionedFieldReader(function(fieldType)
        if fieldType == PLAYER_PHEROMONE then return 321 end
        return nil
    end)
    assert(SquadLogic.playerPheromone({ [PLAYER_PHEROMONE] = 500 }, 1100) == 321,
        "player must come from the solver, not the chunk slot")
    SquadLogic.setSourceVersionedFieldReader(nil)
    assert(SquadLogic.playerPheromone({ [PLAYER_PHEROMONE] = 500 }, 1100) == 0,
        "no reader must yield 0, not nil")
end)

my_collection:add('baseEnemyResource_read_the_source_versioned_field', function()
    SquadLogic.setSourceVersionedFieldReader(function(fieldType)
        if fieldType == BASE_PHEROMONE then return 77 end
        if fieldType == ENEMY_PHEROMONE then return 88 end
        if fieldType == RESOURCE_PHEROMONE then return 99 end
        return nil
    end)

    -- The chunk slots are ignored: these three fields have no chunk slot any
    -- more, and the accessors must not fall back to reading one.
    local chunk = { [BASE_PHEROMONE] = 1, [ENEMY_PHEROMONE] = 2, [RESOURCE_PHEROMONE] = 3 }
    assert(SquadLogic.basePheromone(chunk, 1050) == 77, "base must come from the solver")
    assert(SquadLogic.enemyPheromone(chunk, 1050) == 88, "enemy must come from the solver")
    assert(SquadLogic.resourcePheromone(chunk, 1050) == 99, "resource must come from the solver")

    SquadLogic.setSourceVersionedFieldReader(nil)
end)

my_collection:add('baseEnemyResource_are_total_without_a_reader', function()
    SquadLogic.setSourceVersionedFieldReader(nil)
    local chunk = {}
    -- nil here would be nil arithmetic in every scorer and navigator downstream.
    assert(SquadLogic.basePheromone(chunk, 0) == 0, "no reader must yield 0, not nil")
    assert(SquadLogic.enemyPheromone(chunk, 0) == 0, "no reader must yield 0, not nil")
    assert(SquadLogic.resourcePheromone(chunk, 0) == 0, "no reader must yield 0, not nil")
end)

-- Raw scorers: canonical prod formulas (exact values)

my_collection:add('settlerFormationRaw_exact', function()
    assert(SquadLogic.settlerFormationRaw(100, 10) == 90, "resource - player")
    assert(SquadLogic.settlerFormationRaw(100, 80) == 20, "penalizes high player presence")
end)

my_collection:add('siegeFormationRaw_exact', function()
    assert(SquadLogic.siegeFormationRaw(100, 50, 20) == 130, "(resource + base) - player")
end)

my_collection:add('unitGroupFormationRaw_exact', function()
    assert(SquadLogic.unitGroupFormationRaw(100, 50) == 150, "player + base")
end)

my_collection:add('resourceMovementRaw_exact', function()
    local expected = 100 - (2 * PLAYER_PHEROMONE_MULTIPLER) - 10
    assert(SquadLogic.resourceMovementRaw(100, 2, 10) == expected, "resource - player*" .. PLAYER_PHEROMONE_MULTIPLER .. " - enemy")
    assert(PLAYER_PHEROMONE_MULTIPLER == 100, "the multiplier constant is load-bearing")
end)

my_collection:add('siegeMovementRaw_exact', function()
    local expected = 50 + (100 * 0.5) + (2 * PLAYER_PHEROMONE_MULTIPLER) - 10
    assert(SquadLogic.siegeMovementRaw(50, 100, 2, 10) == expected, "base + resource*0.5 + player*multiplier - enemy")
end)

my_collection:add('siegeKamikazeMovementRaw_exact', function()
    assert(SquadLogic.siegeKamikazeMovementRaw(30, 100, 10) == 70, "kamikaze + resource*0.5 - enemy")
end)

-- Sign laws (law_form_sign, law_march_sign, prop_formation_avoids, prop_march_seeks)

my_collection:add('formation_strictly_decreases_in_player', function()
    local previous = nil
    for _, p in ipairs({ 0, 1, 5, 50, 500 }) do
        local settler = SquadLogic.settlerFormationRaw(1000, p)
        local siege = SquadLogic.siegeFormationRaw(1000, 100, p)
        if previous then
            assert(settler < previous.settler, "settler formation must strictly decrease as player grows")
            assert(siege < previous.siege, "siege formation must strictly decrease as player grows")
        end
        previous = { settler = settler, siege = siege }
    end
    -- prop_formation_avoids: raw(r,b,p2) < raw(r,b,p1) iff p2 > p1
    assert(SquadLogic.siegeFormationRaw(100, 50, 21) < SquadLogic.siegeFormationRaw(100, 50, 20), "higher player must score lower")
    assert(not (SquadLogic.siegeFormationRaw(100, 50, 19) < SquadLogic.siegeFormationRaw(100, 50, 20)), "lower player must not score lower")
end)

my_collection:add('siegeMovement_slope_plus_100_in_player', function()
    for _, dp in ipairs({ 1, 7 }) do
        local delta = SquadLogic.siegeMovementRaw(50, 100, 5 + dp, 10)
            - SquadLogic.siegeMovementRaw(50, 100, 5, 10)
        assert(delta == PLAYER_PHEROMONE_MULTIPLER * dp, "d(siegeMovement)/d(player) must be +" .. PLAYER_PHEROMONE_MULTIPLER)
    end
end)

-- Predicates (truth tables)

my_collection:add('isPreferredSiegeChunk_truthTable', function()
    assert(SquadLogic.isPreferredSiegeChunk(nil, nil) == true, "no player structures -> preferred")
    assert(SquadLogic.isPreferredSiegeChunk(5, nil) == false, "player base -> not preferred")
    assert(SquadLogic.isPreferredSiegeChunk(nil, 5) == false, "player structure -> not preferred")
    assert(SquadLogic.isPreferredSiegeChunk(5, 5) == false, "both -> not preferred")
    -- 0 is truthy in Lua: a 0 generator count is still a generator field
    assert(SquadLogic.isPreferredSiegeChunk(0, nil) == false, "0 generator is truthy -> not preferred")
end)

my_collection:add('isPreferredResourceChunk_truthTable', function()
    assert(SquadLogic.isPreferredResourceChunk(nil, nil, 10, 0) == true, "resource, no player, no enemy structures")
    assert(SquadLogic.isPreferredResourceChunk(nil, nil, 10, 1) == false, "enemy structures block preference")
    assert(SquadLogic.isPreferredResourceChunk(nil, nil, nil, 0) == false, "no resource -> not preferred")
    assert(SquadLogic.isPreferredResourceChunk(5, nil, 10, 0) == false, "player base blocks")
    assert(SquadLogic.isPreferredResourceChunk(nil, 5, 10, 0) == false, "player structure blocks")
    assert(SquadLogic.isPreferredResourceChunk(nil, nil, false, 0) == false, "false resource -> not preferred")
end)

my_collection:add('evaluateSettleTrigger_emptyResource_truthTable', function()
    -- The empty-resource rule (in-place settle) truth table, evaluated
    -- through the production path (validSettleChunk was deleted as
    -- production-dead: evaluateSettleTrigger rule 3 duplicates it).
    local d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 0, maxDistance = 1000,
        nestCount = nil, hiveCount = nil, resourceGenerator = 10 })
    assert(d.settle and d.reason == SquadLogic.SETTLE_REASON_EMPTY_RESOURCE_CHUNK,
        "empty resource chunk is a settle site")
    d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 0, maxDistance = 1000,
        nestCount = 1, hiveCount = nil, resourceGenerator = 10 })
    assert(not d.settle, "nests block the in-place settle")
    d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 0, maxDistance = 1000,
        nestCount = nil, hiveCount = 1, resourceGenerator = 10 })
    assert(not d.settle, "hives block the in-place settle")
    d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 0, maxDistance = 1000,
        nestCount = nil, hiveCount = nil, resourceGenerator = nil })
    assert(not d.settle, "no resource -> not an in-place settle site")
    -- the prod quirk: a nestCount of 0 is truthy and therefore blocks
    d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 0, maxDistance = 1000,
        nestCount = 0, hiveCount = nil, resourceGenerator = 10 })
    assert(not d.settle, "nestCount 0 is truthy in Lua -> blocks")
end)

my_collection:add('validUnitGroupLocation_truthTable', function()
    assert(SquadLogic.validUnitGroupLocation({ nestCount = 5 }) == false, "nest -> invalid")
    assert(SquadLogic.validUnitGroupLocation({}) == true, "no nestCount field -> valid")
    -- the prod quirk preserved: (not 0) is false in Lua
    assert(SquadLogic.validUnitGroupLocation({ nestCount = 0 }) == false, "nestCount 0 is truthy -> invalid")
end)

-- Chunk scorers (composition: accessors + predicates + raws)

my_collection:add('scoreResourceMovementChunk_composition', function()
    local chunk = {
        [RESOURCE_PHEROMONE] = 100,
        [PLAYER_PHEROMONE] = 2,
        [ENEMY_PHEROMONE] = 10,
        resourceLastRefresh = 1000,
        playerLastRefresh = 1000,
        enemyLastRefresh = 1000,
        resourceGenerator = 10,
    }
    local tick = 1000
    local expected = SquadLogic.resourceMovementRaw(
        SquadLogic.resourcePheromone(chunk, tick),
        SquadLogic.playerPheromone(chunk, tick),
        SquadLogic.enemyPheromone(chunk, tick))
    local score, preferred = SquadLogic.scoreResourceMovementChunk(chunk, tick)
    assert(score == expected, "score must compose the accessors through the raw formula")
    assert(preferred == true, "resource chunk without player/enemy structures is preferred")
    chunk.playerBaseGenerator = 5
    local _, blocked = SquadLogic.scoreResourceMovementChunk(chunk, tick)
    assert(blocked == false, "player base generator blocks preference")
end)

my_collection:add('scoreSiegeMovementChunk_composition', function()
    local chunk = {
        [BASE_PHEROMONE] = 50,
        [RESOURCE_PHEROMONE] = 100,
        [PLAYER_PHEROMONE] = 2,
        [ENEMY_PHEROMONE] = 10,
        baseLastRefresh = 1000,
        resourceLastRefresh = 1000,
        playerLastRefresh = 1000,
        enemyLastRefresh = 1000,
    }
    local tick = 1000
    local expected = SquadLogic.siegeMovementRaw(
        SquadLogic.basePheromone(chunk, tick),
        SquadLogic.resourcePheromone(chunk, tick),
        SquadLogic.playerPheromone(chunk, tick),
        SquadLogic.enemyPheromone(chunk, tick))
    local score, preferred = SquadLogic.scoreSiegeMovementChunk(chunk, tick)
    assert(score == expected, "score must compose the accessors through the raw formula")
    assert(preferred == true, "no player structures -> preferred")
    chunk.playerGenerator = 5
    local _, blocked = SquadLogic.scoreSiegeMovementChunk(chunk, tick)
    assert(blocked == false, "player generator blocks preference")
end)

my_collection:add('scoreSiegeKamikazeMovementChunk_readsKamikazeRaw', function()
    local chunk = {
        [KAMIKAZE_PHEROMONE] = 30,
        [RESOURCE_PHEROMONE] = 100,
        [ENEMY_PHEROMONE] = 10,
        -- stale timestamps: K and R must still be RAW (law_kamikaze_raw)
        resourceLastRefresh = 0,
        enemyLastRefresh = 1000,
    }
    local tick = 1000
    local expected = SquadLogic.siegeKamikazeMovementRaw(30, 100, SquadLogic.enemyPheromone(chunk, tick))
    local score, preferred = SquadLogic.scoreSiegeKamikazeMovementChunk(chunk, tick)
    assert(score == expected, "kamikaze/resource raw + decayed enemy")
    assert(preferred == true, "no player structures -> preferred")
end)

my_collection:add('formationChunkScorers_preferredAlwaysTrue', function()
    local chunk = {
        [RESOURCE_PHEROMONE] = 100,
        [BASE_PHEROMONE] = 50,
        [PLAYER_PHEROMONE] = 20,
        resourceLastRefresh = 0,
        baseLastRefresh = 0,
        playerLastRefresh = 0,
    }
    local tick = 0
    local settlerScore, settlerPreferred = SquadLogic.scoreSettlerFormationChunk(chunk, tick)
    assert(settlerScore == SquadLogic.settlerFormationRaw(
        SquadLogic.resourcePheromone(chunk, tick),
        SquadLogic.playerPheromone(chunk, tick)), "settler formation composition")
    assert(settlerPreferred == true, "formation scorers have no preferred logic")
    local siegeScore, siegePreferred = SquadLogic.scoreSiegeFormationChunk(chunk, tick)
    assert(siegeScore == SquadLogic.siegeFormationRaw(
        SquadLogic.resourcePheromone(chunk, tick),
        SquadLogic.basePheromone(chunk, tick),
        SquadLogic.playerPheromone(chunk, tick)), "siege formation composition")
    assert(siegePreferred == true, "formation scorers have no preferred logic")
    local groupScore, groupPreferred = SquadLogic.scoreUnitGroupFormationChunk(chunk, tick)
    assert(groupScore == SquadLogic.unitGroupFormationRaw(
        SquadLogic.playerPheromone(chunk, tick),
        SquadLogic.basePheromone(chunk, tick)), "unit group formation composition")
    assert(groupPreferred == true, "formation scorers have no preferred logic")
end)

-- Formation gate (G_* rules; prop_metrics_unchanged)

my_collection:add('formationGate_attackerPoints_reject', function()
    local d = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = false,
        unitPoints = 100, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 0, squadLimit = 10,
        formThreshold = 1, roll = 0,
    })
    assert(d.accepted == false, "lacking points must reject")
    assert(d.metric == "squad.formReject.attacker.points", "exact counter name")
end)

my_collection:add('formationGate_attackerPheromone_reject', function()
    local d = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = false,
        unitPoints = 1000, cost = 175,
        basePheromone = 0.00005, playerPheromone = 0.00005,
        squadCount = 0, squadLimit = 10,
        formThreshold = 1, roll = 0,
    })
    assert(d.accepted == false and d.metric == "squad.formReject.attacker.pheromone", "quiet map must reject")
    -- boundary: exactly 0.0001 passes (not <)
    local dPass = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = true,
        unitPoints = 1000, cost = 175,
        basePheromone = 0.0001, playerPheromone = 0.0001,
        squadCount = 0, squadLimit = 10,
        formThreshold = 1, roll = 0,
    })
    assert(dPass.accepted == true, "base/player at exactly 0.0001 must pass")
end)

my_collection:add('formationGate_attackerSquadLimit_reject', function()
    local d = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = false,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 11, squadLimit = 10,
        formThreshold = 1, roll = 0,
    })
    assert(d.accepted == false and d.metric == "squad.formReject.attacker.squadLimit", "squad limit must reject")
    local dPass = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = true,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 10, squadLimit = 10,
        formThreshold = 1, roll = 0,
    })
    assert(dPass.accepted == true, "squadCount == limit must pass (not >)")
end)

my_collection:add('formationGate_attackerCandidate_reject', function()
    local d = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = false,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 0, squadLimit = 10,
        hasActiveCandidate = false,
        formThreshold = 1, roll = 0,
    })
    assert(d.accepted == false and d.metric == "squad.formReject.attacker.candidate", "no candidate must reject")
    local dPass = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = false,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 0, squadLimit = 10,
        hasActiveCandidate = true,
        formThreshold = 1, roll = 0,
    })
    assert(dPass.accepted == true, "active candidate must pass")
    local dVengence = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = true,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 0, squadLimit = 10,
        hasActiveCandidate = false,
        formThreshold = 1, roll = 0,
    })
    assert(dVengence.accepted == true, "vengence skips the candidate gate")
end)

my_collection:add('formationGate_attackerThreshold_boundary', function()
    local d = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = true,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 0, squadLimit = 10,
        formThreshold = 0.70, roll = 0.71,
    })
    assert(d.accepted == false and d.metric == "squad.formReject.attacker.threshold", "roll > threshold must reject")
    local dBoundary = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = true,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 0, squadLimit = 10,
        formThreshold = 0.70, roll = 0.70,
    })
    assert(dBoundary.accepted == true, "roll == threshold must PASS")
    local dBelow = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = true,
        unitPoints = 1000, cost = 175,
        basePheromone = 1, playerPheromone = 1,
        squadCount = 0, squadLimit = 10,
        formThreshold = 0.70, roll = 0.69,
    })
    assert(dBelow.accepted == true, "roll < threshold must pass")
end)

my_collection:add('formationGate_settlerGates_reject', function()
    local base = {
        attacker = false, vengence = false,
        unitPoints = 1000, cost = 300,
        builderCount = 0, builderLimit = 5,
        sentExpansionGroups = 0, maxExpansionGroups = 1,
        formThreshold = 1, roll = 0,
    }
    local d = SquadLogic.evaluateFormationGate(base)
    assert(d.accepted == true, "clean settler params must accept")
    assert(d.metric == nil, "accepted decisions carry no metric")

    base.unitPoints = 299
    assert(SquadLogic.evaluateFormationGate(base).metric == "squad.formReject.settler.points", "settler points gate")
    base.unitPoints = 1000

    base.builderCount = 6
    base.builderLimit = 5
    assert(SquadLogic.evaluateFormationGate(base).metric == "squad.formReject.settler.builder", "builder limit gate")
    base.builderCount = 5
    base.builderLimit = 5
    assert(SquadLogic.evaluateFormationGate(base).accepted == true, "builderCount == limit passes (not >)")

    base.sentExpansionGroups = 1
    base.maxExpansionGroups = 1
    assert(SquadLogic.evaluateFormationGate(base).metric == "squad.formReject.settler.groups", "expansion quota gate")
    base.maxExpansionGroups = 2
    assert(SquadLogic.evaluateFormationGate(base).accepted == true, "sent < max passes")

    base.formThreshold = 0.70
    base.roll = 0.71
    assert(SquadLogic.evaluateFormationGate(base).metric == "squad.formReject.settler.threshold", "settler threshold gate")
end)

my_collection:add('formationGate_gatePrecedence', function()
    -- multiple failures: the first gate in the chain wins
    local d = SquadLogic.evaluateFormationGate({
        attacker = true, vengence = false,
        unitPoints = 0, cost = 175,
        basePheromone = 0, playerPheromone = 0,
        squadCount = 99, squadLimit = 10,
        hasActiveCandidate = false,
        formThreshold = 0, roll = 1,
    })
    assert(d.metric == "squad.formReject.attacker.points", "points gate is first in the attacker chain")
    local dSettler = SquadLogic.evaluateFormationGate({
        attacker = false, vengence = false,
        unitPoints = 0, cost = 300,
        builderCount = 99, builderLimit = 5,
        sentExpansionGroups = 9, maxExpansionGroups = 1,
        formThreshold = 0, roll = 1,
    })
    assert(dSettler.metric == "squad.formReject.settler.points", "points gate is first in the settler chain")
end)

my_collection:add('formationGate_metricNames_verbatim', function()
    -- prop_metrics_unchanged: the counter strings must match the existing
    -- Metrics counters verbatim (deliberate duplication — catches renames)
    local expected = {
        "squad.formReject.attacker.points",
        "squad.formReject.attacker.pheromone",
        "squad.formReject.attacker.squadLimit",
        "squad.formReject.attacker.candidate",
        "squad.formReject.attacker.threshold",
        "squad.formReject.settler.points",
        "squad.formReject.settler.builder",
        "squad.formReject.settler.groups",
        "squad.formReject.settler.threshold",
    }
    local function gateWith(metric)
        local params = {
            attacker = true, vengence = true,
            unitPoints = 1000, cost = 175,
            basePheromone = 1, playerPheromone = 1,
            squadCount = 0, squadLimit = 10,
            builderCount = 0, builderLimit = 5,
            sentExpansionGroups = 0, maxExpansionGroups = 1,
            hasActiveCandidate = true,
            formThreshold = 1, roll = 0,
        }
        if metric == "squad.formReject.attacker.points" then
            params.unitPoints = 0
        elseif metric == "squad.formReject.attacker.pheromone" then
            params.basePheromone = 0
            params.playerPheromone = 0
        elseif metric == "squad.formReject.attacker.squadLimit" then
            params.squadCount = 99
        elseif metric == "squad.formReject.attacker.candidate" then
            params.vengence = false
            params.hasActiveCandidate = false
        elseif metric == "squad.formReject.attacker.threshold" then
            params.formThreshold = 0
            params.roll = 1
        elseif metric == "squad.formReject.settler.points" then
            params.attacker = false
            params.unitPoints = 0
            params.cost = 300
        elseif metric == "squad.formReject.settler.builder" then
            params.attacker = false
            params.builderCount = 99
        elseif metric == "squad.formReject.settler.groups" then
            params.attacker = false
            params.vengence = false
            params.sentExpansionGroups = 9
            params.maxExpansionGroups = 1
        elseif metric == "squad.formReject.settler.threshold" then
            params.attacker = false
            params.formThreshold = 0
            params.roll = 1
        end
        return SquadLogic.evaluateFormationGate(params)
    end
    for _, metric in ipairs(expected) do
        local d = gateWith(metric)
        assert(d.accepted == false, metric .. " must be reachable")
        assert(d.metric == metric, "metric must be verbatim: " .. tostring(d.metric))
    end
end)

-- Scorer selection identity

my_collection:add('selectFormationScorer_identity', function()
    assert(SquadLogic.selectFormationScorer(true, 1) == SquadLogic.scoreUnitGroupFormationChunk,
        "attacker -> unitGroupFormation")
    assert(SquadLogic.selectFormationScorer(false, BASE_AI_STATE_SIEGE) == SquadLogic.scoreSiegeFormationChunk,
        "settler in SIEGE -> siegeFormation")
    assert(SquadLogic.selectFormationScorer(false, 5) == SquadLogic.scoreSettlerFormationChunk,
        "settler not in SIEGE -> settlerFormation")
    assert(SquadLogic.selectFormationScorer(false, nil) == SquadLogic.scoreSettlerFormationChunk,
        "nil state -> settlerFormation")
end)

my_collection:add('selectSettleScorer_identity', function()
    assert(SquadLogic.selectSettleScorer(BASE_AI_STATE_SIEGE, true) == SquadLogic.scoreSiegeKamikazeMovementChunk,
        "siege + kamikaze -> siegeKamikazeMovement")
    assert(SquadLogic.selectSettleScorer(BASE_AI_STATE_SIEGE, false) == SquadLogic.scoreSiegeMovementChunk,
        "siege -> siegeMovement")
    assert(SquadLogic.selectSettleScorer(5, true) == SquadLogic.scoreResourceMovementChunk,
        "non-siege -> resourceMovement")
    assert(SquadLogic.selectSettleScorer(nil, false) == SquadLogic.scoreResourceMovementChunk,
        "nil legacy type -> resourceMovement")
end)

-- Distraction selection

my_collection:add('selectSettleDistraction_identity', function()
    assert(SquadLogic.selectSettleDistraction(true) == SquadLogic.DISTRACTION_NONE, "kamikaze -> none")
    assert(SquadLogic.selectSettleDistraction(false) == SquadLogic.DISTRACTION_BY_ENEMY, "plain -> by_enemy")
    assert(SquadLogic.DISTRACTION_NONE == "none", "distraction key shape")
    assert(SquadLogic.DISTRACTION_BY_ENEMY == "by_enemy", "distraction key shape")
end)

my_collection:add('selectMoveDistraction_identity', function()
    assert(SquadLogic.selectMoveDistraction(false, false) == SquadLogic.DISTRACTION_BY_ENEMY, "plain -> by_enemy")
    assert(SquadLogic.selectMoveDistraction(true, false) == SquadLogic.DISTRACTION_NONE, "rabid -> none")
    assert(SquadLogic.selectMoveDistraction(false, true) == SquadLogic.DISTRACTION_NONE, "kamikaze -> none")
    assert(SquadLogic.selectMoveDistraction(true, true) == SquadLogic.DISTRACTION_NONE, "rabid+kamikaze -> none")
end)

-- Settle trigger (T_Settle* rules)

my_collection:add('settleTrigger_maxDistance_boundary', function()
    local d = SquadLogic.evaluateSettleTrigger({ distanceToOrigin = 100, maxDistance = 100 })
    assert(d.settle == true, "distance == maxDistance must settle (>=)")
    assert(d.reason == SquadLogic.SETTLE_REASON_MAX_DISTANCE, "reason max_distance")
    local dOver = SquadLogic.evaluateSettleTrigger({ distanceToOrigin = 101, maxDistance = 100 })
    assert(dOver.settle == true and dOver.reason == SquadLogic.SETTLE_REASON_MAX_DISTANCE, "distance > maxDistance settles")
    local dUnder = SquadLogic.evaluateSettleTrigger({ distanceToOrigin = 99, maxDistance = 100 })
    assert(dUnder.settle == false, "distance < maxDistance does not settle")
end)

my_collection:add('settleTrigger_emptyResourceChunk', function()
    local d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 10, maxDistance = 100,
        resourceGenerator = 10, nestCount = nil, hiveCount = nil,
    })
    assert(d.settle == true and d.reason == SquadLogic.SETTLE_REASON_EMPTY_RESOURCE_CHUNK, "empty resource chunk settles")
    assert(SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 10, maxDistance = 100,
        resourceGenerator = 10, nestCount = 1, hiveCount = nil,
    }).settle == false, "nests block in-place settle")
    assert(SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 10, maxDistance = 100,
        resourceGenerator = 10, nestCount = nil, hiveCount = 1,
    }).settle == false, "hives block in-place settle")
    assert(SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 10, maxDistance = 100,
        resourceGenerator = nil, nestCount = nil, hiveCount = nil,
    }).settle == false, "no resource -> keep marching")
end)

my_collection:add('settleTrigger_precedence', function()
    local d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 100, maxDistance = 100,
        resourceGenerator = 10, nestCount = nil, hiveCount = nil,
    })
    assert(d.reason == SquadLogic.SETTLE_REASON_MAX_DISTANCE, "max_distance wins over empty_resource_chunk (prod or-order)")
end)

my_collection:add('settleTrigger_pathTerminated', function()
    local d = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 10, maxDistance = 100,
        pathEndIndex = 2, pathLength = 4,
    })
    assert(d.settle == true and d.reason == SquadLogic.SETTLE_REASON_PATH_TERMINATED, "short path terminates the march")
    -- boundary: pathEndIndex == pathLength does NOT settle
    local dFull = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 10, maxDistance = 100,
        pathEndIndex = 4, pathLength = 4,
    })
    assert(dFull.settle == false, "full-length path continues the march")
    -- path rule takes precedence over the in-place rules
    local dPathFirst = SquadLogic.evaluateSettleTrigger({
        distanceToOrigin = 100, maxDistance = 100,
        pathEndIndex = 1, pathLength = 4,
    })
    assert(dPathFirst.reason == SquadLogic.SETTLE_REASON_PATH_TERMINATED, "path rule fires first")
    -- nil pathEndIndex is inert (pre-path call site)
    local dNoPath = SquadLogic.evaluateSettleTrigger({ distanceToOrigin = 10, maxDistance = 100 })
    assert(dNoPath.settle == false, "nil pathEndIndex must not settle")
end)

-- Kamikaze roll (R_Kamikaze* rules)

my_collection:add('kamikazeRoll_attackerThresholds', function()
    local d = SquadLogic.evaluateKamikazeRoll({
        attacker = true, siege = false,
        memberCount = 100, maxSize = 200, evolutionLevel = 0.5, roll = 0.19,
    })
    assert(d.threshold == 0.2, "(100/200)*0.2 + 0.5*0.2 = 0.2")
    assert(d.kamikaze == true, "roll below threshold is kamikaze")
    local dBoundary = SquadLogic.evaluateKamikazeRoll({
        attacker = true, siege = false,
        memberCount = 100, maxSize = 200, evolutionLevel = 0.5, roll = 0.2,
    })
    assert(dBoundary.kamikaze == false, "roll == threshold is NOT kamikaze (strict <)")
    local dHigh = SquadLogic.evaluateKamikazeRoll({
        attacker = true, siege = false,
        memberCount = 100, maxSize = 200, evolutionLevel = 1.0, roll = 0.31,
    })
    assert(math.abs(dHigh.threshold - 0.3) < 1e-12, "evo 1.0 -> 0.3")
    assert(dHigh.kamikaze == false, "roll above threshold is not kamikaze")
    local dLow = SquadLogic.evaluateKamikazeRoll({
        attacker = true, siege = false,
        memberCount = 50, maxSize = 200, evolutionLevel = 0.5, roll = 0.14,
    })
    assert(math.abs(dLow.threshold - 0.15) < 1e-12, "fewer members -> lower threshold")
    assert(dLow.kamikaze == true, "roll below threshold is kamikaze")
end)

my_collection:add('kamikazeRoll_settlerNonSiege_passthrough', function()
    local d = SquadLogic.evaluateKamikazeRoll({
        attacker = false, siege = false,
        memberCount = 100, maxSize = 150, evolutionLevel = 0.5, roll = 0.23,
    })
    local expected = (100 / 150) * 0.2 + 0.5 * 0.2
    assert(math.abs(d.threshold - expected) < 1e-12, "no multiplier outside SIEGE")
    assert(d.kamikaze == true, "roll below the plain threshold")
end)

my_collection:add('kamikazeRoll_siegeSettler_multiplied', function()
    local d = SquadLogic.evaluateKamikazeRoll({
        attacker = false, siege = true,
        memberCount = 100, maxSize = 150, evolutionLevel = 0.5, roll = 0.58,
    })
    local plain = (100 / 150) * 0.2 + 0.5 * 0.2
    assert(math.abs(d.threshold - plain * 2.5) < 1e-12, "SIEGE settler threshold is x2.5")
    assert(d.kamikaze == true, "roll below the multiplied threshold")
    local dOver = SquadLogic.evaluateKamikazeRoll({
        attacker = false, siege = true,
        memberCount = 100, maxSize = 150, evolutionLevel = 0.5, roll = 0.59,
    })
    assert(dOver.kamikaze == false, "roll above the multiplied threshold")
    -- boundary at the multiplied threshold: strict <
    local dBoundary = SquadLogic.evaluateKamikazeRoll({
        attacker = false, siege = true,
        memberCount = 100, maxSize = 150, evolutionLevel = 0.5, roll = plain * 2.5,
    })
    assert(dBoundary.kamikaze == false, "roll == multiplied threshold is NOT kamikaze")
end)

my_collection:add('kamikazeRoll_siegeAttacker_noMultiplier', function()
    local d = SquadLogic.evaluateKamikazeRoll({
        attacker = true, siege = true,
        memberCount = 100, maxSize = 200, evolutionLevel = 0.5, roll = 0.25,
    })
    assert(d.threshold == 0.2, "the multiplier requires a SETTLER (not attacker)")
    assert(d.kamikaze == false, "roll above the plain threshold")
end)

return my_collection
