local tests = {}

local constants = require("libs/Constants")
local mathUtils = require("libs/MathUtils")
local chunkUtils = require("libs/ChunkUtils")
local chunkPropertyUtils = require("libs/ChunkPropertyUtils")
local mapUtils = require("libs/MapUtils")
local baseUtils = require("libs/BaseUtils")
-- local tendrilUtils = require("libs/TendrilUtils")

function tests.pheromoneLevels(size)
    local player = game.player.character
    local playerChunkX = math.floor(player.position.x / 32) * constants.CHUNK_SIZE
    local playerChunkY = math.floor(player.position.y / 32) * constants.CHUNK_SIZE
    if not size then
	size = 3 * constants.CHUNK_SIZE
    else
	size = size * constants.CHUNK_SIZE
    end
    print("------")
    print(#global.map.processQueue)
    print(playerChunkX .. ", " .. playerChunkY)
    print("--")
    for y=playerChunkY-size, playerChunkY+size,32 do
	for x=playerChunkX-size, playerChunkX+size,32 do
            if (global.map[x] ~= nil) then
                local chunk = global.map[x][y]
                if (chunk ~= nil) then
                    local str = ""
                    for i=1,#chunk do
                        str = str .. " " .. tostring(i) .. "/" .. tostring(chunk[i])
                    end
		    str = str .. " " .. "p/" .. game.surfaces[global.natives.activeSurface].get_pollution(chunk) .. " " .. "n/" .. chunkPropertyUtils.getNestCount(global.map, chunk) .. " " .. "w/" .. chunkPropertyUtils.getWormCount(global.map, chunk)
		    if (chunk.x == playerChunkX) and (chunk.y == playerChunkY) then
			print("=============")
			print(chunk.x, chunk.y, str)
			print("=============")
		    else
			print(chunk.x, chunk.y, str)
		    end
		    -- print(str)
		    print("----")
                end
            end
        end
	print("------------------")
    end
end

function tests.killActiveSquads()
    print("--")
    for i=1, #global.natives.squads do
	local squad = global.natives.squads[i]
	if (squad.group.valid) then
	    local members = squad.group.members
	    for x=1, #members do
		local member = members[x]
		local val = member.valid and member.die()
	    end
	end
    end
end

function tests.activeSquads()
    print("-----")
    print("Squads", #global.natives.squads)
    for i=1, #global.natives.squads do
	print("-")
        local squad = global.natives.squads[i]
	local squadHealth = 0
	local squadMakeup = {}
        if squad.group.valid then
	    for x=1,#squad.group.members do
		local member = squad.group.members[x].prototype
		if not squadMakeup[member.name] then
		    squadMakeup[member.name] = 0
		end

		squadHealth = squadHealth + member.max_health
		squadMakeup[member.name] = squadMakeup[member.name] + 1
	    end
            print(math.floor(squad.group.position.x * 0.03125), math.floor(squad.group.position.y * 0.03125), squad.status, squad.group.state, #squad.group.members, squad.cycles, squadHealth, squad.group.group_number)
	    -- print(serpent.dump(squadResistances))
	    print(serpent.dump(squadMakeup))
            print(serpent.dump(squad))
        end
    end
    print("---")
    print("pending", #global.natives.pendingAttack)    
    for i=1, #global.natives.pendingAttack do
	print("-")
        local squad = global.natives.pendingAttack[i]
	local squadHealth = 0
	local squadMakeup = {}
        if squad.group.valid then
	    for x=1,#squad.group.members do
		local member = squad.group.members[x].prototype
		if not squadMakeup[member.name] then
		    squadMakeup[member.name] = 0
		end

		squadHealth = squadHealth + member.max_health
		squadMakeup[member.name] = squadMakeup[member.name] + 1
	    end
            print(math.floor(squad.group.position.x * 0.03125), math.floor(squad.group.position.y * 0.03125), squad.status, squad.group.state, #squad.group.members, squad.cycles, squadHealth, squad.group.group_number)
	    -- print(serpent.dump(squadResistances))
	    print(serpent.dump(squadMakeup))
            print(serpent.dump(squad))
        end
    end
    print("---")
    print("building", #global.natives.building)
    for i=1, #global.natives.building do
	print("-")
        local squad = global.natives.building[i]
	local squadHealth = 0
	local squadMakeup = {}
        if squad.group.valid then
	    for x=1,#squad.group.members do
		local member = squad.group.members[x].prototype
		if not squadMakeup[member.name] then
		    squadMakeup[member.name] = 0
		end

		squadHealth = squadHealth + member.max_health
		squadMakeup[member.name] = squadMakeup[member.name] + 1
	    end
            print(math.floor(squad.group.position.x * 0.03125), math.floor(squad.group.position.y * 0.03125), squad.status, squad.group.state, #squad.group.members, squad.cycles, squadHealth, squad.group.group_number)
	    -- print(serpent.dump(squadResistances))
	    print(serpent.dump(squadMakeup))
            print(serpent.dump(squad))
        end
    end        
end

function tests.entitiesOnPlayerChunk()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local entities = game.surfaces[global.natives.activeSurface].find_entities_filtered({area={{chunkX, chunkY},
											     {chunkX + constants.CHUNK_SIZE, chunkY + constants.CHUNK_SIZE}},
											 force="player"})
    for i=1, #entities do
        print(entities[i].name)
    end
    print("--")
end

function tests.findNearestPlayerEnemy()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local entity = game.surfaces[global.natives.activeSurface].find_nearest_enemy({position={chunkX, chunkY},
										   max_distance=constants.CHUNK_SIZE,
										   force = "enemy"})
    if (entity ~= nil) then
        print(entity.name)
    end
    print("--")
end

function tests.morePoints(points)
    global.natives.points = global.natives.points + points
end

function tests.getOffsetChunk(x, y)
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125)
    local chunkY = math.floor(playerPosition.y * 0.03125)
    local chunk = mapUtils.getChunkByIndex(global.map, chunkX + x, chunkY + y)
    print(serpent.dump(chunk))
end

function tests.aiStats()
    print(global.natives.points, game.tick, global.natives.state, global.natives.temperament, global.natives.stateTick, global.natives.temperamentTick)
end

function tests.fillableDirtTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[global.natives.activeSurface].set_tiles({{name="fillableDirt", position={chunkX-1, chunkY-1}},
	    {name="fillableDirt", position={chunkX, chunkY-1}},
	    {name="fillableDirt", position={chunkX-1, chunkY}},
	    {name="fillableDirt", position={chunkX, chunkY}}},
	false)
end

function tests.tunnelTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[global.natives.activeSurface].create_entity({name="tunnel-entrance-rampant", position={chunkX, chunkY}})
end

function tests.createEnemy(x,d)
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local a = {name=x, position={chunkX, chunkY}, force="enemy"}
    if d then
	a['direction'] = d
    end
    return game.surfaces[global.natives.activeSurface].create_entity(a)
end

function tests.registeredNest(x)
    local entity = tests.createEnemy(x)
    chunk.registerEnemyBaseStructure(global.map,
				     entity,
				     nil)
end

function tests.attackOrigin()
    local enemy = game.surfaces[global.natives.activeSurface].find_nearest_enemy({position={0,0},
										  max_distance = 1000})
    if (enemy ~= nil) and enemy.valid then
        print(enemy, enemy.unit_number)
        enemy.set_command({type=defines.command.attack_area,
                           destination={0,0},
                           radius=32})
    end
end

function tests.dumpNatives()
    print(serpent.dump(global.natives))
end

function tests.cheatMode()
    game.players[1].cheat_mode = true
    game.forces.player.research_all_technologies()
end

function tests.gaussianRandomTest()
    local result = {}
    for x=0,100,1 do
    	result[x] = 0
    end
    for _=1,10000 do
	local s = mathUtils.roundToNearest(mathUtils.gaussianRandomRange(50, 25, 0, 100), 1)
	result[s] = result[s] + 1
    end
    for x=0,100,1 do
    	print(x, result[x])
    end
end

function tests.reveal (size)
    local pos = game.player.character.position
    game.player.force.chart(game.player.surface,
			    {{x=-size+pos.x, y=-size+pos.y}, {x=size+pos.x, y=size+pos.y}})
end

function tests.baseStats()
    local natives = global.natives
    print ("cX", "cY", "pX", "pY", "created", "align", "str", "upgradePoints", "#nest", "#worms", "#eggs", "hive")
    for i=1, #natives.bases do
	local base = natives.bases[i]
	local nestCount = 0
	local wormCount = 0
	local eggCount = 0
	local hiveCount = 0
	for _,_ in pairs(base.nests) do
	    nestCount = nestCount + 1
	end
	for _,_ in pairs(base.worms) do
	    wormCount = wormCount + 1
	end
	for _,_ in pairs(base.eggs) do
	    eggCount = eggCount + 1
	end
	for _,_ in pairs(base.hives) do
	    hiveCount = hiveCount + 1
	end
	print(base.x, base.y, base.created, base.alignment, base.strength, base.upgradePoints, nestCount, wormCount, eggCount, hiveCount)
	print(serpent.dump(base.tendrils))
	print("---")
    end
end

function tests.clearBases()

    local surface = game.surfaces[global.natives.activeSurface]
    for x=#global.natives.bases,1,-1 do
	local base = global.natives.bases[x]
	for c=1,#base.chunks do
	    local chunk = base.chunks[c]
	    chunkUtils.clearChunkNests(chunk, surface)
	end

	base.chunks = {}

	if (surface.can_place_entity({name="biter-spawner-powered", position={base.cX * 32, base.cY * 32}})) then
	    surface.create_entity({name="biter-spawner-powered", position={base.cX * 32, base.cY * 32}})
	    local slice = math.pi / 12
	    local pos = 0
	    for i=1,24 do
		if (math.random() < 0.8) then
		    local distance = mathUtils.roundToNearest(mathUtils.gaussianRandomRange(45, 5, 37, 60), 1)
		    if (surface.can_place_entity({name="biter-spawner", position={base.cX * 32 + (distance*math.sin(pos)), base.cY * 32 + (distance*math.cos(pos))}})) then
			if (math.random() < 0.3) then
			    surface.create_entity({name="small-worm-turret", position={base.cX * 32 + (distance*math.sin(pos)), base.cY * 32 + (distance*math.cos(pos))}})
			else
			    surface.create_entity({name="biter-spawner", position={base.cX * 32 + (distance*math.sin(pos)), base.cY * 32 + (distance*math.cos(pos))}})
			end
		    end
		end
		pos = pos + slice
	    end
	else
	    table.remove(global.natives.bases, x)
	end
    end
end

function tests.mergeBases()
    local natives = global.natives
    baseUtils.mergeBases(natives)
end

function tests.unitBuildBase()
end

function tests.showBaseGrid()
    local n = {}

    for k,v in pairs(global.natives.bases) do
	n[v] = k % 4
    end

    local chunks = global.map.chunkToBase
    for chunk,base in pairs(chunks) do
	local pick = n[base]
	local color = "concrete"
	if  base.alignment == constants.BASE_ALIGNMENT_NE then
	    color = "hazard-concrete-left"
	elseif (pick == 1) then
	    color = "water"
	elseif (pick == 2) then
	    color = "deepwater"
	elseif (pick == 3) then
	    color = "water-green"
	end
	chunkUtils.colorChunk(chunk.x, chunk.y, color, game.surfaces[global.natives.activeSurface])
    end
end

function tests.showMovementGrid()
    local chunks = global.map.processQueue
    for i=1,#chunks do
	local chunk = chunks[i]
	local color = "concrete"
	if (chunkPropertyUtils.getPassable(global.map, chunk) == constants.CHUNK_ALL_DIRECTIONS) then
	    color = "hazard-concrete-left"
	elseif (chunkPropertyUtils.getPassable(global.map, chunk) == constants.CHUNK_NORTH_SOUTH) then
	    color = "deepwater"
	elseif (chunkPropertyUtils.getPassable(global.map, chunk) == constants.CHUNK_EAST_WEST) then
	    color = "water-green"
	end
	chunkUtils.colorChunk(chunk.x, chunk.y, color, game.surfaces[global.natives.activeSurface])
    end
end

function tests.colorResourcePoints()
    local chunks = global.map.processQueue
    for i=1,#chunks do
	local chunk = chunks[i]
	local color = "concrete"
	if (chunk[constants.RESOURCE_GENERATOR] ~= 0) and (chunk[constants.NEST_COUNT] ~= 0) then
	    color = "hazard-concrete-left"
	elseif (chunk[constants.RESOURCE_GENERATOR] ~= 0) then
	    color = "deepwater"
	elseif (chunk[constants.NEST_COUNT] ~= 0) then
	    color = "water-green"
	end
	chunkUtils.colorChunk(chunk.x, chunk.y, color, game.surfaces[global.natives.activeSurface])
    end
end

function tests.entityStats(name, d)
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local a = game.surfaces[global.natives.activeSurface].create_entity({name=name, position={chunkX, chunkY}})
    if d then
	a['direction'] = d
    end
    print(serpent.dump(a))
    a.destroy()
end

function tests.exportAiState()

    local printState = function ()
	local chunks = global.map.processQueue
	local s = ""
	for i=1,#chunks do
	    local chunk = chunks[i]

	    s = s .. table.concat({chunk[constants.MOVEMENT_PHEROMONE],
				   chunk[constants.BASE_PHEROMONE],
				   chunk[constants.PLAYER_PHEROMONE],
				   chunk[constants.RESOURCE_PHEROMONE],
				   chunkPropertyUtils.getPassable(global.map, chunk),
				   chunk[constants.CHUNK_TICK],
				   chunkPropertyUtils.getPathRating(global.map, chunk),
				   chunk.x,
				   chunk.y,
				   chunkPropertyUtils.getNestCount(global.map, chunk),
				   chunkPropertyUtils.getWormCount(global.map, chunk),
				   chunkPropertyUtils.getRallyTick(global.map, chunk),
				   chunkPropertyUtils.getRetreatTick(global.map, chunk),
				   chunkPropertyUtils.getResourceGenerator(global.map, chunk),
				   chunkPropertyUtils.getPlayerBaseGenerator(global.map, chunk),
				   chunkPropertyUtils.getDeathGenerator(global.map, chunk),
                                   game.surfaces[global.natives.activeSurface].get_pollution(chunk),
                                   chunkPropertyUtils.getNestActiveness(global.map, chunk),
                                   chunkPropertyUtils.getRaidNestActiveness(global.map, chunk),
                                   #chunkPropertyUtils.getSquadsOnChunk(global.map, chunk)}, ",") .. "\n"
	end
	game.write_file("rampantState.txt", s, false)
    end

    return function(interval)
	if not interval then
	    interval = 0
	else
	    interval = tonumber(interval)
	end

	printState()

	if (interval > 0) then
	    script.on_nth_tick(interval, printState)
	end
    end
end

function tests.createEnergyTest(x)
    local entity = tests.createEnemy(x)

    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local entities = game.surfaces[global.natives.activeSurface].find_entities_filtered({area={{chunkX, chunkY},
											     {chunkX + constants.CHUNK_SIZE, chunkY + constants.CHUNK_SIZE}},
											 type = "electric-pole",
											 force="player"})
    -- for i=1, #entities do
    --     print(entities[i].name)
    -- end
    local wires

    if #entities > 0 then
	entity.connect_neighbour(entities[1])
    end

    --     if wires then
    -- 	for connectType,neighbourGroup in pairs(wires) do
    -- 	    if connectType == "copper" then
    -- 		for _,v in pairs(neighbourGroup) do
    -- ;
    -- 		end
    -- 	    end
    -- 	end
    --     end
end

function tests.unitGroupBuild()
    local surface = game.surfaces[global.natives.activeSurface]
    local group = surface.create_unit_group({position={-32, -32}})

    for i=1,10 do
	group.add_member(surface.create_entity({name="small-biter", position={-32, -32}}))
    end

    group.set_command({
	    type = defines.command.build_base,
	    destination = {-64, -64},
	    distraction = defines.distraction.by_enemy,
	    ignore_planner = true
    })
end

function tests.dumpEnvironment(x)
    print (serpent.dump(global[x]))
end

function tests.scanChunkPaths()
    local surface = game.surfaces[global.natives.activeSurface]
    local playerPosition = game.players[1].position
    local chunk = mapUtils.getChunkByPosition(global.map, playerPosition)
    print("------")
    print(chunkUtils.scanChunkPaths(chunk, surface, global.map))
    print("------")
end

function tests.stepAdvanceTendrils()
    -- for _, base in pairs(global.natives.bases) do
    -- 	tendrilUtils.advanceTendrils(global.map, base, game.surfaces[global.natives.activeSurface], {nil,nil,nil,nil,nil,nil,nil,nil})
    -- end
end

return tests
