local tests = {}

local constants = require("libs/Constants")
local mathUtils = require("libs/MathUtils")
local chunkUtils = require("libs/ChunkUtils")
local baseUtils = require("libs/BaseUtils")

function tests.pheromoneLevels() 
    local player = game.player.character
    local playerChunkX = math.floor(player.position.x / 32)
    local playerChunkY = math.floor(player.position.y / 32)
    print("------")
    print(#global.regionMap.processQueue)
    print(playerChunkX .. ", " .. playerChunkY)
    print("--")
    for x=playerChunkX-9, playerChunkX+9 do
        for y=playerChunkY-9, playerChunkY+9 do
            if (global.regionMap[x] ~= nil) then
                local chunk = global.regionMap[x][y]
                if (chunk ~= nil) then
                    local str = ""
                    for i=1,#chunk do
                        str = str .. " " .. tostring(i) .. "/" .. tostring(chunk[i])
                    end
		    str = str .. " " .. "p/" .. game.surfaces[1].get_pollution({x=chunk.pX, y=chunk.pY})
		    if (chunk.cX == playerChunkX) and (chunk.cY == playerChunkY) then
			print("*", chunk.cX, chunk.cY, str)
		    else
			print(chunk.cX, chunk.cY, str)
		    end
		    -- print(str)
		    print("-")
                end
            end
        end
    end
end

function tests.activeSquads()
    print("--")
    for i=1, #global.natives.squads do
        local squad = global.natives.squads[i]
        if squad.group.valid then
            print(math.floor(squad.group.position.x * 0.03125), math.floor(squad.group.position.y * 0.03125), squad.status, squad.group.state)
            print(serpent.dump(squad))
        end
    end
end

function tests.entitiesOnPlayerChunk()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local entities = game.surfaces[1].find_entities_filtered({area={{chunkX, chunkY},
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
    local entity = game.surfaces[1].find_nearest_enemy({position={chunkX, chunkY},
                                                        max_distance=constants.CHUNK_SIZE,
                                                        force = "enemy"})
    if (entity ~= nil) then
        print(entity.name)
    end
    print("--")
end

function tests.aiStats()
    print(global.natives.points, game.tick, global.natives.state, global.natives.temperament, global.natives.stateTick, global.natives.temperamentTick)
end

function tests.fillableDirtTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[1].set_tiles({{name="fillableDirt", position={chunkX-1, chunkY-1}},
	    {name="fillableDirt", position={chunkX, chunkY-1}},
	    {name="fillableDirt", position={chunkX-1, chunkY}},
	    {name="fillableDirt", position={chunkX, chunkY}}}, 
	false)
end

function tests.tunnelTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[1].create_entity({name="tunnel-entrance", position={chunkX, chunkY}})
end

function tests.createEnemy(x)
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[1].create_entity({name=x, position={chunkX, chunkY}})
end

function tests.attackOrigin()
    local enemy = game.surfaces[1].find_nearest_enemy({position={0,0},
                                                       max_distance = 1000})
    if (enemy ~= nil) and enemy.valid then
        print(enemy, enemy.unit_number)
        enemy.set_command({type=defines.command.attack_area,
                           destination={0,0},
                           radius=32})
    end
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
	local s = mathUtils.roundToNearest(mathUtils.gaussianRandomRange(10, 17, 0, 100), 1)
	result[s] = result[s] + 1
    end
    for x=0,100,1 do
    	print(x, result[x])
    end
end

function tests.reveal (size)
    game.player.force.chart(game.player.surface,
			    {{x=-size, y=-size}, {x=size, y=size}})
end

function tests.baseStats()
    local natives = global.natives
    print ("cX", "cY", "pX", "pY", "created", "alignment", "strength", "numberOfChunks", "upgradePoints")
    for i=1, #natives.bases do
	local base = natives.bases[i]
	print(base.cX, base.cY, base.cX * 32, base.cY * 32, base.created, base.alignment, base.strength, #base.chunks, base.upgradePoints)
    end
end

function tests.baseTiles()
    local natives = global.natives
    for i=1, #natives.bases do
	local base = natives.bases[i]
	local color = "concrete"
	if (i % 3 == 0) then
	    color = "deepwater"
	elseif (i % 2 == 0) then
	    color = "water"
	end
	for x=1,#base.chunks do
	    local chunk = base.chunks[x]
	    chunkUtils.colorChunk(chunk.pX, chunk.pY, color, game.surfaces[1])
	end
	chunkUtils.colorChunk(base.cX * 32, base.cY * 32, "deepwater-green", game.surfaces[1])
    end
end


function tests.clearBases()

    local surface = game.surfaces[1]
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

function tests.showMovementGrid()
    local chunks = global.regionMap.processQueue
    for i=1,#chunks do
	local chunk = chunks[i]
	local color = "deepwater-green"
	if (chunk[constants.NORTH_SOUTH_PASSABLE] and chunk[constants.EAST_WEST_PASSABLE]) then
	    color = "water"
	elseif chunk[constants.NORTH_SOUTH_PASSABLE] then
	    color = "deepwater"
	elseif chunk[constants.EAST_WEST_PASSABLE] then
	    color = "water-green"
	end
	chunkUtils.colorChunk(chunk.pX, chunk.pY, color, game.surfaces[1])
    end
end

return tests
