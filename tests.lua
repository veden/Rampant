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

-- Developer console helpers, exposed through the "rampantTests" remote
-- interface. Only tools that work on the current architecture live here: the
-- per-chunk CSV dump and the storage dumper were removed with the dirty pass
-- (they walked the deleted process queue and read deleted chunk slots, and
-- called the data-stage `serpent` lib, which does not exist at runtime).
-- Field values and per-chunk state are queried over UDP by Observability.

local tests = {}

function tests.fillableDirtTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.get_surface(game.surfaces[1]).set_tiles({{name="fillableDirt", position={chunkX-1, chunkY-1}},
            {name="fillableDirt", position={chunkX, chunkY-1}},
            {name="fillableDirt", position={chunkX-1, chunkY}},
            {name="fillableDirt", position={chunkX, chunkY}}},
        false)
end

function tests.tunnelTest()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.get_surface(game.surfaces[1]).create_entity({name="tunnel-entrance-rampant", position={chunkX, chunkY}})
end

function tests.reveal(size)
    local pos = game.player.character.position
    game.player.force.chart(game.player.surface,
                            {{x=-size+pos.x, y=-size+pos.y}, {x=size+pos.x, y=size+pos.y}})
end

return tests
