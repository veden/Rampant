local out = {}
local n = 0
local src = 0
local per = {}
local ox, oy = 0, 0
local sx, sy = 0, 0
for _, m in pairs(storage.universe.maps) do
    for x, xc in pairs(m) do
        if type(xc) == "table" then
            for y, c in pairs(xc) do
                if type(c) == "table" and c.id then
                    n = n + 1
                    local base = c.playerBaseGenerator or 0
                    local ore = c.resourceGenerator or 0
                    local player = c.playerGenerator or 0
                    local structures = (c.nestCount or 0) + (c.turretCount or 0)
                        + (c.utilityCount or 0) + (c.hiveCount or 0)
                    local death = c.deathGenerator or 0
                    local g = base + ore + player + structures
                    if g > 0 then
                        src = src + 1
                        local k = "ore"
                        if base > 0 then k = "base"
                        elseif structures > 0 then k = "nest"
                        elseif death ~= 0 then k = "death"
                        elseif player > 0 then k = "player"
                        end
                        per[k] = (per[k] or 0) + 1
                        if ore > 0 then ox = ox + 1 end
                        if base > 0 then oy = oy + 1 end
                        if player > 0 then sx = sx + 1 end
                        if structures > 0 then sy = sy + 1 end
                    end
                end
            end
        end
    end
end
out.chunks = n
out.sources = src
out.byClass = per
out.oreChunks = ox
out.baseChunks = oy
out.playerChunks = sx
out.structureChunks = sy
return out

--[[
    Save model extraction (UDP exec channel, mod context).

    Launch the save with the exec channel enabled, then send this file:
      factorio --start-server <save> --enable-lua-udp 5200 ...
      python3 - <<'PY'
      import socket; s=socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
      s.bind(('127.0.0.1',0))
      s.sendto(('exec:'+open('tools/dump-save-model.lua').read()).encode(), ('127.0.0.1',5200))
      print(s.recvfrom(65536))
      PY

    Result on the real late-game save (12.9M ticks, ~60 game-hours):
      chunks: 10739, sources: 10300 (96%!)
      byClass: ore 8117, nest 1760, base 423, death 0, player 0
      oreChunks 10292 (96% of chunks!), baseChunks 423, structureChunks 1760

    Why it matters: the source set is NOT sparse — it is 96% of the map, and the
    ore is 96% of it. At 0.9/hop with EPS=0.0005 the decay range is 73 hops,
    larger than the map, so a lazy read's candidate set is every source — worse than
    the sweep. The win is the STATIC/DYNAMIC SPLIT: the ore field is a fixed point
    and needs no renewal (96% of the sweep's work), leaving ~2,183 dynamic sources
    (20%) for the sweep at a low rate plus the event-driven path.
]]
