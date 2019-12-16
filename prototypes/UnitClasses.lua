-- module code

function generateMigration()

    for t = 1, 10 do
        for v = 1, 20 do
            print("[\"neutral-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"neutral-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")
            print("[\"neutral-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"neutral-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"acid-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"acid-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")
            print("[\"acid-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"acid-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"physical-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"physical-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"electric-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"electric-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"suicide-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"suicide-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"nuclear-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"nuclear-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"fire-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"fire-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")
            print("[\"fire-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"fire-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"interno-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"interno-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"troll-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"troll-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")
            print("[\"troll-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"troll-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"laser-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"laser-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")
            print("[\"laser-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"laser-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"fast-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"fast-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")
            print("[\"fast-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"fast-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"wasp-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"wasp-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"spawner-spitter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"spawner-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"energy-thief-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"energy-thief-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")

            print("[\"poison-biter-nest-v" .. v .. "-t" .. t .. "-rampant\", \"poison-biter-spawner-v" .. v .. "-t" .. t .. "-rampant\"],")
        end
    end
end

-- local FACTION_TYPES = {
--     "neutral",
--     "acid",
--     "physical",
--     "electric",
--     "suicide",
--     "nuclear",
--     "fire",
--     "inferno",
--     "troll",
--     "fast",
--     "laser",
--     "wasp",
--     "spawner",
--     "energy-thief",
--     "poison"
-- }

function generateLocal()
    --    local names = {"Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"}
    local names = {"Neutral", "Acid", "Physical", "Electric", "Suicide", "Nuclear", "Fire", "Inferno", "Troll", "Fast", "Laser", "Wasp", "Spawner", "Energy Thief", "Poison", "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"}
    local sizes = {"Larva", "Pupae", "Worker", "Grunt", "Soldier", "General", "Overlord", "Titan", "Leviathan", "Juggernaut"}


    local HIVE_BUILDINGS_TYPES = {
        "trap",
        "turret",
        "utility",
        "spitter-spawner",
        "biter-spawner",
        "hive"
    }

    local HIVE_BUILDINGS_NAMES = {
        "Traps",
        "Turrets",
        "Utilities",
        "Spitter Spawners",
        "Biter Spawners",
        "Hives"
    }

    local HIVE_BUILDING_NAME_MAP = {
        ["trap"] = "trap",
        ["turret"] = "turret",
        ["utility"] = "utility",
        ["spitter-spawner"] = "spitter spawner",
        ["biter-spawner"] = "biter spawner",
        ["hive"] = "hive"
    }

    local name = names[1]

    for t = 1, 10 do
        for i=1,#HIVE_BUILDINGS_TYPES do
            print("entity-proxy-" .. HIVE_BUILDINGS_TYPES[i] .. "-t" .. t .. "-rampant=" .. HIVE_BUILDINGS_NAMES[i] .. ": " .. sizes[t] .. " class")
        end

        local size = sizes[t]
        for v = 1, 20 do
            print("neutral-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("neutral-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("neutral-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("neutral-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("neutral-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("neutral-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("neutral-spitter-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("neutral-worm-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
        end
    end

    name = names[2]

    for t = 1, 10 do
        local size = sizes[t]

        for v = 1, 20 do
            print("acid-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("acid-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("acid-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("acid-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("acid-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("acid-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("acid-spitter-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("acid-worm-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("acid-biter-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
        end
    end

    name = names[3]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("physical-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("physical-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("physical-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("physical-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
        end
    end

    name = names[4]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("electric-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("electric-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("electric-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("electric-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
        end
    end

    name = names[5]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("suicide-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("suicide-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("suicide-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("suicide-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("suicide-worm-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
        end
    end

    name = names[6]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("nuclear-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("nuclear-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("nuclear-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("nuclear-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("nuclear-worm-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
        end
    end

    name = names[7]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("fire-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("fire-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("fire-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("fire-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("fire-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("fire-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("fire-worm-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
        end
    end

    name = names[8]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("inferno-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("inferno-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("inferno-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("inferno-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
        end
    end

    name = names[9]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("troll-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("troll-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("troll-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("troll-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("troll-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("troll-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
        end
    end

    name = names[10]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("fast-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("fast-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("fast-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("fast-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("fast-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("fast-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("fast-spitter-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("fast-worm-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
        end
    end

    name = names[11]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("laser-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("laser-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("laser-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("laser-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("laser-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("laser-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
        end
    end

    name = names[12]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("wasp-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("wasp-wasp-v" .. v .. "-t" .. t .. "-drone-rampant=" .. name .. ": " .. size .. " class")
            print("wasp-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("wasp-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("wasp-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("wasp-worm-drone-v" .. v .. "-t" .. t .. "-drone-rampant=" .. name .. ": " .. size .. " class")
            print("wasp-wasp-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("wasp-worm-wasp-v" .. v .. "-t" .. t .. "-drone-rampant=Wasp: " .. size .. " class")
        end
    end

    name = names[13]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("spawner-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("spawner-drone-v" .. v .. "-t" .. t .. "-drone-rampant=" .. name .. " eggs: " .. size .. " class")
            print("spawner-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
            print("spawner-worm-egg-v" .. v .. "-t" .. t .. "-drone-rampant=" .. name .. " eggs: " .. size .. " class")
            print("spawner-worm-egg-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("spawner-spawn-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("spawner-spitter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
            print("spawner-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
            print("spawner-spitter-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("spawner-egg-v" .. v .. "-t" .. t .. "-acid-fire-rampant=Acid pool: " .. size .. " class")
            print("spawner-egg-v" .. v .. "-t" .. t .. "-drone-rampant=" .. name .. " egg: " .. size .. " class")
        end
    end

    name = names[14]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("energy-thief-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("energy-thief-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("energy-thief-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("energy-thief-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
        end
    end

    name = names[15]

    for t = 1, 10 do
        local size = sizes[t]
        for v = 1, 20 do
            print("poison-hive-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " hive: " .. size .. " class")
            print("poison-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
            print("poison-biter-spawner-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
            print("poison-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
        end
    end


end
