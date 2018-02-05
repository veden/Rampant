-- module code

function generateLocal() 
    --    local names = {"Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"}
    local names = {"Neutral", "Acid", "Physical", "Electric", "Suicide", "Nuclear", "Fire", "Inferno", "Troll", "Fast", "Laser", "Wasp", "Spawner", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"}
    local sizes = {"Larva", "Pupae", "Worker", "Grunt", "Soldier", "General", "Overlord", "Titan", "Leviathan", "Juggernaut"}

    print("[entity-name]")

    local name = names[1]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("neutral-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("neutral-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("neutral-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("neutral-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("neutral-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[2]
    
    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    
	    print("acid-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("acid-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("acid-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("acid-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("acid-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[3]
    
    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    
	    print("physical-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class") 
	    print("physical-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("physical-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[4]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    
	    print("electric-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class") 
	    print("electric-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("electric-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")

	end
    end

    name = names[5]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    
	    print("suicide-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class") 
	    print("suicide-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("suicide-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")

	end
    end

    name = names[6]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do

	    print("nuclear-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class") 
	    print("nuclear-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("nuclear-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")

	end
    end

    name = names[7]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("fire-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("fire-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("fire-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("fire-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("fire-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[8]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("inferno-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("inferno-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("inferno-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[9]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("troll-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("troll-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("troll-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("troll-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("troll-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[10]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("fast-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("fast-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("fast-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("fast-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("fast-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[11]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("laser-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("laser-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("laser-biter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter nest: " .. size .. " class")
	    print("laser-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("laser-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[12]

    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("wasp-drone-v" .. v .. "-t" .. t .. "-drone-rampant=" .. name .. ": " .. size .. " class")
	    print("wasp-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("wasp-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("wasp-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    name = names[13]
    
    for t = 1, 10 do
	local size = sizes[t]
	for v = 1, 20 do
	    print("spawner-drone-v" .. v .. "-t" .. t .. "-drone-rampant=" .. name .. " eggs: " .. size .. " class")
	    print("spawner-spitter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter: " .. size .. " class")
	    print("spawner-biter-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " biter: " .. size .. " class")
	    print("spawner-spitter-nest-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " spitter nest: " .. size .. " class")
	    print("spawner-worm-v" .. v .. "-t" .. t .. "-rampant=" .. name .. " worm: " .. size .. " class")
	end
    end

    
end
