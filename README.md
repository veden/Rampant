# Rampant Tactics
Factorio Mod - Improves the enemies tactics by using potential fields/pheromones allowing probing of defenses, retreats, reinforcements, counterattacking, breaching, raids, rallying death cry, and player hunting. Uses nonhoming blockable biter projectiles. Adds new Enemies (disabled by default). Can completely replace the vanilla AI. Difficulty setting in mod options menu.  

# Forum Post

https://forums.factorio.com/viewtopic.php?f=94&t=31445  

# Notes

0.14.14 factorio version fixed save corruption  
0.14.10 factorio version fixed more pathing issues  
0.14.4 factorio version fixed some issues with unit groups commands  

There will be a pause the first time and on every upgrade that Rampant loads.  
This is due to indexing all the chunks that have been generated.  

MP should be working  
If experiencing desyncs, after an update, please do the following:  
	1) let me know  
	2) Load save with Rampant enabled  
	3) Save the map after Rampant has been updated  
	4) Load save in step 3  

Configure Options not in game menu:  
- Ramp up to max biter wave size  

# Features

- New Enemy Factions - Neutral, Acid, Fast, Physical, Electric, Inferno, Suicide, Fire, Nuclear, Laser, Troll, Wasp, Spawner  
- Swarming - Units will smoothly slide by one another allowing for streamlined attacking  
- Difficulty Scaling - A mod option to control how quickly the ai can perform actions like making attack waves.  
- Nocturnal Mode - A mod option to force biters to only attack at night. Does not yet affect vanilla attacks. Best use with clockwork or daynight extender mod  
- Recycling Biters - When large groups of biters form on the game map and aren't assigned to a unit group or near a base will be periodically removed and refunded to the ai causing attack waves proportional to the number of units removed.  
- Breaching - When biters are destroying structures nearby unit groups will come to join them  
- Frenzy squads - When a unit group gets close to a player or start combat they switch to attacking everything in there path for a set radius or until there is nothing left  
- Rabid squads - Is in a permanent frenzied state as soon as the group is formed  
- Tactical Retreats - These will take place when a unit group is in a chunk that has reached a death threshold  
- Unit Group Merging - If two squads occupy roughly the same area and are doing the same task then they will merge  
- Unit Group Forming - Any chunks with spawners in it that is covered by a pollution or player clouds will form groups based on the evolution factor  
- Probing Behavior Against Defenses - unit groups will attempt to avoid chunks that are soaked in death  
- Player Hunting - Unit groups will track the player based on there emitted pheromone cloud  
- Rallying Death Cry - When a unit is killed on a chunk that is past the retreat threshold, the unit will attempt to summon reinforcements to help them  
- Counterattacks - When the player is in combat near nests they will send reinforcements to unit groups  
- Reinforcements - Nests will send assistance to nearby nests under attack by the player  
- No Homing Projectiles - All projectiles are fired at locations and no longer track the player  
- Pathfinding - Unit groups will use potential fields to perform only single step pathfinding allowing for efficient and dynamic pathing  
- Peace mode - If something sets peace mode, Rampant will respect it  
- Ion Cannon Reaction - Firing the Ion Cannon will cause nests around the blast site to form into an attack wave and agitate all biters  
- Rocket Reaction - Firing the rocket from the rocket silo will cause the biters to form extra attack waves  
- Blockable Projectiles - Some of the biters projectiles can now be blocked by walls and trees  
- Raiding AI state - The AI will periodically send attack waves based on building proximity and not just pollution  
- Migration AI State - Where the ai looks for resources patches to setup new bases  
- Sieging AI state - Where the ai does a migration event but also builds towards the player and their base  
- Onslaught AI state - Where the ai gains 2x credits per logic cycle that can be used on units and buildings  
- Vanilla AI Replacement - The default expansion and attack waves can be completely turned off and allow Rampant to work its magic  

# Planned Features

- Tunneling Biters  
- Infesting Biters  

# Version History

New updates appear in the changelog now

0.15.24 -  
- Feature: Swarming - by reducing the unit collision_mask to 40% of its original size the units no longer have the pathing issues that plagued large groups attacking  
- Optimization: further reduced memory footprint for faster saving and loading  

0.15.23 -  
- Fixed: Retreat radius being centered on chunk corner instead of on the unit dying  
- Fixed: Spitter flamethrower sound fx  
- Moved: Item Collector into separate mod  
- Balance: Adjusted spitter and worms damages, ranges, and cooldowns to be more inline with vanilla  
- Optimization: Reduced memory footprint for faster saving and loading  
- Framework: Reorganization of files  

0.15.22 -  
- Contribution - Martok88, Improvement: Added optional attack wave message per player  

0.15.21 -  
- Fixed: Added check for nil'ed itemCollection event (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=200#p302631)  
- Fixed: Item Collector deconstruction not pulling items before destroying chest  
- Fixed: Force Item Collector chest to be only minable and deconstructable piece to force contents to be moved to player  
- Improvement: Switched over to on_player_mined_entity and on_robot_mined_entity instead of the pre variants  

0.15.20 -  
- Fixed: Added Low Resolution version of overlay, ISSUE: range of collector extends past visual display because of 2048px limit on sprites (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=180#p302128)  
- Tweak: Increased item collector build time from 0.5 seconds to 10 seconds  
- Improvement: Added an expensive recipe variant that doubles the cost of the item collector (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=180#p302123)  
- Optimization: Tuned chunk scoring heuristic  

0.15.19 -  
- Fixed: Error when processing item collectors  

0.15.18 -  
- Feature: Adds an item collector for things like alien artifacts  
- Improvement: Added checks for how many squads have been created to enforce global limit over all squad creation methods  
- Tweak: Increased breach multiplier from 10000 to 100000  
- Fixed: Current version wasn't be set causing the upgrade code to run repeatedly  
- Fixed: Neighbors function not correctly clearing its state between calls  
- Optimization: Reduced number of chunks processed per cycle from 500 to 400  
- Optimization: Reduced number of squads to regroup per cycle from 5 to 2  
- Optimization: Reduced number of chunks to scan per cycle from 6 to 5  
- Optimization: Added additional short circuits for chunk scoring  

0.15.17 -  
- Fixed: Remote call load issue. (https://github.com/veden/Rampant/issues/5)  
- Tweak: Increased sampling threshold for water tiles from 5 to 10 tiles  
- Tweak: Increased small worm turret range from 17 to 18  
- Improvement: Added option to remove blood particles on biter deaths, which should help reduce lag spikes (Default is to remove)  
- Optimization: Moved math.random to local level instead of global  
- Framework: Refactored unit and attack prototypes  

0.15.16 -  
- Tweak: Increased death pheromone weight for squad attack from 1 to 2  
- Tweak: Increased failed unit behaviors from 6 to 10  
- Improvement: Added labs to list of targets  
- Improvement: Removed chunks processed at a time limit  
- Optimization: Moved chunk reindexing to mod load as opposed to ingame  
- Optimization: Preallocated a position for use in squad movement  
- Optimization: Greatly reduced reindexing and chunk scoring time  
- Framework: Split chunk scoring and custom ai chunk purging  
- Framework: Refactored code into more appropriate modules  

0.15.15 -  
- Fixed: Desync when reindexing chunks (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=180#p287941)  
- Fixed: Switched from "and" to "or" for player pheromone and base pheromone to form squads  
- Fixed: Inverted logic comparison for unit group formation  
- Fixed: Added checks for if a nest exists on player scan before creating squads  
- Tweak: Reduced map setting for path finding min steps to check for path from 350 to 100  
- Optimization: Reduced max number of chunks to processes at anyone time from 1000 to 200  
- Optimization: Specialized neighbor search function  
- Optimization: Chunk scoring for determining chunk path rating and passable status changed to two pass  
- Framework: Refactored recycle biters into separate function  
- Framework: Removed custom ai option until it is complete  

0.15.14 -  
- Tweak: Increased pheromone output of:
	- ammo-turret 2.5 -> 10
	- wall 0.25 -> 0.5
	- electric-turret 7 -> 20
	- fluid-turret 9 -> 28
	- turret 2.5 -> 10
	- mining-drill 15 -> 35
- Tweak: Increased attack radius of squads from 28 to 32 tiles  
- Tweak: Decreased movement(death) pheromone persistance from 0.98 to 0.9  
- Tweak: Increased impassable terrain generator pheromone amount from -30 to -0.1  
- Tweak: Reduced number of remembered past chunks for a squad from 10 to 7  
- Tweak: Increased unit group count for "ground shake" message from 11 to 14  
- Fixed: Pheromone is no longer placed on impassable chunks  
- Improvement: Removed pollution from ai attack chunk scoring, pollution travels over water and creates weird pockets groups get stuck in  
- Improvement: Made create squads aware of orientation changes when building squads  
- Improvement: Recycling unit groups that are stuck  
- Improvement: Allow movement from an impassable chunk to a all cardinals passable chunk  
- Improvement: When creating squads base pheromone or player pheromone must be present. Prevents squad spawns when they can't reach the player(s) or player(s) structures.  
- Improvement: Made retreats aware of orientation changes with retreating squads  
- Improvement: Path finding check for invalid destination before making command  
- Improvement: Added path rating to each chunk to reduce the scores on chunks that may be passable but are easy to invalidate the unit group by making a command to an invalid location(water, entity)  
- Optimization: Collapsed chunk attributes *_PASSABLE into single attribute PASSABLE  
- Optimization: Preallocated neighbors and cardinal neighbors array  

0.15.13 -  
- Improvement: Added lamps to make safe options (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=160#p284736)  

0.15.12 -  
- Fixed: Nil fillTunnel invocation (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=160#p284719)  

0.15.11 -  
- Integration: Ion cannon mod  
- Fixed: Player region scan can no longer overlap passive map scan causing double processing  
- Tweak: Setting group disown distance back to the default of 10 from 5  
- Tweak: Increased group late unit threshold from 80 to 360 ticks  
- Tweak: Increased rally cry chance from 0.02(@100 evo) to 0.08(@100 evo) compensate for the once per logic cycle per chunk  
- Tweak: Increased player pheromone for weight multipler from 25 to 50 for hunting parties  
- Improvement: Changed biter base detection from slow map scan to event  
- Improvement: Added negative score contribution to nest causing biters to move around nests instead of through  
- Optimization: Reduced max number of active squads from 40 to 30, reducing large lag spikes  
- Optimization: Player region scan can no longer overlap passive map scan causing double processing  
- Optimization: Short circuit rally cry search if not enough points to make a squad  
- Optimization: Short circuit vengence squad search if not enough points to make a squad  
- Optimization: Short circuit attack groups search if not enough points to make a squad  
- Optimization: Precompute retreatThreshold, maxSquads, rallyThreshold, formSquadThreshold, attackWaveSize, attackWaveSizeLowerBound, attackWaveSizeUpperBound, attackWaveSizeDeviation, kamikazeThreshold, attackWaveThreshold once per logic cycle  
- Optimization: Reduced garbage generated by pheromone map processing  
- Optimization: Reduced garbage generated by attack formation scanning  
- Optimization: Capped squad regrouping attempts to 15 per logic cycle  
- Framework: renamed chunk pX,pY to x,y, so chunks could be used in calls to things like get_pollution  

0.15.10 -  
- Fix: nil chunk in pheromone utils(https://mods.factorio.com/mods/Veden/Rampant/discussion/13806)  
- Tweak: Increased failed behaviors before dispanding from 3 to 6  
- Improvement: Switched to untargetable indestructible safe buildings  
- Improvement: Changed the "ground shake" message to be displayed at a more appropriate time  
- Improvement: Recycling biter groups now has a lower threshold and checks for active nearby squads before purging the clusters  
- Optimization: Adjusted factorio pathfinder parameters to favor short paths for performance  
- Optimization: Moved invariants out of inner loop in pheromone dispersion  
- Optimization: Reduced garbage generated when doing passive map scan  
- Optimization: Switched rally cries to a once per logic cycle per chunk  
- Optimization: Locallized global defines in files that use them  
- Optimization: Preallocating tables of falses for chunk neighbors  
- Framework: Split squad regrouping and squad cleanup  

0.15.9 -  
- Improvement: Added bobs higher tier big electric poles to be included with make safe toggle for big electric poles  
- Improvement: Added logic to reconnect wires when big electric poles are made safe and get destroyed (Thanks Jeroen D Stout, https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=140#p275663)  
- Improvement: Added a mod option to add acid resistance to walls to make damage levels with dumb projectiles to be equal to vanilla levels. (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=120#p274792)  

0.15.8 -  
- Fixed: aiPointsScaler being nil  

0.15.7 -  
- Feature: Difficulty Scaling - A mod options to control how quickly the ai performs actions  
- Fixed: Spelling on mod option  
- Improvement: Exposed nocturnal toggle and difficulty scaling to remote interop  

0.15.6 -  
- Feature: Nocturnal Mode - Causes Rampant attacks waves to only spawn at night. Best use with daynight extender mod.  

0.15.5 -  
- Tweak: Increased ai refund from 2 to 3  
- Fix: Signals, Chain Signals, and Train stops now correctly rebuild when the corresponding make safe is toggled  
- Feature: Remote interfaces for adjusting wave size, thresholds, ai build points, ai state, player thresholds, and attack triggers  
- Improvement: Added reactor, programmable speaker, radars, lights, and rocket silo to biter targets  
- Improvement: Switched all configs to global runtime except Dumb projectiles and NE Unit Launcher Options  

0.15.4 -  
- Fix: Changed setting name from "make buildings safe from biters" to "Enable building safety." This is to clarify what the setting does.  

0.15.3 -  
- Improvement: Added configuration for safe buildings. This will be improved after a bug fix in factorio  

0.15.2 -  
- Improvement: Created in game options for  
  - Max biter wave size  
  - Use dumb projectiles  
  - Use Natural Evolution unit launchers (Requires NE)  
  - Togglable attack wave triggers (pollution, player)  
  - Attack wave pollution trigger threshold  

0.15.1 -  
- Tweak: Increased small spitter damage from 7 to 15  
- Tweak: Increased medium spitter damage from 15 to 22  
- Improvement: Replaced game.evolution with game.forces.enemy.evolution  
- Improvement: Fixed flame-thrower to flamethrower  
- Improvement: Changed tunnel to have non-zero hp  

0.14.13 -  
- Reverted: Pheromone generation tweaks from 0.14.11  
- Feature: Recycling large biter swarms that are stuck (causes a big attack wave, and pops up a message)  
- Feature: Breaching, Biters destroying player buildings will attract other unit groups  
- Tweak: Reduced unit group max size from 600 to 450  
- Tweak: Reduced unit group radius from 30 to 20  
- Fix: Unit groups merging during combat  
- Optimization: Cleaned up regroup squad function to scale better  
- Optimization: Retreats only fire once per chunk for every logic cycle  
- Improvement: Cleaned up dispersion function for Pheromones  
- Improvement: Increased attack wave frequency based on evolution factor (mainly for endgame)  

0.14.12 -  
- Tweak: Decreased slow map scan chunks scanned per logic cycle from 8 to 6  
- Fix: Didn't set new version number forcing chunk recalculation every load  
- Improvement: Added player buildings to slow map scan to catch buildings made outside factorio event system, (i.e. FARL)  
- Improvement: Added console message when upgrading  

0.14.11 -  
- Tweak: Increased pheromone generation on (This only potential affects target selection and pathfinding, these do not trigger biter waves):  
  - Generators from 8 to 12  
  - Pumps from 2 to 5  
  - Offshore Pumps from 2 to 5  
  - Transport Belts from 1 to 2  
  - Accumulator from 10 to 14  
  - Solar Panel from 8 to 12  
  - Boiler from 12 to 16  
  - Assembling Machines from 10 to 16  
  - Roboport from 10 to 14  
  - Beacon from 10 to 14  
  - Furance from 12 to 16  
  - Mining Drills from 15 to 19  
  - Ammo Turret from 2.5 to 5  
  - Wall from 0.25 to 0.55  
  - Electric Turret from 4.25 to 7  
  - Fluid Turret from 5 to 9  
  - Turret from 3.5 to 5  
- Tweak: Reduced map scan speed from 10 chunks to 8 chunks to account for worms in calculation  
- Tweak: Increased pheromone dispersion from 0.05 to 0.1  
- Tweak: Increased unit group move distance from 32 to 40  
- Tweak: Reduced unit group member disown distance from 10 to 5  
- Tweak: Increased unit group max slow down when ahead from 0.6 to 1  
- Tweak: Reduced unit group max speedup when behind from 1.4 to 1.1  
- Tweak: Increased unit group max slowdown from slow members from 0.3 to 0.9 (this is max speed multipler, so this is faster)  
- Tweak: Increased unit attack radius from 20 to 28 tiles  
- Improvement: Increased movement cycles for larger groups from 4 cycles to 6 cycles  
- Improvement: Biter groups should attempt to avoid getting too close to water (screws up pathing)  
- Improvement: Added worms to chunk calculation for determining better unit group formation and pathing  
- Improvement: Removed nest negative contribution to base pheromone map for better pathing  
- Improvement: Pheromones no longer travel over impassable terrain  
- Fix: Prevent group spawning on chunks with enemy structures  
- Fix: Capped the max group size that can be created by merging to 600 biters  
- Fix: Dispand unit groups larger than 600 (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=80#p255243, Thank you Nerchio for the save game)  

0.14.10 -  
- Feature: Replaces homing projectiles with non homing versions  
- Improvement: Respect for peace mode. To be used with something like the True Peace Mod  
- Fix: Enforce rate limit for retreats  

0.14.9 -  
- Fix: Added null check in rally cry for chunks that have yet to be generated by game engine (https://mods.factorio.com/mods/Veden/Rampant/discussion/7946)  

0.14.8 -  
- Feature: Rallying death cry, when a native dies on a chunk past the death threshold it will attempt to summon reinforcements from nearby nests  
- Tweak: Increased unit group merge distance from 16 to 28 tiles  
- Tweak: Increased retreat grab radius from 15 tiles to 24 tiles  
- Tweak: Decreased vengeance squad cost from 50 to 45  
- Improvement: On group merge recalculate the kamikaze threshold, so groups that become large have a chance to kamikaze before attacking  
- Improvement: Disallow group merges when units are taking and receiving damage  
- Fix: Corrected unit group frenzy trigger based on engaging a target  
- Optimization: Switched to increment tick counter for processing and logic event  
- Optimization: Rate limited rally cry to 3/0.75 sec  
- Optimization: Rate limited retreat to 8/0.75 sec  

0.14.7 -  
- Feature: Counterattack waves trigger when the player is standing in a chunk with the death pheromone past the retreat threshold  
- Feature: Reinforcement waves trigger when the player is standing in a chunk that contains a nest  
- Tweak: Increased max number of unit groups that can be active concurrently from 30 to 40  
- Improvement: Removed restriction on unit group formations around nests  

0.14.6 -  
- Major Fix: Corrected retreat logic having inverted comparison introduced in 14.4, so unit groups stopped retreating after lots of death happened in a chunk  
- Major Fix: Corrected pheromone dispersal with negative numbers  
- Fix: Adjusted scoring so unit groups try to avoid nest clusters when retreating (Messes up pathfinding)  
- Fix: When placing a player building ai was given credit as if they destroyed it  
- Tweak: Reduced retreat time length from 4.5 to 3 seconds  
- Tweak: Reduced death pheromone produced on death from 100 to 75  
- Tweak: Increased attack radius of unit groups from 16 to 20 tiles  
- Tweak: Increased attack time length from 2.25 to 3 seconds  
- Tweak: Increased nest pheromone production from 15 to 30  
- Tweak: Increased unit group search radius during formations from 2 chunks to 3 chunks  
- Framework: Decoupled squad status and kamikaze flag  
- Improvement: Biters don't retreat when dying on a chunk with a nest  
- Improvement: Chunks covered by nest pheromone remove death pheromone faster  
- Improvement: Attack wave size varies based on normal distribution that is centered around a scaled evolution factor that is lower than vanilla  

0.14.5 -  
- Improvement: Enlarged player processing bubble from 3 to 4 chunks (pheromone radius is still 4 chunks)  
- Fix: Increased player scoring weight, so biter groups correctly hunt once more (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=40#p216119)  
- Improvement: Adjusted attack pathing, so groups can move diagonally  
- Fix: Unit group retreating when player occupies same chunk  

0.14.4 -  
- Fixed a bug in the processing queue when upgrading mod  
- Greatly decreased Player pheromone radius, now sits at roughly 4 chunks around the player  
- Reworked pheromone pathfinding  
- Removed base and defense attack wave trigger, in favor of using player pheromone and pollution  
- Added periods of time where the enemy is not sending Rampant attack waves   
- Adjusted retreat percentage to suit the reduced attack wave size  
- Improved responsiveness on larger maps  
- Reduced AI max build points  
- Fixed player iteration bug  

0.14.3 -   
- Slightly lowered Rampant attack wave frequency  
- Altered attack wave size to ramp up slower  
- Added configuration options for:  
- - Attack wave generation area  
	- - Attack wave threshold  
	- - Attack wave size  
	- - Turn off rampant attack waves  

0.14.2 -  
- Adjusted unit retreat group size threshold   
- Adjusted squad attack pattern (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=20#p203861)  
- Fixed migration issue   

0.14.1 -   
- Fixed ai created bases not being counted in logic   
- Optimization to offset ai created bases scanning   

0.13.5 = 0.14.5   

0.13.4 = 0.14.4   

0.13.3 = 0.14.3   

0.13.2 = 0.14.2   

0.13.1 - Back ported 0.14 factorio version to 0.13 factorio version   

0.0.8 -   
- Fixed retreat oscillations (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=10#p198750)   
- Added scaling for kamikaze attack (https://forums.factorio.com/viewtopic.php?f=94&t=31445&start=10#p199401)   
- Increased squad size max from 125 to 150, (larger waves)   

0.0.6 -  
- Some speed improvements   
- MP is working (https://github.com/veden/Rampant/issues/1)   

0.0.5 -  
- Fix for nil chunk in ai attack (https://mods.factorio.com/mods/Veden/Rampant/discussion/2512)   
- Checks for main surface (https://forums.factorio.com/viewtopic.php?f=94&t=31445&p=198228#p198563)   
- Updated info with forum homepage   
        
0.0.4 - initial release   

