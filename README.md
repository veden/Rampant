# Rampant Tactics
Factorio Mod - Improves the enemies tactics by using potential fields/pheromones allowing probing of defenses, retreats, reinforcements, counterattacking, breaching, rallying death cry, and player hunting. Also removes homing biter projectiles.

# Forum Post

https://forums.factorio.com/viewtopic.php?f=94&t=31445  

# Notes

0.14.14 factorio version fixed save corruption  
0.14.10 factorio version fixed more pathing issues  
0.14.4 factorio version fixed some issues with unit groups commands  

There will be a slight pause the first time this is started up due to indexing all the chunks that have been generated.  

MP should be working  

Configure Options:  
- Max biter wave size  
- Ramp up to max biter wave size  
- Use dumb projectiles  
- Use Natural Evolution unit launchers (Requires NE)  
- Togglable attack wave triggers (pollution, player)  
- Attack wave pollution trigger threshold  

# Features

- Recycling Biters - When large groups of biters form on the game map and aren't assigned to a unit group or near a base will be periodically removed and refunded to the ai causes attack waves proportional to the number of units removed.  
- Breaching - When biters are destroying structures nearby unit groups will come to join them  
- Frenzy squads - When a unit group gets close to a player or start combat they switch to attacking everything in there path for a set radius or until there is nothing left  
- Rabid squads - Is in a permanent frenzied state as soon as the group is formed  
- Tactical Retreats - these will take place when a unit group is in a chunk that has reached a death threshold  
- Unit Group Merging - if multiple unit groups retreat at the same time there is a chance the groups will merge  
- Unit Group Forming - any chunks with spawners in it that is covered by a pollution or player clouds will form groups based on the evolution factor  
- Probing Behavior Against Defenses - unit groups will attempt to avoid chunks that are soaked in death  
- Player Hunting - unit groups will track the player based on there emitted pheromone cloud  
- Rallying Death Cry - when a unit is killed on a chunk that is past the retreat threshold, the unit will attempt to summon reinforcements to help them  
- Counterattacks - when the player is in combat near nests they will send reinforcements to unit groups  
- Reinforcements - nests will send assistance to nearby nests under attack by the player  
- No Homing Projectiles - all projectiles are fired at locations and no longer track the player  
- Pathfinding - unit groups will use potential fields to perform only single step pathfinding allowing for efficient and dynamic pathing  
- Peace mode - if something sets peace mode, Rampant will respect it  

# Planned Features

- Tunneling Biters  
- Fire Biters  
- Suicide Biters  
- infesting Biters  
- adaptive aliens  
- Base Expansion  

# Version History

0.14.13 -  
- Reverted: Pheromone generation tweaks from 0.14.11  
- Feature: Recycling large biter swarms that are stuck (causes a big attack wave, and pops up a message)  
- Feature: Biters destroying player buildings will attract other unit groups  
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

