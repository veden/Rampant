# Rampant Tactics
Factorio Mod - Improves the enemies tactics by using potential fields/pheromones allowing probing of defenses, retreats, reinforcements, counterattacking, and player hunting  

# Forum Post

https://forums.factorio.com/viewtopic.php?f=94&t=31445  

# Notes

0.14.14 factorio version fixed save corruption  
0.14.10 factorio version fixed more pathing issues  
0.14.4 factorio version fixed some issues with unit groups commands  

There will be a slight pause the first time this is started up due to indexing all the chunks that have been generated.  

MP should be working  

# Features

Tactical Retreats - these will take place when a unit group is in a chunk that has reached a death threshold  
Unit Group Merging  - if multiple unit groups retreat at the same time there is a chance the groups will merge  
Unit Group Forming - any chunks with spawners in it that is covered by a pollution, player, player base pheromone clouds will form groups based on the evolution factor  
Probing Behavior Against Defenses - unit groups will attempt to avoid chunks that are soaked in death  
Player Hunting - unit groups will track the player based on there emitted pheromone cloud  
Counterattacks - when the player is in combat near nests they will send reinforcements to unit groups  
Reinforcements - nests will send assistance to nearby nests under attack by the player  
Pathfinding - unit groups will use potential fields to perform only single step pathfinding allowing for efficient and dynamic pathing  

# Planned Features

Tunneling Biters  
Fire Biters  
Suicide Biters  
infesting Biters  
Base Expansion  

# Version History

0.14.7 -  
- Feature: Counterattack waves trigger when the player is standing in a chunk with the death pheromone past the retreat threshold  
- Feature: Reinforcement waves trigger when the player is standind in a chunk that contains a nest  
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
- Fix: Increased player scoring weight, so biter groups correctly hunt once more (https://forums.factorio.com/viewtopic.php?f=94&t=31445#p216119)  
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

0.13.1 - Backported 0.14 factorio version to 0.13 factorio version   

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

