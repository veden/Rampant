# Rampant Tactics
Factorio Mod - Improves the enemies tactics by using potential fields/pheromones allowing probing of defenses, retreats, and player hunting  

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
Unit Group Forming - any chunks with spawners in it that is covered by a pollution, player, player base, or player defense pheromone clouds will form groups based on the evolution factor  
Probing Behavior Against Defenses - unit groups will attempt to avoid chunks that are soaked in death  
Player Hunting  - unit groups will track the player based on there emitted pheromone cloud  
Pathfinding - unit groups will use potential fields to perform only single step pathfinding allowing for efficient and dynamic pathing  

# Planned Features

Tunneling Biters  
Fire Biters  
Suicide Biters  
infesting Biters  
Base Expansion  

# Version History

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
- Xhecks for main surface (https://forums.factorio.com/viewtopic.php?f=94&t=31445&p=198228#p198563)   
- Updated info with forum homepage   
        
0.0.4 - initial release   
	
