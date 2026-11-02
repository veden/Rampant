# Starting2 layout — the pressure map the enemy sees

Measured on a copy of `Starting2.zip` (md5 `697ee0a8...`, original untouched), reading the
same per-chunk quantity the base field is built from: `chunk.playerBaseGenerator` = sum of
structure weights (25 wall/belt, 500 assembler/turret/solar, 1,750 furnace/lab/drill,
6,000 artillery/reactor/silo), plus nest chunks from `chunkToNests`.

```
bounds=-34,-38..36,39   valueChunks=250   nestChunks=1327   both=0
legend:  ' ' none    . + # player value (low/mid/high)    n nest
  -16 nnn      nnn                               n    nnnn   nn   
  -15 nnn  nn  nnn           ###############    nn n nnnnn   nnnn 
  -14 n  nnn   n            ## # ## ## ## ###  nn    nn nn  nnnn  
  -13 nn        nnnn      ##           ## ##  n      n            
  -12 nn              n nn       #             ### nn     nn      
  -11 nnn            nn   n      #                     nn nn      
  -10 n   nn   nn   nn   n  #   #  ###        #            nn     
   -9 n  nnn  nn       #      ###        #         n  nn     nnn  
   -8 nn     n     n nn   ##  #  ###                  n      nn   
   -7 nnnn      nnnn nn   ##  #  ###        #       n    n n      
   -6 nnn    nn n n   n  ###    ###        #        nnn   n  nn  n
   -5 nn   nn               ### #  #####        #        n  n   nn
   -4 nn   nn  n            ##  #  #####      # #           n    n
   -3 nn   nn        n nn   #      ########   #     #       n  nnn
   -2 nn   nnn nn   ##  #  #########       ##  nn      nnn    nnn 
   -1 n     nn nn   nn   nn  # ################## ####  nn        
    0 nn   nn nn  nn     n  #     ## ######## #  ####   n n       
    1 nn   n      n nnn     ## #   #     #### #  # ##   n n nn    
    2 nn   n      n   n nn  ##     # #   ##      #                
    3 nn      n  nn   nnnnn ##       #   ###                n  nn 
    4 n   nn   nn nnn   ##          ####     #       n  n  nn     
    5 nn  nn      nn       #                    #    nnnnn n      
    6 nn  nn   nn  n     n   #                        nn n     nn 
    7 n  nnn  n   n  nnnn    #                  # ##      nn    nn
    8 n      nn              #                   ### nn    n      
    9 nn      n     n                          # ##   nn   n      
   10 n      nnn nn     #    ##    #   ## ##     n  nn      nn  n 
   11 nnnn    nnn      n     #   #               n               n
   12 nn   nn nn                 ## n       n      nn  nn n    
   13 nn          n     #          #### nn n   nnnnn   nnn   nn   
   14 nnn     nn   nn         n  #### ##   ##   n  n  nnn nn  n   
   15 nnn  n   n    nn    nn  n####      #  #   n  n  nnn    nnn  
   16 nnn          n  nn   ###         ##   n  n         nn nnn   
```

## What it is

* **A fortress inside a nest doughnut.** 250 chunks carry player value, all in the top
  weighting band (avg 14,000, peak chunk 94,000, total 3.5M). The value blob is compact
  (~22x22 chunks with spurs north and south).
* **1,327 nest chunks** form a ring around it, in every quadrant (n 686 / s 640 / e 670 /
  w 656). The **nearest nest chunk is 1 chunk (32 tiles) from a value chunk** — the ring is
  built up against the base, not somewhere out in the map.
* **No chunk holds both** value and nests; the ring starts at least one chunk outside.
* Distance from a nest to the nearest value chunk: 249 within 0-7 chunks, 509 within 8-15,
  568 within 16-23. No corridors, no directional gap.

## What the base field makes of it

* `REGIONAL_FIELD_RADIUS = 32` hops, decay 0.9 per hop, so the reach horizon is 32 hops.
* **312 of the 1,327 nest chunks are inside it.** Their values are large: p50 of those 312
  is thousands, p90 11,212, p99 23,441, max 27,797. The other 1,015 read exactly 0.
* So reach on this save is **binary, not graded**: in the field or out of it, and where in,
  always hundreds-to-thousands of times the `RAIDING_MINIMUM_BASE_THRESHOLD = 50` gate.
* **Pollution is 0 on every nest chunk.** The pollution channel is dormant; all pressure here
  is geographic.

## What it predicts under the heat model

1. The hot set is the inner ring (312 chunks, all reachable). Heat's reach term is
   `clamp(field/peak)`, so on this save it saturates: the inner ring is hot, the outer ring
   is cold. The ladder therefore selects by **band**, not by gradient.
2. With no pollution, the pulse ladder is reach-driven: **probes** on the inner ring, and
   **assaults** only if reach income can bank to the assault threshold. If assaults should
   be possible on a clean save, that sets the minimum reach gain.
3. There is **no layout gradient to steer with** here: the ring is uniform. Shaping pressure
   on this save can only come from **target cost** (value / approach cost), i.e. the AI must
   prefer the cheapest quadrant, so a heavy wall in one direction has to shift bursts to
   another. That is the mechanism that makes multiple walls and corridors matter on a map
   with no natural chokepoint.
4. The player's lever here is **shaving the inner ring**: killing the nests in the hot 312
   drops that base's heatSum to zero and pushes the effective front outward by a band. That
   is a concrete, measurable offensive objective on this save.

## Why this save reads as a tax

The geometry says "surrounded, no gaps, every direction in reach": a permanent warm ring on
all sides. That layout invites exactly one answer — a single perimeter — and today's measured
output (6 groups per 10 game-minutes, live squads p50 1, 100% of attempts rejected on the
points meter) is the *points meter* starving a geometry that should be busier. Starting2 is
therefore the **maximum-pressure end** of the 1x-3x range: the right save to test the ceiling
on, not the one to judge the default on.
