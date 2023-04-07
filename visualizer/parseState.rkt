;; Copyright (C) 2022  veden

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


(module AiState racket
  (provide (all-defined-out))

  (require math/statistics)

  (struct AiState (chunks
                   chunksLookup
                   minMaxes)
    #:transparent)

  (struct MinMax (min max)
    #:transparent)

  (struct ChunkRange (x
                      y
                      movement
                      base
                      player
                      resource
                      enemy
                      passable
                      tick
                      rating
                      nests
                      worms
                      rally
                      retreat
                      resourceGen
                      playerGen
                      deathGen
                      scoreResourceKamikaze
                      scoreResource
                      scoreSiegeKamikaze
                      scoreSiege
                      scoreAttackKamikaze
                      scoreAttack
                      pollution
                      aNe
                      aRNe
                      squads
                      baseAlign
                      hives
                      traps
                      utility
                      vg
                      kamikaze)
    #:transparent)

  (struct Chunk (x
                 y
                 movement
                 base
                 player
                 resource
                 enemy
                 passable
                 tick
                 rating
                 nests
                 worms
                 rally
                 retreat
                 resourceGen
                 playerGen
                 deathGen
                 scoreResourceKamikaze
                 scoreResource
                 scoreSiegeKamikaze
                 scoreSiege
                 scoreAttackKamikaze
                 scoreAttack
                 pollution
                 aNe
                 aRNe
                 squads
                 baseAlign
                 hives
                 traps
                 utility
                 vg
                 kamikaze)
    #:transparent)

  (require threading)

  (define (getFile filePath)
    (call-with-input-file filePath
      (lambda (port)
        (port->string port))))

  (define (stringToChunk str)
    (apply Chunk
           (map string->number
                (string-split str ","))))
  
  (define (chunk->string chunk)
    (string-append "x:" (~v (Chunk-x chunk)) "\n"
                   "y:" (~v (Chunk-y chunk)) "\n"
                   "m:" (~v (Chunk-movement chunk)) "\n"
                   "b:" (~v (Chunk-base chunk)) "\n"
                   "p:" (~v (Chunk-player chunk)) "\n"
                   "r:" (~v (Chunk-resource chunk)) "\n"
                   "e:" (~v (Chunk-enemy chunk)) "\n"
                   "pa:" (~v (Chunk-passable chunk)) "\n"
                   "t:" (~v (Chunk-tick chunk)) "\n"
                   "rat:" (~v (Chunk-rating chunk)) "\n"
                   "ne:" (~v (Chunk-nests chunk)) "\n"
                   "wo:" (~v (Chunk-worms chunk)) "\n"))

  (define (chunk->string2 chunk)
    (string-append "ral:" (~v (Chunk-rally chunk)) "\n"
                   "ret:" (~v (Chunk-retreat chunk)) "\n"
                   "rG:" (~v (Chunk-resourceGen chunk)) "\n"
                   "pG:" (~v (Chunk-playerGen chunk)) "\n"
                   "dG:" (~v (Chunk-deathGen chunk)) "\n"
                   "sA:" (~v (Chunk-scoreAttack chunk)) "\n"
                   "sAK:" (~v (Chunk-scoreAttackKamikaze chunk)) "\n"
                   "sS:" (~v (Chunk-scoreSiege chunk)) "\n"
                   "sSK:" (~v (Chunk-scoreSiegeKamikaze chunk)) "\n"
                   "sR:" (~v (Chunk-scoreResource chunk)) "\n"))

  (define (chunk->string3 chunk)
    (string-append "sRK:" (~v (Chunk-scoreResourceKamikaze chunk)) "\n"
                   "pu:" (~v (Chunk-pollution chunk)) "\n"
                   "aN:" (~v (Chunk-aNe chunk)) "\n"
                   "aRN:" (~v (Chunk-aRNe chunk)) "\n"
                   "sqs:" (~v (Chunk-squads chunk)) "\n"
                   "bA:" (~v (Chunk-baseAlign chunk)) "\n"
                   "H:" (~v (Chunk-hives chunk)) "\n"
                   "T:" (~v (Chunk-traps chunk)) "\n"
                   "U:" (~v (Chunk-utility chunk)) "\n"
                   "vg:" (~v (Chunk-vg chunk)) "\n"
                   "kam:" (~v (Chunk-kamikaze chunk)) "\n"))

  (define (normalizeRange xs)
    (let* ((sDev (stddev xs))
           (sMean (mean xs))
           (target (* 2.5 sDev))
           (cleanXs (filter (lambda (x)
                              (<= (abs (- x sMean)) target))
                            xs)))
      (MinMax (apply min cleanXs)
              (apply max cleanXs))))

  (define (findChunkPropertiesMinMax chunks)
    (let ((xs (map Chunk-x chunks))
          (ys (map Chunk-y chunks))
          (movements (map Chunk-movement chunks))
          (bases (map Chunk-base chunks))
          (players (map Chunk-player chunks))
          (resources (map Chunk-resource chunks))
          (enemy (map Chunk-enemy chunks))
          (passables (map Chunk-passable chunks))
          (ticks (map Chunk-tick chunks))
          (ratings (map Chunk-rating chunks))
          (nests (map Chunk-nests chunks))
          (worms (map Chunk-worms chunks))
          (rallys (map Chunk-rally chunks))
          (retreats (map Chunk-retreat chunks))
          (rGens (map Chunk-resourceGen chunks))
          (pGens (map Chunk-playerGen chunks))
          (dGens (map Chunk-deathGen chunks))
          (sRKs (map Chunk-scoreResourceKamikaze chunks))
          (sRs (map Chunk-scoreResource chunks))
          (sSKs (map Chunk-scoreSiegeKamikaze chunks))
          (sSs (map Chunk-scoreSiege chunks))
          (sAKs (map Chunk-scoreAttackKamikaze chunks))
          (sAs (map Chunk-scoreAttack chunks))
          (pol (map Chunk-pollution chunks))
          (aNe (map Chunk-aNe chunks))
          (aRNe (map Chunk-aRNe chunks))
          (sqs (map Chunk-squads chunks))
          (bA (map Chunk-baseAlign chunks))
          (H (map Chunk-hives chunks))
          (T (map Chunk-traps chunks))
          (U (map Chunk-utility chunks))
          (vg (map Chunk-vg chunks))
          (kamikaze (map Chunk-kamikaze chunks)))

      (ChunkRange (MinMax (apply min xs) (apply max xs))
                  (MinMax (apply min ys) (apply max ys))
                  (normalizeRange movements)
                  (normalizeRange bases)
                  (normalizeRange players)
                  (normalizeRange resources)
                  (normalizeRange enemy)
                  (MinMax (apply min passables) (apply max passables))
                  (normalizeRange ticks)
                  (normalizeRange ratings)
                  (MinMax (apply min nests) (apply max nests))
                  (MinMax (apply min worms) (apply max worms))
                  (MinMax (apply min rallys) (apply max rallys))
                  (MinMax (apply min retreats) (apply max retreats))
                  (MinMax (apply min rGens) (apply max rGens))
                  (MinMax (apply min pGens) (apply max pGens))
                  (MinMax (apply min dGens) (apply max dGens))
                  (normalizeRange sRKs)
                  (normalizeRange sRs)
                  (normalizeRange sSKs)
                  (normalizeRange sSs)
                  (normalizeRange sAKs)
                  (normalizeRange sAs)
                  (normalizeRange pol)
                  (MinMax (apply min aNe) (apply max aNe))
                  (MinMax (apply min aRNe) (apply max aRNe))
                  (MinMax (apply min sqs) (apply max sqs))
                  (MinMax (apply min bA) (apply max bA))
                  (MinMax (apply min H) (apply max H))
                  (MinMax (apply min T) (apply max T))
                  (MinMax (apply min U) (apply max U))
                  (MinMax (apply min vg) (apply max vg))
                  (normalizeRange kamikaze)
                  )))

  (define (readState filePath)
    (let* ((replayChunks (getFile filePath))
           (chunks (map stringToChunk (string-split replayChunks "\n")))
           (minMaxes (findChunkPropertiesMinMax chunks)))
      (AiState chunks
               (apply hash
                      (apply append
                             (map (lambda (chunk)
                                    (list (list (Chunk-x chunk)
                                                (Chunk-y chunk))
                                          chunk))
                                  chunks)))
               minMaxes)))

  (define (test)
    (AiState-minMaxes (readState "/data/games/factorio/script-output/rampantState.txt"))))
