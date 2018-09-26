(module AiState racket
  (provide (all-defined-out))

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
                      attackScore
                      settleScore
                      seigeScore)
    #:transparent)
  
  (struct Chunk (seigeScore
                 settleScore
                 attackScore
                 x
                 y
                 movement
                 base
                 player
                 resource
                 passable
                 tick
                 rating
                 nests
                 worms
                 rally
                 retreat
                 resourceGen
                 playerGen
                 deathGen)
    #:transparent)
  
  (require threading)

  (define (getFile filePath)
    (call-with-input-file filePath
      (lambda (port)
        (port->string port))))

  (define (stringToChunk str)
    (match-let (((list movement base player resource passable tick rating x y nest
                       worms rally retreat resourceGen playerGen deathGen) (string-split str ",")))
      (apply Chunk
             (cons (+ (* 2 (string->number movement))
                      (string->number resource)
                      (string->number base)
                      (* (string->number player) 500))
                   (cons (+ (* 2 (string->number movement))
                            (string->number resource))
                         (cons (+ (* 2 (string->number movement))
                                  (string->number base)
                                  (* (string->number player) 500))
                               (map string->number
                                    (list x y movement base player resource passable tick rating nest
                                          worms rally retreat resourceGen playerGen deathGen))))))))

  (define (chunk->string chunk)
    (string-append "x: " (~v (Chunk-x chunk)) "\n"
                   "y: " (~v (Chunk-y chunk)) "\n"
                   "m: " (~v (Chunk-movement chunk)) "\n"
                   "b: " (~v (Chunk-base chunk)) "\n"
                   "p: " (~v (Chunk-player chunk)) "\n"
                   "r: " (~v (Chunk-resource chunk)) "\n"
                   "pass: " (~v (Chunk-passable chunk)) "\n"
                   "tic: " (~v (Chunk-tick chunk)) "\n"
                   "rat: " (~v (Chunk-rating chunk)) "\n"
                   "ne: " (~v (Chunk-nests chunk)) "\n"
                   "wo: " (~v (Chunk-worms chunk)) "\n"
                   "rall: " (~v (Chunk-rally chunk)) "\n"
                   "retr: " (~v (Chunk-retreat chunk)) "\n"
                   "rGen: " (~v (Chunk-resourceGen chunk)) "\n"))

  (define (chunk->string2 chunk)
    (string-append "pGen: " (~v (Chunk-playerGen chunk)) "\n"
                   "dGen: " (~v (Chunk-deathGen chunk)) "\n"
                   "aSco: " (~v (Chunk-attackScore chunk)) "\n"
                   "sSco: " (~v (Chunk-settleScore chunk)) "\n"
                   "sSei: " (~v (Chunk-seigeScore chunk)) "\n"))

  (define (findChunkPropertiesMinMax chunks)
    (let ((xs (map Chunk-x chunks))
          (ys (map Chunk-y chunks))
          (movements (map Chunk-movement chunks))
          (bases (map Chunk-base chunks))
          (players (map Chunk-player chunks))
          (resources (map Chunk-resource chunks))
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
          (aSco (map Chunk-attackScore chunks))
          (sSco (map Chunk-settleScore chunks))
          (sSei (map Chunk-seigeScore chunks)))
      (ChunkRange (MinMax (apply min xs) (apply max xs))
                  (MinMax (apply min ys) (apply max ys))
                  (MinMax (apply min movements) (apply max movements))
                  (MinMax (apply min bases) (apply max bases))
                  (MinMax (apply min players) (apply max players))
                  (MinMax (apply min resources) (apply max resources))
                  (MinMax (apply min passables) (apply max passables))
                  (MinMax (apply min ticks) (apply max ticks))
                  (MinMax (apply min ratings) (apply max ratings))
                  (MinMax (apply min nests) (apply max nests))
                  (MinMax (apply min worms) (apply max worms))
                  (MinMax (apply min rallys) (apply max rallys))
                  (MinMax (apply min retreats) (apply max retreats))
                  (MinMax (apply min rGens) (apply max rGens))
                  (MinMax (apply min pGens) (apply max pGens))
                  (MinMax (apply min dGens) (apply max dGens))
                  (MinMax (apply min aSco) (apply max aSco))
                  (MinMax (apply min sSco) (apply max sSco))
                  (MinMax (apply min sSei) (apply max sSei)))))

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
    (readState "/data/games/factorio/script-output/rampantState.txt")))
