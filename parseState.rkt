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
                      playerGen)
    #:transparent)
  
  (struct Chunk (x
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
                 playerGen)
    #:transparent)
  
  (require threading)

  (define (getFile filePath)
    (call-with-input-file filePath
      (lambda (port)
        (port->string port))))

  (define (stringToChunk str)
    (match-let (((list movement base player resource passable tick rating x y nest worms rally retreat resourceGen playerGen) (string-split str ",")))
      (apply Chunk
             (map string->number
                  (list x y movement base player resource passable tick rating nest worms rally retreat resourceGen playerGen)))))

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
                   "rGen: " (~v (Chunk-resourceGen chunk)) "\n"
                   "pGen: " (~v (Chunk-playerGen chunk)) "\n"))
  
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
          (pGens (map Chunk-playerGen chunks)))
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
                  (MinMax (apply min pGens) (apply max pGens)))))
  
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
