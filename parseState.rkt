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
                      rating)
    #:transparent)
  
  (struct Chunk (x
                 y
                 movement
                 base
                 player
                 resource
                 passable
                 tick
                 rating)
    #:transparent)
  
  (require threading)

  (define (getFile filePath)
    (call-with-input-file filePath
      (lambda (port)
        (port->string port))))

  (define (stringToChunk str)
    (match-let (((list movement base player resource passable tick rating x y) (string-split (substring str
                                                                                                        12
                                                                                                        (- (string-length str)
                                                                                                           14))
                                                                                             ",")))
      (apply Chunk
             (map (lambda (x)
                    (string->number (second (string-split x
                                                          "="))))
                  (list x y movement base player resource passable tick rating)))))

  (define (chunk->string chunk)
    (string-append "x: " (~v (Chunk-x chunk)) "\n"
                   "y: " (~v (Chunk-y chunk)) "\n"
                   "movement: " (~v (Chunk-movement chunk)) "\n"
                   "base: " (~v (Chunk-base chunk)) "\n"
                   "player: " (~v (Chunk-player chunk)) "\n"
                   "resource: " (~v (Chunk-resource chunk)) "\n"
                   "passable: " (~v (Chunk-passable chunk)) "\n"
                   "tick: " (~v (Chunk-tick chunk)) "\n"
                   "rating: " (~v (Chunk-rating chunk)) "\n"))
  
  (define (findChunkPropertiesMinMax chunks)
    (let ((xs (map Chunk-x chunks))
          (ys (map Chunk-y chunks))
          (movements (map Chunk-movement chunks))
          (bases (map Chunk-base chunks))
          (players (map Chunk-player chunks))
          (resources (map Chunk-resource chunks))
          (passables (map Chunk-passable chunks))
          (ticks (map Chunk-tick chunks))
          (ratings (map Chunk-rating chunks)))
      (ChunkRange (MinMax (apply min xs) (apply max xs))
                  (MinMax (apply min ys) (apply max ys))
                  (MinMax (apply min movements) (apply max movements))
                  (MinMax (apply min bases) (apply max bases))
                  (MinMax (apply min players) (apply max players))
                  (MinMax (apply min resources) (apply max resources))
                  (MinMax (apply min passables) (apply max passables))
                  (MinMax (apply min ticks) (apply max ticks))
                  (MinMax (apply min ratings) (apply max ratings)))))
  
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
