(module Visualizer racket

  (require "parseState.rkt")
  (require racket/gui/base)
  (require plot)  

  (define CHUNK_SIZE 32)

  (define INVALID_CHUNK (Chunk -1 -1 0 0 0 0 0 0 0))

  (define windowX 500)
  (define windowY 0)
  (define windowWidth 1024)
  (define windowHeight 1024)

  (define activeHighlight null)
  (define activeLayer "movement")

  (define (normalize v low high)
    (/ (- v low)
       (- high low)))

  (define (roundTo x digits)
    (* (floor (/ x digits))
       digits))    
  
  (define (showVisual aiState)
    (match-let* (((AiState chunks chunkLookups chunkMinMaxes) aiState)
                 ((ChunkRange (MinMax minX maxX)
                              (MinMax minY maxY)
                              (MinMax minMovement maxMovement)
                              (MinMax minBase maxBase)
                              (MinMax minPlayer maxPlayer)
                              (MinMax minResource maxResource)
                              (MinMax minPassable maxPassable)
                              (MinMax minTick maxTick)
                              (MinMax minRating maxRating)) chunkMinMaxes))

      ;; (pretty-display aiState)
      ;; (display "\n")

      (define frameWithEvents% (class frame%
                                 (define/override (on-subwindow-char r event)
                                   (when (eq? (send event get-key-code) #\c)
                                     (exit))
                                   (super on-subwindow-char r event))
                                 (super-new)))
      
      (define canvasWithEvents% (class canvas%
                                  (define/override (on-event event)
                                    (match (send event get-event-type)
                                      ((== 'motion) (displayChunk (send event get-x) (send event get-y)))
                                      ((== 'left-down) (displayHighlight (send event get-x) (send event get-y)))
                                      ((== 'right-down) (begin (set! activeHighlight null)
                                                               (refresh)))
                                      (_ #t)))
                                  (super-new)))
      
      (define (newFrame width height x y [label ""])
        (new frameWithEvents%
             [label label]
             [width width]
             [height height]
             [x x]
             [y y]))

      (define templates (list '(250 500 0 0 "controls")
                              (list windowWidth windowHeight windowX windowY "map")))
      (define frames (map (lambda (frame)
                            (match-let (((list width height x y name) frame))
                              (newFrame width height x y name)))
                          templates))

      (define mainFrame (first frames))
      (define mapFrame (second frames))
      
      (define canvass (map (lambda (frame)
                             (let ((c (new canvasWithEvents%
                                           [parent frame]
                                           [paint-callback (lambda (canvas dc)
                                                             (drawFrame dc))])))
                               (send c set-canvas-background (make-object color% 0 0 0))
                               c))
                           (cdr frames)))
      (define dcs (map (lambda (canvas)
                         (send canvas get-dc))
                       canvass))

      (define dcMap (first dcs))

      (define tileWidth (ceiling (/ windowWidth (abs (/ (- maxX minX) CHUNK_SIZE)))))
      (define tileHeight (ceiling (/ windowHeight (+ (abs (/ (- maxY minY) CHUNK_SIZE)) 1))))

      ;; (print (list (exact->inexact tileWidth)
      ;;              (exact->inexact tileHeight)))
      ;; (display "\n")
      
      (define (chunkX->screenX x)
        (roundTo (* (normalize x minX maxX)
                    windowWidth)
                 tileWidth))

      (define (chunkY->screenY y)
        (roundTo (+ (- windowHeight
                    (* (normalize y minY maxY)
                       windowHeight)))
                 tileHeight))

      (define (screenX->chunkX x)
        (+ (* (ceiling (/ x tileWidth))
              CHUNK_SIZE)
           minX))
      
      (define (screenY->chunkY y)
        (- maxY
           (* (floor (/ y tileHeight))
              CHUNK_SIZE)))
      
      (define (drawFrame context)
        (send context suspend-flush)
        (let ((chunkField (eval (string->symbol (string-append "Chunk-" activeLayer))))
              (chunkRangeField (eval (string->symbol (string-append "ChunkRange-" activeLayer)))))
          (map (lambda (chunk)
                 (let ((x (chunkX->screenX (Chunk-x chunk)))
                       (y (chunkY->screenY (Chunk-y chunk))))
                   (if (eq? activeHighlight chunk)
                       (send context set-pen (make-object color% 255 255 255) 1 'solid)
                       (send context set-pen (make-object color% 0 0 0) 1 'solid))
                   (define (dcDraw dc property minMax)
                     (scaleColor dc property (MinMax-min minMax) (MinMax-max minMax))
                     (send dc draw-rectangle x y tileWidth tileHeight))
                   (dcDraw context
                           (chunkField chunk)
                           (chunkRangeField chunkMinMaxes))))
               chunks))
        (send context resume-flush))

      (define (refresh)
        (drawFrame dcMap))

      (define (findChunk x y)
        (hash-ref chunkLookups (list x y) INVALID_CHUNK))
      
      (define (displayChunk x y)
        (send siteBox set-label
              (chunk->string (findChunk (screenX->chunkX x)
                                        (screenY->chunkY y)))))

      (define (displayHighlight x y)
        ;; (print (list x y))
        ;; (display "\n")
        ;; (print (list (screenX->chunkX x) (screenY->chunkY y)))
        ;; (display "\n---\n")
        (let ((chunk (findChunk (screenX->chunkX x)
                                (screenY->chunkY y))))
          (set! activeHighlight chunk))
        (refresh))

      (define (scaleColor dc value low high)
        (define v (/ (- value low)
                     (- high low)))
        (define r (if (= v 0)
                      0
                      (if (> 0.75 v)
                          150
                          (if (> 0.50 v)
                              100
                              50))))
        (define g (inexact->exact (round (* v 255))))
        (send dc set-brush (make-object color% r g 0) 'solid))

      (define panel (new panel%
                         [parent mainFrame]
                         (alignment '(left top))))

      (define statusBox (new message%
                             [parent panel]
                             [label (~v "")]
                             [vert-margin 16]))
      (define siteBox (new message%
                           [parent panel]
                           [label ""]
                           [vert-margin 30]))
      
      (new radio-box%
           [label "Show Layer"]
           [choices (list "movement" "base" "player" "resource" "passable" "tick" "rating")]
           [selection 0]
           [parent mainFrame]
           (callback (lambda (radioButton event)
                       (set! activeLayer (send radioButton get-item-label (send radioButton get-selection)))
                       (refresh))))

      (new button%
           [parent mainFrame]
           [label "Refresh"]
           (callback (lambda (button event)
                       (exit))))
      
      (new button%
           [parent mainFrame]
           [label "Quit"]
           (callback (lambda (button event)
                       (exit))))

      (map (lambda (f)
             (send f show #t))
           frames)
      'showing))

  (define (test)
    (showVisual (readState "/data/games/factorio/script-output/rampantState.txt"))))
