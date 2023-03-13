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

(module AiVisualizer racket
  (provide (all-defined-out))

  (require "parseState.rkt")
  (provide (all-from-out "parseState.rkt"))
  (require racket/gui/base)
  (require plot)

  (define CHUNK_SIZE 32)

  (define INVALID_CHUNK (Chunk -1 -1 0 0 0
                               0 0 0 0 0
                               0 0 0 0 0
                               0 0 0 0 0
                               0 0 0 0 0
                               0 0 0 0 0
                               0 0))

  (define windowX 500)
  (define windowY 0)
  (define windowWidth 1024)
  (define windowHeight 1024)

  (define activeHighlight null)
  (define activeLayer "movement")

  (define (normalize v low high)
    (/ (- v low)
       (- high low)))

  (define (fromNormalize x low high)
    (+ (* (- high low)
          x)
       low))

  (define (roundTo x digits)
    (* (floor (/ x digits))
       digits))

  (define (visualize)
    (define frameWithEvents% (class frame%
                               (define/override (on-subwindow-char r event)
                                 (when (eq? (send event get-key-code) #\c)
                                   (exit))
                                 (super on-subwindow-char r event))
                               (super-new)))

    (define (newFrame width height x y [label ""])
      (new frameWithEvents%
           [label label]
           [width width]
           [height height]
           [x x]
           [y y]))

    (define templates (list '(250 750 0 0 "controls")
                            (list windowWidth windowHeight windowX windowY "map")))
    (define frames (map (lambda (frame)
                          (match-let (((list width height x y name) frame))
                            (newFrame width height x y name)))
                        templates))

    (define mainFrame (first frames))
    (define mapFrame (second frames))

    (define activeChunkSet null)
    (define activeChunkSetLookup null)
    (define activeChunkMinMaxSet null)

    (define topPanel (new panel%
                          [parent mainFrame]
                          (alignment '(left top))))

    (define botPanel (new panel%
                          [parent mainFrame]
                          (alignment '(right bottom))))

    (define statusBox (new message%
                           [parent topPanel]
                           [label (~v "")]
                           [vert-margin 16]))
    (define siteBox (new message%
                         [parent topPanel]
                         [label ""]
                         [vert-margin 30]))
    (define siteBox2 (new message%
                          [parent topPanel]
                          [label ""]
                          [horiz-margin 300]))
    (define siteBox3 (new message%
                          [parent topPanel]
                          [label ""]
                          [vert-margin 300]))

    (new button%
         [parent mainFrame]
         [label "Quit"]
         (callback (lambda (button event)
                     (exit))))

    (define tileWidth 0)
    (define tileHeight 0)

    (define minX 0)
    (define maxX 0)
    (define minY 0)
    (define maxY 0)

    (define canvasWithEvents% (class canvas%
                                (define/override (on-event event)
                                  (match (send event get-event-type)
                                    ((== 'motion) (displayChunk (send event get-x) (send event get-y)))
                                    ((== 'left-down) (displayHighlight (send event get-x) (send event get-y)))
                                    ((== 'right-down) (begin (set! activeHighlight null)
                                                             (refresh (send this get-dc))))
                                    (t (super on-event event))))
                                (super-new)))

    (define (refresh dc)
      (when (not (null? dc))
        (drawFrame dc)))

    (define drawFrame (lambda (context)
                        null))

    (define canvass (map (lambda (frame)
                           (let ((c (new canvasWithEvents%
                                         [parent frame]
                                         [paint-callback (lambda (canvas dc)
                                                           (drawFrame dc))])))
                             (send c set-canvas-background (make-object color% 111 111 111))
                             c))
                         (cdr frames)))
    (define dcs (map (lambda (canvas)
                       (send canvas get-dc))
                     canvass))

    (define (showVisual dc aiState)
      (let* ((chunkMinMaxes (AiState-minMaxes aiState))
             (minMaxX (ChunkRange-x chunkMinMaxes))
             (minMaxY (ChunkRange-y chunkMinMaxes)))
        (set! activeChunkSet (AiState-chunks aiState))
        (set! activeChunkMinMaxSet chunkMinMaxes)
        (set! activeChunkSetLookup (AiState-chunksLookup aiState))

        (when (Chunk? activeHighlight)
          (set! activeHighlight (findChunk (Chunk-x activeHighlight)
                                           (Chunk-y activeHighlight))))

        (set! minX (MinMax-min minMaxX))
        (set! maxX (MinMax-max minMaxX))
        (set! minY (MinMax-min minMaxY))
        (set! maxY (MinMax-max minMaxY))

        (set! tileWidth (ceiling (/ windowWidth (+ (abs (/ (- maxX minX) CHUNK_SIZE)) 3))))
        (set! tileHeight (ceiling (/ windowHeight (+ (abs (/ (- maxY minY) CHUNK_SIZE)) 3))))

        (refresh dc)

        (thread (lambda ()
                  (sync (filesystem-change-evt "/mnt/gallery/gameFiles/factorio/script-output/rampantState.txt"))
                  (showVisual dc (readState "/mnt/gallery/gameFiles/factorio/script-output/rampantState.txt"))))))

    (define dcMap (first dcs))

    (showVisual dcMap (readState "/mnt/gallery/gameFiles/factorio/script-output/rampantState.txt"))

    (define (chunkX->screenX x)
      (roundTo (* (normalize x minX maxX)
                  windowWidth)
               tileWidth))

    (define (chunkY->screenY y)
      (roundTo (* (normalize y minY maxY)
                  windowHeight)
               tileHeight))

    (define (screenX->chunkX x)
      (roundTo (fromNormalize (/ x windowWidth)
                              minX
                              maxX)
               CHUNK_SIZE))

    (define (screenY->chunkY y)
      (roundTo (fromNormalize (/ y windowHeight)
                              minY
                              maxY)
               CHUNK_SIZE))

    (set! drawFrame (lambda (context)
                      (send context suspend-flush)
                      (send context clear)
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
                                         (chunkRangeField activeChunkMinMaxSet))))
                             activeChunkSet))
                      (send context resume-flush)))

    (define (findChunk x y)
      (hash-ref activeChunkSetLookup (list x y) INVALID_CHUNK))

    (define (displayChunk x y)
      (let ((chunk (if (Chunk? activeHighlight)
                       activeHighlight
                       (findChunk (screenX->chunkX x)
                                  (screenY->chunkY y)))))
        (send siteBox set-label
              (chunk->string chunk))
        (send siteBox2 set-label
              (chunk->string2 chunk))
        (send siteBox3 set-label
              (chunk->string3 chunk))))

    (define (displayHighlight x y)
      ;; (display (list (screenX->chunkX x)
      ;;                (screenY->chunkY y)
      ;;              x
      ;;              y))
      ;; (display "\n")

      (let ((chunk (findChunk (screenX->chunkX x)
                              (screenY->chunkY y))))
        (set! activeHighlight chunk))
      (refresh dcMap))

    (define (normalizeLog x low high)
      (if (= (- high low) 0)
          0
          (/ (- x low)
             (- high low))))

    (define (scaleColor dc value low high)
      (let* ((v (normalizeLog value low high))
             (r (cond ((= v 0) 0)
                      ((> 0.75 v) 150)
                      ((> 0.50 v) 100)
                      (#t 50)))
             (g (inexact->exact (round (* v 255)))))
        (send dc
              set-brush
              (make-object color%
                           (min (max r 0) 255)
                           (min (max g 0) 255)
                           0)
              'solid)))


    (new button%
         [parent mainFrame]
         [label "Retry"]
         (callback (lambda (button event)
                     (showVisual dcMap (readState "/mnt/gallery/gameFiles/factorio/script-output/rampantState.txt")))))

    (new radio-box%
         [label "Show Layer"]
         [choices (list "movement" "base" "player" "resource" "enemy" "passable" "tick" "rating" "nests" "worms" "rally" "retreat" "resourceGen" "playerGen" "deathGen" "scoreResourceKamikaze" "scoreResource" "scoreSiegeKamikaze" "scoreSiege" "scoreAttackKamikaze" "scoreAttack" "pollution" "aNe" "aRNe" "squads" "baseAlign" "hives" "traps" "utility" "vg")]
         [selection 0]
         [parent botPanel]
         (callback (lambda (radioButton event)
                     (set! activeLayer (send radioButton get-item-label (send radioButton get-selection)))
                     (refresh dcMap))))

    (map (lambda (f)
           (send f show #t))
         frames)))
