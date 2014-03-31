#lang racket

;; render images and sounds

(provide render-maze
         render-score
         render-bloke
         render-ghost
         render-node-halo ;; debug
         render-dot-eaten
         render-powerpill-eaten
         unrender
         render-ghost-score
         unrender-ghost-score
         render-ghost-eaten
         render-lose-life)

(require racket/gui/base)
(require "bitmaps.rkt"
         "maze.rkt"
         "state.rkt")

(define cell-size 20)

(define ghost-flash-period 3)
(define ghost-flash-point 21)

(define wall-colour (send the-color-database find-color "DodgerBlue"))
(define dot-colour (send the-color-database find-color "Ivory"))
(define powerpill-colour (send the-color-database find-color "Wheat"))
(define player-colour (send the-color-database find-color "Yellow"))
(define ghost1-colour (send the-color-database find-color "Orange"))
(define ghost2-colour (send the-color-database find-color "OrangeRed"))
(define ghost3-colour (send the-color-database find-color "LightPink"))
(define ghost4-colour (send the-color-database find-color "Aqua"))
(define scaredghost-colour (send the-color-database find-color "DodgerBlue"))
(define scaredghostending-colour (send the-color-database find-color "White"))
(define forcefield-colour (send the-color-database find-color "Lime"))
(define ghostscore-colour (send the-color-database find-color "Turquoise"))
(define other-colour (send the-color-database find-color "Black"))

(define (cell-bitmap cell cell-above cell-below cell-left cell-right)
  (let* ((wall-above? (eq? cell-above 'wall))
         (wall-below? (eq? cell-below 'wall))
         (wall-left? (eq? cell-left 'wall))
         (wall-right? (eq? cell-right 'wall))
         (adjacent-walls (+ (if wall-above? 1 0)
                            (if wall-below? 1 0)
                            (if wall-left? 1 0)
                            (if wall-right? 1 0))))
    (cond ((eq? cell 'gap) blank)
          ((eq? cell 'dot) dot)
          ((eq? cell 'powerpill) pill)
          ((eq? cell 'forcefield) forcefield)
          ((eq? cell 'wall)
           (case adjacent-walls
             ((0) loan-square)
             ((1) (cond (wall-above? bottom-terminator)
                        (wall-below? top-terminator)
                        (wall-left? right-terminator)
                        (else left-terminator)))
             ((2) (cond (wall-above? (cond (wall-below? vertical)
                                           (wall-left? bottom-right-corner)
                                           (else bottom-left-corner)))
                        (wall-below? (if wall-left? 
                                         top-right-corner 
                                         top-left-corner))
                        (else horizontal)))
             ((3) (cond ((not wall-above?) top-t)
                        ((not wall-below?) bottom-t)
                        ((not wall-left?) left-t)
                        (else right-t)))
             ((4) cross)))
          (else blank))))

(define (cell-colour obj)
  (case obj
    ('wall wall-colour)
    ('dot dot-colour)
    ('powerpill powerpill-colour)
    ('bloke player-colour)
    ('forcefield forcefield-colour)
    (else other-colour)))

(define (render-maze maze-canvas maze)
  (send maze-canvas min-height (* cell-size (maze-height maze)))
  (send maze-canvas min-width (* cell-size (maze-width maze)))
  (let ((maze-dc (send maze-canvas get-dc)))
    (send maze-dc set-background "Black")
    (let loopy ((row 0))
      (let loopx ((col 0))
        (cond ((= row (maze-height maze)) 0)
              ((= col (maze-width maze)) (loopy (+ row 1)))
              (else
               (let ((cell (maze-cell maze col row)))
                 (send maze-dc 
                       draw-bitmap 
                       (cell-bitmap cell
                                    (maze-cell maze col (- row 1))
                                    (maze-cell maze col (+ row 1))
                                    (maze-cell maze (- col 1) row)
                                    (maze-cell maze (+ col 1) row))
                       (* col cell-size) 
                       (* row cell-size)
                       'opaque
                       (cell-colour cell))
                 (loopx (+ col 1)))))))))


(define (unrender x y dc maze-state)
  (let loop ((ux (floor x)) (uy (floor y)))
    (cond ((> uy (ceiling y)) (void))
          ((> ux (ceiling x)) (loop (floor x) (+ uy 1)))
          (else
            (let ((reveal (cell-state maze-state ux uy)))
              (send dc draw-bitmap
                       (cell-bitmap reveal
                                    (cell-state maze-state ux (- uy 1))
                                    (cell-state maze-state ux (+ uy 1))
                                    (cell-state maze-state (- ux 1) uy)
                                    (cell-state maze-state (+ ux 1) uy))
                       (* ux cell-size)
                       (* uy cell-size)
                       'opaque
                       (cell-colour reveal))
              (loop (+ ux 1) uy))))))

(define (render-bloke x y direction dc frame-number)
  (send dc draw-bitmap
           (case (remainder (quotient frame-number 2) 4)
              ((0) bloke-shut)
              ((1 3) (case direction
                       ('up bloke-ajar-up)
                       ('down bloke-ajar-down)
                       ('left bloke-ajar-left)
                       (else bloke-ajar-right)))
              ((2) (case direction
                     ('up bloke-open-up)
                     ('down bloke-open-down)
                     ('left bloke-open-left)
                     (else bloke-open-right))))
           (* x cell-size)
           (* y cell-size) 
           'solid
           player-colour))

(define (render-ghost x y id mode flee-frames-left dc)
  (send dc draw-bitmap
           (if (eq? mode 'fleeing) scared-ghost ghost)
           (* x cell-size)
           (* y cell-size)
           'solid  ;; not sure if I really like this setting for ghosts...
           (cond ((eq? mode 'fleeing) 
                  (if (and (< flee-frames-left ghost-flash-point) 
                           (zero? (remainder flee-frames-left ghost-flash-period)))
                      scaredghostending-colour
                      scaredghost-colour))
                 (else (case (remainder id 4)
                         ((1) ghost1-colour)
                         ((2) ghost2-colour)
                         ((3) ghost3-colour)
                         ((0) ghost4-colour))))))

;; debug
(define (render-node-halo x y id dc)
  (send dc draw-bitmap
           node-halo
           (* x cell-size)
           (* y cell-size)
           'solid
           (case (remainder id 4)
             ((1) ghost1-colour)
             ((2) ghost2-colour)
             ((3) ghost3-colour)
             ((0) ghost4-colour))))


(define (ghost-score-x-y player-x player-y ghost-x ghost-y)
  (let ((x (cond ((> ghost-x player-x) (+ player-x 1))
                 ((< ghost-x player-x) (- player-x 1))
                 (else ghost-x)))
        (y (cond ((> ghost-y player-y) (+ player-y 1))
                 ((< ghost-y player-y) (- player-y 1))
                 (else ghost-y))))
    (values x y)))

(define (render-ghost-score player-x player-y ghost-x ghost-y score dc)
  (let-values (((x y) (ghost-score-x-y player-x player-y ghost-x ghost-y)))
    (send dc draw-bitmap
             (case score
               ((200) score-200)
               ((400) score-400)
               ((800) score-800)
               ((1600) score-1600))
             (* x cell-size)
             (* y cell-size)
             'opaque
             ghostscore-colour)))

(define (unrender-ghost-score player-x player-y ghost-x ghost-y maze-state dc)
  (let-values (((x y) (ghost-score-x-y player-x player-y ghost-x ghost-y)))
    (unrender x y dc maze-state)))

(define (render-score score-canvas score-dc lives score)
  (send score-canvas min-height 30)
  (send score-canvas min-width 200)
  (send score-dc set-background "Black")
  (send score-dc set-text-foreground "White")
  (send score-dc set-font (make-object font% 20
                                             'modern
                                             'normal
                                             'bold
                                             #f
                                             'default
                                             #t
                                             'aligned))
  (send score-dc clear)
  (let loop ((life 1))
    (when (<= life lives)
          (send score-dc draw-bitmap
                         bloke-ajar-right
                         (* life 2 cell-size)
                         5
                         'opaque
                         player-colour)
                         (loop (+ life 1))))
  (send score-dc draw-text (format "~a" score) (* 10 cell-size) 5))

;; sounds are part of the game display in my world...
(define (sound-effect sound-file)
  (when (and (sound-on?) (file-exists? sound-file))
        (play-sound sound-file #t)))

(define (render-dot-eaten)
  (sound-effect "eatdot.wav"))

(define (render-powerpill-eaten)
  (sound-effect "eatpill.wav"))

(define (render-ghost-eaten)
  (sound-effect "eatghost.wav"))

(define (render-lose-life)
  (sound-effect "loselife.wav"))



