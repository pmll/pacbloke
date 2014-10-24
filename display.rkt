#lang racket

;; render images, sounds and animated sequences

(provide register-maze-canvas
         render-maze
         render-score
         render-player
         render-ghosts
         render-target-halos ;; debug
         render-dot-eaten
         render-powerpill-eaten
         unrender-roamer
         unrender-ghosts
         unrender-target-halos
         render-title
         render-ghost-eaten
         render-lose-life)

(require racket/gui/base)
(require "bitmaps.rkt"
         "maze.rkt"
         "movables.rkt"
         "state.rkt"
         "settings.rkt")

(define cell-size 20)

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
(define deadghost-colour (send the-color-database find-color "DimGray"))
(define forcefield-colour (send the-color-database find-color "Lime"))
(define ghostscore-colour (send the-color-database find-color "Cyan"))
(define other-colour (send the-color-database find-color "Black"))

(define maze-canvas #f)
(define maze-dc #f)

(define (register-maze-canvas mc)
  (set! maze-canvas mc)
  (set! maze-dc (send maze-canvas get-dc)))

(define (render-maze-bitmap bitmap x y mode colour)
  (send maze-dc draw-bitmap bitmap (* x cell-size) (* y cell-size) mode colour))

(define (cell-bitmap cell cell-above cell-below cell-left cell-right)
  (cond ((eq? cell 'gap) blank)
        ((eq? cell 'dot) dot)
        ((eq? cell 'powerpill) pill)
        ((eq? cell 'forcefield) forcefield)
        ((eq? cell 'wall)
           (let* ((wall-above? (eq? cell-above 'wall))
                  (wall-below? (eq? cell-below 'wall))
                  (wall-left? (eq? cell-left 'wall))
                  (wall-right? (eq? cell-right 'wall))
                  (adjacent-walls (+ (if wall-above? 1 0)
                                     (if wall-below? 1 0)
                                     (if wall-left? 1 0)
                                     (if wall-right? 1 0))))
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
               ((4) cross))))
        (else blank)))

(define (cell-colour obj)
  (case obj
    ('wall wall-colour)
    ('dot dot-colour)
    ('powerpill powerpill-colour)
    ('bloke player-colour)
    ('forcefield forcefield-colour)
    (else other-colour)))

(define (render-maze maze)
  (send maze-canvas min-height (* cell-size (maze-height maze)))
  (send maze-canvas min-width (* cell-size (maze-width maze)))
  (send maze-dc set-background "Black")
  (let loopy ((row 0))
    (let loopx ((col 0))
      (cond ((= row (maze-height maze)) 0)
            ((= col (maze-width maze)) (loopy (+ row 1)))
            (else
              (let ((cell (maze-cell maze col row)))
                (render-maze-bitmap (cell-bitmap cell
                                                 (maze-cell maze col (- row 1))
                                                 (maze-cell maze col (+ row 1))
                                                 (maze-cell maze (- col 1) row)
                                                 (maze-cell maze (+ col 1) row))
                                    col
                                    row
                                    'opaque
                                    (cell-colour cell))
                (loopx (+ col 1))))))))

(define (unrender x y maze-state)
  (let loop ((ux (floor x)) (uy (floor y)))
    (cond ((> uy (ceiling y)) (void))
          ((> ux (ceiling x)) (loop (floor x) (+ uy 1)))
          (else
            (let ((reveal (cell-state maze-state ux uy)))
              (render-maze-bitmap
                (cell-bitmap reveal
                             (cell-state maze-state ux (- uy 1))
                             (cell-state maze-state ux (+ uy 1))
                             (cell-state maze-state (- ux 1) uy)
                             (cell-state maze-state (+ ux 1) uy))
                ux
                uy
                'opaque
                (cell-colour reveal))
              (loop (+ ux 1) uy))))))

(define (unrender-roamer r maze-state)
  (unrender (roamer-x r) (roamer-y r) maze-state))

(define (render-player player frame-number)
  (render-maze-bitmap (case (remainder (quotient frame-number 2) 4)
                        ((0) bloke-shut)
                        ((1 3) (case (roamer-direction player)
                                 ('up bloke-ajar-up)
                                 ('down bloke-ajar-down)
                                 ('left bloke-ajar-left)
                                 (else bloke-ajar-right)))
                        ((2) (case (roamer-direction player)
                               ('up bloke-open-up)
                               ('down bloke-open-down)
                               ('left bloke-open-left)
                               (else bloke-open-right))))
                      (roamer-x player)
                      (roamer-y player) 
                      'solid
                      player-colour))

(define (render-ghost g frame-number)
  (define mode (ghost-mode g))
  (render-maze-bitmap (case mode
                        ('chasing normal-ghost)
                        ('fleeing scared-ghost)
                        ('returning dead-ghost))
                      (roamer-x (ghost-roamer g))
                      (roamer-y (ghost-roamer g))
                      'solid  ;; not sure if I really like this setting for ghosts...
                      (cond ((eq? mode 'fleeing)
                             (let ((flee-frames-left (+ (- scared-ghost-frames
                                                           frame-number)
                                                        (ghost-flee-frame g))))
                               (if (and (< flee-frames-left ghost-flash-point)
                                        (zero? (remainder flee-frames-left
                                                          ghost-flash-period)))
                                   scaredghostending-colour
                                   scaredghost-colour)))
                            ((eq? mode 'returning) deadghost-colour)
                            (else (case (remainder (ghost-id g) 4)
                                    ((1) ghost1-colour)
                                    ((2) ghost2-colour)
                                    ((3) ghost3-colour)
                                    ((0) ghost4-colour))))))

(define (render-ghosts ghost-lst frame-number)
  (for ((g ghost-lst)) (render-ghost g frame-number)))
 
(define (unrender-ghosts ghost-lst maze-state)
  (for ((g ghost-lst)) (unrender-roamer (ghost-roamer g) maze-state)))

;; debug - halo functions
(define (render-target-halos ghost-lst)
  (for ((g ghost-lst))
       (render-maze-bitmap node-halo
                           (node-x (target-node (ghost-target g)))
                           (node-y (target-node (ghost-target g)))
                           'solid
                           (case (remainder (ghost-id g) 4)
                             ((1) ghost1-colour)
                             ((2) ghost2-colour)
                             ((3) ghost3-colour)
                             ((0) ghost4-colour)))))

(define (unrender-target-halos ghost-lst maze-state)
  (for ((g ghost-lst)) (unrender (node-x (target-node (ghost-target g)))
                                 (node-y (target-node (ghost-target g)))
                                 maze-state)))

(define (ghost-score-x-y player-x player-y ghost-x ghost-y)
  (let ((x (cond ((> ghost-x player-x) (+ player-x 1))
                 ((< ghost-x player-x) (- player-x 1))
                 (else ghost-x)))
        (y (cond ((> ghost-y player-y) (+ player-y 1))
                 ((< ghost-y player-y) (- player-y 1))
                 (else ghost-y))))
    (values x y)))

(define (render-ghost-score x y score)
  (render-maze-bitmap (case score
                        ((200) score-200)
                        ((400) score-400)
                        ((800) score-800)
                        ((1600) score-1600))
                        x
                        y
                        'solid
                        ghostscore-colour))

(define (render-ghost-eaten g player score maze-state frame-number)
  (let-values (((score-x score-y) (ghost-score-x-y (roamer-x player)
                                                   (roamer-y player)
                                                   (roamer-x (ghost-roamer g))
                                                   (roamer-y (ghost-roamer g)))))
    (send maze-canvas suspend-flush)
    (unrender (roamer-x (ghost-roamer g))
              (roamer-y (ghost-roamer g))
              maze-state)
    ; repaint player to restore that part wiped out by ghost
    (render-player player frame-number)
    (render-ghost-score score-x score-y score)
    (send maze-canvas resume-flush)
    (send maze-canvas flush)
    (sound-effect "eatghost.wav")
    ;; pause just long enough to read the score
    (sleep 0.5)
    (send maze-canvas suspend-flush)
    (unrender score-x score-y maze-state)
    (send maze-canvas resume-flush)
    (send maze-canvas flush))) 

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

(define (render-title title-canvas)
  (define title-dc (send title-canvas get-dc))
  (define (title-bitmap x y bitmap colour)
     (send title-dc draw-bitmap bitmap x y 'solid colour))
  (define (title-text x y text) (send title-dc draw-text text x y))
  (send title-canvas min-height 460)
  (send title-canvas min-width 460)
  (send title-dc set-background "Black")
  (send title-dc set-text-foreground "White")
  (send title-dc set-font (make-object font% 20
                                             'modern
                                             'normal
                                             'bold
                                             #f
                                             'default
                                             #t
                                             'aligned))
  (send title-dc clear)
  (title-text 181 5 "PACBLOKE")
  (title-bitmap 139 40 bloke-ajar-right player-colour)
  (for-each (lambda (x) (title-bitmap x 40 dot dot-colour))
            '(159 179 199 219 239 259 279))
  (title-bitmap 299 40 bloke-ajar-left player-colour)
  (title-text 10 80 "Eat this")
  (title-text 170 80 "Get")
  (title-bitmap 10 110 dot dot-colour)
  (title-text 170 110 "10 Points")
  (title-bitmap 10 140 pill powerpill-colour)
  (title-text 170 140 "50 Points")
  (for-each (lambda (x) (title-bitmap x 170 scared-ghost scaredghost-colour))
            '(10 40 70 100))
  (title-text 170 170 "200/400/800/1600 Points")
  (for-each (lambda (y colour) (title-bitmap 10 y normal-ghost colour))
            '(200 230 260 290)
            (list ghost1-colour ghost2-colour ghost3-colour ghost4-colour))
  (for-each (lambda (y) (title-text 170 y "DEAD")) '(200 230 260 290))
  (title-text 10 330 "Move using the arrow keys")
  (title-text 10 360 "'P' to play / pause")
  (title-text 10 390 "'S' to toggle sound")
  (title-text 10 420 "'Q' to quit")
  (send title-dc set-text-foreground "Red")
  (title-text 260 390 (if (sound-on?) "On" "Off")))

;; sounds are part of the game display in my world...
(define (sound-effect sound-file)
  (when (and (sound-on?) (file-exists? sound-file))
        (play-sound sound-file #t)))

(define (render-dot-eaten)
  (sound-effect "eatdot.wav"))

(define (render-powerpill-eaten)
  (sound-effect "eatpill.wav"))

(define (render-lose-life player maze-state ghost-lst)
  (define x (roamer-x player))
  (define y (roamer-y player))
  (define frame-length 0.1)
  (define (render-frame player-bitmap)
    (send maze-canvas suspend-flush)
    (unrender x y maze-state)
    ; repaint ghosts in case unrendering player has overwritten them
    (render-ghosts ghost-lst 0)
    (render-maze-bitmap player-bitmap x y 'solid player-colour)
    (send maze-canvas resume-flush)
    (send maze-canvas flush)
    (sleep frame-length))
  (sound-effect "loselife.wav")
  (for-each render-frame player-death-throes)) 
