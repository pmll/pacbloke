#lang racket

;; render images and sounds

(provide render-maze
         render-score
         render-bloke
         render-ghost
         render-dot-eaten
         render-powerpill-eaten
         unrender)

(require racket/gui/base)
(require "bitmaps.rkt"
         "maze.rkt"
         "state.rkt")

(define cell-size 20)

(define ghost-flash-period 3)
(define ghost-flash-point 21)

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
  (send the-color-database 
        find-color
        (case obj
          ('wall "DodgerBlue")
          ('dot  "Ivory")
          ('powerpill "Wheat")
          ('bloke "Yellow")
          ('ghost1 "Orange")
          ('ghost2 "OrangeRed")
          ('ghost3 "LightPink")
          ('ghost4 "Aqua")
          ('scaredghost "DodgerBlue")
          ('scaredghostending "White")
          ('forcefield "Lime")
          (else "Black"))))

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
                       (cell-bitmap reveal 'gap 'gap 'gap 'gap)
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
           'opaque
           (cell-colour 'bloke)))

(define (render-ghost x y id mode flee-frames-left dc)
  (send dc draw-bitmap
           (if (eq? mode 'fleeing) scared-ghost ghost)
           (* x cell-size)
           (* y cell-size)
           'opaque
           (cond ((eq? mode 'fleeing) 
                  (if (and (< flee-frames-left ghost-flash-point) 
                           (zero? (remainder flee-frames-left ghost-flash-period)))
                      (cell-colour 'scaredghostending)
                      (cell-colour 'scaredghost)))
                 (else (cell-colour (case (remainder id 4)
                                      ((1) 'ghost1)
                                      ((2) 'ghost2)
                                      ((3) 'ghost3)
                                      ((0) 'ghost4)))))))

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
                         (cell-colour 'bloke))
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
