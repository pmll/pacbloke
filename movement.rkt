#lang racket

;;; deal with objects that roam the maze...

(provide make-player
         make-ghosts
         player-movement
         ghost-movement
         scared-ghost-frames
         player-caught?
         ghosts-caught
         back-to-base
         (struct-out roamer)
         (struct-out ghost))

(require "maze.rkt")

(define player-speed 5)

(define ghost-speed 5)

(define scared-ghost-speed 6)

(define min-collision-area (/ 1 10))

(define scared-ghost-frames 100)

(define (clock-number a b)
  (cond ((< a 0) (+ a b))
        ((>= a b) (- a b))
        (else a)))

;; here speed denotes which fraction of unit is travelled in each frame
;; so the higher the value, the slower the travel
;; that way, the object in question is guaranteed to exactly align with a cell
;; at some point as it travels through it
(define-struct roamer (x y speed direction next-direction blocked-by))

(define-struct ghost (roamer id mode flee-frame home-x home-y next-speed))

(define (make-player maze)
  (let ((pos (vector-memq 'player (maze-map maze))))
    (make-roamer (if pos (remainder pos (maze-width maze)) 1)
                 (if pos (quotient pos (maze-width maze)) 1)
                 player-speed
                 #f
                 #f
                 '(wall forcefield))))

(define (make-ghosts maze)
  (let loop ((x 0) (y 0) (id 1))
    (cond ((= y (maze-height maze)) '())
          ((= x (maze-width maze)) (loop 0 (+ y 1) id))
          ((eq? (maze-cell maze x y) 'ghost)
           (cons (make-ghost (make-roamer x y ghost-speed #f #f '(wall))
                             id
                             'chasing
                             0
                             x
                             y
                             #f)
                 (loop (+ x 1) y (+ id 1))))
          (else (loop (+ x 1) y id)))))


(define (next-position maze x y direction speed)
  (cond ((eq? direction 'up) 
         (values x (clock-number (- y (/ 1 speed)) (maze-height maze))))
        ((eq? direction 'down)
         (values x (clock-number (+ y (/ 1 speed)) (maze-height maze))))
        ((eq? direction 'left)
         (values (clock-number (- x (/ 1 speed)) (maze-width maze)) y))
        ((eq? direction 'right)
         (values (clock-number (+ x (/ 1 speed)) (maze-width maze)) y))
        (else (values x y))))
  
(define (can-enter-cell? maze x y blocked-by)
  (not (memq (maze-cell maze x y) blocked-by)))

(define (can-occupy-coordinates? maze x y blocked-by)
  (let ((floor-x (clock-number (floor x) (maze-width maze)))
        (floor-y (clock-number (floor y) (maze-height maze)))
        (ceil-x  (clock-number (ceiling x) (maze-width maze)))
        (ceil-y  (clock-number (ceiling y) (maze-height maze))))
    (and (can-enter-cell? maze floor-x floor-y blocked-by)
         (can-enter-cell? maze floor-x ceil-y blocked-by)
         (can-enter-cell? maze ceil-x floor-y blocked-by)
         (can-enter-cell? maze ceil-x ceil-y blocked-by))))

;; change of direction does not happen right away, it may never happen.
;; it is queued up as the next direction to take and it will happen if and when
;; it is possible (and of course, we might have a change of heart before then).
(define (change-direction r direction)
  (struct-copy roamer r (next-direction direction)))

(define (change-speed r new-speed)
  (struct-copy roamer r (speed new-speed)))

(define (navigate maze r)
  (let ((x (roamer-x r))
        (y (roamer-y r))
        (speed (roamer-speed r))
        (next-direction (roamer-next-direction r))
        (blocked-by (roamer-blocked-by r)))
    (if next-direction
        (let-values (((next-x next-y)
                      (next-position maze x y next-direction speed)))
          (if (can-occupy-coordinates? maze next-x next-y blocked-by)
              (make-roamer x y speed next-direction #f blocked-by)
              r))
        r)))

(define (continue-motion maze r)
  (let ((x (roamer-x r))
        (y (roamer-y r))
        (direction (roamer-direction r))
        (speed (roamer-speed r))
        (next-direction (roamer-next-direction r))
        (blocked-by (roamer-blocked-by r)))
    (let-values (((next-x next-y) (next-position maze x y direction speed)))
      (if (can-occupy-coordinates? maze next-x next-y blocked-by)
          (make-roamer next-x next-y speed direction next-direction blocked-by)
          r))))

(define (realise-motion maze r)
  (continue-motion maze (navigate maze r)))

(define (player-movement player direction-request maze)
  (realise-motion maze
                  (if direction-request 
                      (change-direction player direction-request)
                      player)))


;; random ghost movement for now...
(define (ghost-movement ghost-lst maze last-meal frame-number)
  (define (become-scared g)
    (struct-copy ghost g (flee-frame frame-number)
                         (mode 'fleeing)
                         (next-speed scared-ghost-speed)))
  (define (become-bold g)
    (struct-copy ghost g (mode 'chasing) (next-speed ghost-speed)))
  (define (transform-ghost g)
    (cond ((eq? last-meal 'powerpill) (become-scared g))
          ((and (eq? (ghost-mode g) 'fleeing)
                (>= (- frame-number (ghost-flee-frame g)) scared-ghost-frames))
           (become-bold g))
          (else g)))
  (define (move-ghost g)
    (let ((direction-request (if (or (eq? (roamer-direction (ghost-roamer g)) 'up)
                                     (eq? (roamer-direction (ghost-roamer g)) 'down))
                                 (case (random 20)
                                   ((0) 'left)
                                   ((1) 'right)
                                   (else #f))
                                 (case (random 20)
                                   ((0) 'up)
                                   ((1) 'down)
                                   (else #f)))))
      (struct-copy ghost g
        (roamer
          (realise-motion maze
                          (if direction-request
                              (change-direction (ghost-roamer g) 
                                                direction-request)
                              (ghost-roamer g)))))))
  (map (lambda (g) (move-ghost (transform-ghost g))) ghost-lst))



;; provide a list of of ghost refs that have collided with our player
(define (collisions player-x player-y ghost-lst mode)
  (define (overlap x1 x2)
    (let ((xdiff (abs (- x1 x2))))
      (if (< xdiff 1) (- 1 xdiff) 0)))
  (define (collision-area ghost-x ghost-y)
    (* (overlap player-x ghost-x) (overlap player-y ghost-y)))
  (define (collision? g)
    (and (eq? (ghost-mode g) mode)
         (>= (collision-area (roamer-x (ghost-roamer g))
                             (roamer-y (ghost-roamer g)))
             min-collision-area)))
  (let ghost-ids ((ghost-lst ghost-lst))
    (cond ((null? ghost-lst) '())
          ((collision? (car ghost-lst))
           (cons (ghost-id (car ghost-lst)) (ghost-ids (cdr ghost-lst))))
          (else (ghost-ids (cdr ghost-lst))))))

(define (player-caught? player ghost-lst)
  (not (null? (collisions (roamer-x player)
                          (roamer-y player)
                          ghost-lst
                          'chasing))))

(define (ghosts-caught player ghost-lst)
  (collisions (roamer-x player) (roamer-y player) ghost-lst 'fleeing))

;; this proc is a temporary measure
;; it is here to set ghost structs to their base location
;; once we have maze graphs in place, we can have another ghost mode for the
;; um... ghosts of ghosts and they can make their way back to base via the
;; shortest route
(define (back-to-base g)
  (struct-copy ghost g (mode 'chasing)
                       (next-speed #f)
                       (roamer (make-roamer (ghost-home-x g)
                                            (ghost-home-y g)
                                            ghost-speed
                                            #f
                                            #f
                                            '(wall)))))

