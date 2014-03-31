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
         change-direction
         (struct-out roamer)
         (struct-out ghost)
         (struct-out target))
 
(require "maze.rkt")

(define player-speed 5)

(define ghost-speed 5)

(define scared-ghost-speed 10)

(define min-collision-area (/ 1 10))

(define scared-ghost-frames 100)

;; a bit like (modulo a b) except that a doesn't have to be whole
(define (clock-number a b)
  (cond ((< a 0) (+ a b))
        ((>= a b) (- a b))
        (else a)))

;; here speed denotes which fraction of unit is travelled in each frame
;; so the higher the value, the slower the travel
;; that way, the object in question is guaranteed to exactly align with a cell
;; at some point as it travels through it
(define-struct roamer (x y speed direction next-direction blocked-by next-speed))

(define-struct target (node not-via))

(define (target-eq? t1 t2)
  (and (eq? (target-node t1) (target-node t2))
       (eq? (target-not-via t1) (target-not-via t2))))

(define-struct ghost (roamer id mode flee-frame home-x home-y target))

(define (make-player maze)
  (let-values (((x y) (maze-member maze 'player)))
    (make-roamer (if x x 1)
                 (if y y 1)
                 player-speed 
                 #f 
                 #f 
                 '(wall forcefield) 
                 #f)))
          
(define (make-ghosts maze)
  (let loop ((x 0) (y 0) (id 1))
    (cond ((= y (maze-height maze)) '())
          ((= x (maze-width maze)) (loop 0 (+ y 1) id))
          ((eq? (maze-cell maze x y) 'ghost)
           (cons (make-ghost (make-roamer x y ghost-speed #f #f '(wall) #f)
                             id
                             'chasing
                             0
                             x
                             y
                             (make-target (make-node 0 0 0) (make-node 0 0 0)))
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
  (struct-copy roamer r (next-speed new-speed)))

(define (realise-direction maze r)
  (let ((next-direction (roamer-next-direction r)))
    (if next-direction
        (let-values (((next-x next-y)
                      (next-position maze
                                     (roamer-x r)
                                     (roamer-y r) 
                                     next-direction 
                                     (roamer-speed r))))
          (if (can-occupy-coordinates? maze next-x next-y (roamer-blocked-by r))
              (struct-copy roamer r (direction next-direction))
              r))
        r)))

(define (continue-motion maze r)
  (let ((next-speed (roamer-next-speed r)))
    (let-values (((next-x next-y) (next-position maze
                                                 (roamer-x r)
                                                 (roamer-y r)
                                                 (roamer-direction r)
                                                 (roamer-speed r))))
      (if (can-occupy-coordinates? maze next-x next-y (roamer-blocked-by r))
          (struct-copy roamer r (x next-x)
                                (y next-y)
                                (speed (if (and next-speed
                                                (integer? next-x)
                                                (integer? next-y))
                                           next-speed
                                           (roamer-speed r))))
          r))))

(define (realise-motion maze r)
  (continue-motion maze (realise-direction maze r)))

(define (player-movement player direction-request maze)
  (realise-motion maze
                  (if direction-request 
                      (change-direction player direction-request)
                      player)))

(define (ghost-movement ghost-lst maze last-meal frame-number player-x player-y)
  (define (become-scared g)
    (struct-copy ghost g (flee-frame frame-number)
                         (mode 'fleeing)
                         (roamer (change-speed (ghost-roamer g) scared-ghost-speed))))
  (define (become-bold g)
    (struct-copy ghost g (mode 'chasing)
                         (roamer (change-speed (ghost-roamer g) ghost-speed))))
  (define (transform-ghost g)
    (cond ((eq? last-meal 'powerpill) (become-scared g))
          ((and (eq? (ghost-mode g) 'fleeing)
                (>= (- frame-number (ghost-flee-frame g)) scared-ghost-frames))
           (become-bold g))
          (else g)))
  (define (move-ghost g)
    (struct-copy ghost g (roamer (realise-motion maze (ghost-roamer g)))))
  (map (lambda (g) (move-ghost (follow-mission (transform-ghost g) maze)))
       (assign-missions player-x player-y ghost-lst maze)))

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
  (map ghost-id (filter collision? ghost-lst)))

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
                       (roamer (make-roamer (ghost-home-x g)
                                            (ghost-home-y g)
                                            ghost-speed
                                            #f
                                            #f
                                            '(wall)
                                            #f))))

;;;; ghost strategy 
;; fixme: it it not necessary to re-evalue this every frame as the player
;; will only move a fraction of a cell
;; we can save gc if we tune it to the player's speed

(define (snap-coord n) (floor (+ n (/ 1 2))))

(define (opposite-direction? dir1 dir2)
  (or (and (eq? dir1 'up) (eq? dir2 'down))
      (and (eq? dir1 'down) (eq? dir2 'up))
      (and (eq? dir1 'left) (eq? dir2 'right))
      (and (eq? dir1 'right) (eq? dir2 'left))))

(define (zip lsts)
  (define remaining-lsts (filter (lambda (lst) (not (null? lst))) lsts))
  (cond ((null? remaining-lsts) '())
        (else (append (map car remaining-lsts) (zip (map cdr remaining-lsts))))))

(define (ghost-shortcut g tgt maze)
  (define direction (roamer-direction (ghost-roamer g)))
  (define x (modulo ((cond ((eq? direction 'left) floor)
                           ((eq? direction 'right) ceiling)
                           (else snap-coord))
                     (roamer-x (ghost-roamer g)))
                    (maze-width maze)))
  (define y (modulo ((cond ((eq? direction 'up) floor)
                           ((eq? direction 'down) ceiling)
                           (else snap-coord))
                     (roamer-y (ghost-roamer g)))
                    (maze-height maze)))
  (let loop ((fp-lst (navcell maze x y)) (shortest-cut unassigned-shortcut))
    (cond ((null? fp-lst) shortest-cut)
          (else
            (define fp (car fp-lst))
            (define dist (+ (fingerpost-distance fp)
                            (shortcut-distance (shortest maze
                                                         (fingerpost-node fp)
                                                         (target-node tgt)
                                                         (target-not-via tgt)))))
            (if (and (< dist (shortcut-distance shortest-cut))
                     (not (opposite-direction? direction (fingerpost-direction fp))))
                (loop (cdr fp-lst) (make-shortcut (fingerpost-direction fp)
                                                  dist
                                                  #f))
                (loop (cdr fp-lst) shortest-cut))))))

(define (acquired-target? g)
  (define tgt-node (if (ghost-target g) (target-node (ghost-target g)) #f))
  (define roamer (ghost-roamer g))
  (and tgt-node
       (eq? (roamer-x roamer) (node-x tgt-node))
       (eq? (roamer-y roamer) (node-y tgt-node))))

(define (direction-from-node from-node to-node maze)
  (let loop ((fp-lst (navcell maze (node-x from-node) (node-y from-node))))
    (cond ((null? fp-lst) 'none)
          ((eq? (fingerpost-node (car fp-lst)) to-node) (fingerpost-direction (car fp-lst)))
          (else (loop (cdr fp-lst))))))

(define (follow-mission g maze)
  (define tgt (ghost-target g))
  (struct-copy
    ghost g 
    (roamer (change-direction (ghost-roamer g)
                              (if (acquired-target? g)
                                  (direction-from-node (target-node tgt)
                                                       (target-not-via tgt)
                                                       maze)
                                  (shortcut-direction (ghost-shortcut g 
                                                                      (ghost-target g) 
                                                                      maze)))))))

(define (assign-missions player-x player-y ghost-lst maze)
  (define (node-targets x y not-via excl-targets)
    (filter (lambda (tgt) (not (ormap (lambda (e) (target-eq? e tgt))
                                      excl-targets)))
            (if not-via
                (map (lambda (i) (make-target (fingerpost-node i) not-via))
                     (navcell maze x y))
                (map (lambda (i nv) (make-target (fingerpost-node i) (fingerpost-node nv)))
                     (navcell maze x y)
                     (reverse (navcell maze x y))))))
  (define (node-of-node-targets node-lst)
    (zip (map (lambda (n) (node-targets (node-x (target-node n)) (node-y (target-node n)) (target-node n) node-lst)) node-lst)))
  ;; immediate nodes reachable by the player and the nodes reachable from 
  ;; those nodes should be enough unless we decide to start having large
  ;; numbers of ghosts, in which case, some might be left without a mission.
  (define primary-targets (node-targets (snap-coord player-x)
                                        (snap-coord player-y)
                                        #f
                                        '()))
  (define secondary-targets (node-of-node-targets primary-targets)) 
  (define target-nodes (append primary-targets secondary-targets))
  (define (closest-ghost ghost-lst tgt)
    (let loop ((ghost-lst ghost-lst)
               (cghost #f)
               (shortest-cut unassigned-shortcut))
      (cond ((null? ghost-lst)
             (if cghost (struct-copy ghost cghost (target tgt)) #f))
            (else
              (let ((ghost-sc (ghost-shortcut (car ghost-lst) tgt maze)))
                (if (< (shortcut-distance ghost-sc)
                       (shortcut-distance shortest-cut))
                    (loop (cdr ghost-lst) (car ghost-lst) ghost-sc)
                    (loop (cdr ghost-lst) cghost shortest-cut)))))))
  (define (ghost-eq? ghost1 ghost2)
    (= (ghost-id ghost1) (ghost-id ghost2)))
  (define (targets-not-assigned target-nodes ghost-lst)
    (define (target-not-assigned tgt)
      (not (ormap (lambda (g) (target-eq? (ghost-target g) tgt)) ghost-lst)))
    (filter target-not-assigned target-nodes))
  (define (ghosts-filter assigned ghost-lst target-nodes)
    (define (ghost-assigned g)
      (ormap (lambda (tgt) (target-eq? (ghost-target g) tgt)) target-nodes))
    (filter (lambda (g) (if assigned (ghost-assigned g) (not (ghost-assigned g))))
            ghost-lst))
  (let assign-targets ((targets (targets-not-assigned target-nodes ghost-lst))
                       (assigned-ghosts (ghosts-filter #t ghost-lst target-nodes))
                       (unassigned-ghosts (ghosts-filter #f ghost-lst target-nodes)))
    (cond ((null? targets) (append assigned-ghosts unassigned-ghosts))
          ((null? unassigned-ghosts) assigned-ghosts)
          (else
            (define chosen (closest-ghost unassigned-ghosts (car targets)))
            (assign-targets (cdr targets)
                            (if chosen 
                                (cons chosen assigned-ghosts)
                                assigned-ghosts)
                            (filter (lambda (g) (or (not chosen)
                                                    (not (ghost-eq? chosen g))))
                                    unassigned-ghosts))))))

