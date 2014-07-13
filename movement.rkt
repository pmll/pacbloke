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
         (struct-out ghost)
         (struct-out target))
 
(require "maze.rkt"
         "common.rkt")

(define null-node (make-node 0 0 0))

(define player-speed 5)

(define ghost-speed 5)

(define scared-ghost-speed 10)

(define returning-ghost-speed 2)

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
  (and (fnof eq? target-node t1 t2) (fnof eq? target-not-via t1 t2)))

(define-struct ghost (roamer id mode flee-frame home-node target))

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
  (map (lambda (n) (make-ghost (make-roamer (node-x n) (node-y n) ghost-speed #f #f '(wall) #f)
                               (node-id n)
                               'chasing
                               0
                               n
                               (make-target (make-node 0 0 0) (make-node 0 0 0))))
       (ghost-home-nodes maze)))

(define (cell-aligned? x y) (and (integer? x) (integer? y)))

(define (opposite-direction direction)
  (case direction
    ('up 'down)
    ('down 'up)
    ('left 'right)
    ('right 'left)
    (else #f)))

(define (opposite-direction? dir1 dir2)
  (eq? (opposite-direction dir1) dir2))

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

;; in order to ensure that the roamer always aligns with a cell as it passes
;; through, speed changes must also be queued and applied when convenient
(define (change-speed r new-speed)
  (struct-copy roamer r (next-speed new-speed)))

(define (realise-direction maze r)
  (let ((next-direction (roamer-next-direction r)))
    (if (and next-direction (cell-aligned? (roamer-x r) (roamer-y r)))
        (let-values (((next-x next-y)
                      (next-position maze
                                     (roamer-x r)
                                     (roamer-y r) 
                                     next-direction 
                                     (roamer-speed r))))
          (if (can-occupy-coordinates? maze next-x next-y (roamer-blocked-by r))
              (struct-copy roamer r (direction next-direction)
                                    (next-direction #f))
              r))
        r)))

(define (realise-speed r)
  (if (and (roamer-next-speed r) (cell-aligned? (roamer-x r) (roamer-y r)))
      (struct-copy roamer r (speed (roamer-next-speed r))
                            (next-speed #f))
      r))

(define (realise-motion maze r)
  (define new-r (realise-speed (realise-direction maze r)))
  (let-values (((next-x next-y) (next-position maze
                                               (roamer-x new-r)
                                               (roamer-y new-r)
                                               (roamer-direction new-r)
                                               (roamer-speed new-r))))
    (if (can-occupy-coordinates? maze next-x next-y (roamer-blocked-by new-r))
        (struct-copy roamer new-r (x next-x) (y next-y))
        new-r)))

(define (player-movement player direction-request maze)
  (realise-motion maze (if direction-request 
                           (change-direction player direction-request)
                           player)))

(define (filter-ghosts-by-mode ghost-lst mode)
  (filter (lambda (g) (eq? (ghost-mode g) mode)) ghost-lst))

(define (fleeing-ghosts ghost-lst) (filter-ghosts-by-mode ghost-lst 'fleeing))

(define (chasing-ghosts ghost-lst) (filter-ghosts-by-mode ghost-lst 'chasing))

(define (returning-ghosts ghost-lst) (filter-ghosts-by-mode ghost-lst 'returning))

(define (ghost-movement ghost-lst maze last-meal frame-number player-x player-y)
  (define (ghost-home? g)
   (and (= (roamer-x (ghost-roamer g)) (node-x (ghost-home-node g)))
        (= (roamer-y (ghost-roamer g)) (node-y (ghost-home-node g)))))
  (define (become-scared g)
    (struct-copy ghost g (flee-frame frame-number)
                         (mode 'fleeing)
                         (target (make-target (make-node 0 0 0) (make-node 0 0 0)))
                         (roamer (change-speed (ghost-roamer g) scared-ghost-speed))))
  (define (become-bold g)
    (struct-copy ghost g (mode 'chasing)
                         (roamer (change-speed (ghost-roamer g) ghost-speed))))
  (define (transform-ghost g)
    (cond ((and (eq? last-meal 'powerpill)
                (not (eq? (ghost-mode g) 'returning)))
           (become-scared g))
          ((and (eq? (ghost-mode g) 'fleeing)
                (>= (- frame-number (ghost-flee-frame g)) scared-ghost-frames))
           (become-bold g))
          ((and (eq? (ghost-mode g) 'returning)
                (ghost-home? g))
           (become-bold g))
          (else g)))
  (define (gle v1 v2 v3 v4 v5)
    (and (> v2 v1) (< v2 v3) (= v4 v5)))
  (define (ghost-in-bubble? g1 g2)
    (define r1 (ghost-roamer g1))
    (define r2 (ghost-roamer g2))
    (define d1 (roamer-direction r1))
    (define d2 (roamer-direction r2))
    (define x1 (roamer-x r1))
    (define x2 (roamer-x r2))
    (define y1 (roamer-y r1))
    (define y2 (roamer-y r2))
    (and (not (opposite-direction? d1 d2))
         (fnof eq? ghost-mode g1 g2)
         (or (and (= x1 x2)
                  (= y1 y2)
                  (< (ghost-id g1) (ghost-id g2)))
             (and (eq? d1 'left) (gle (- x1 1) x2 x1 y1 y2))
             (and (eq? d1 'right) (gle x1 x2 (+ x1 1) y1 y2))
             (and (eq? d1 'up) (gle (- y1 1) y2 y1 x1 x2))
             (and (eq? d1 'down) (gle y1 y2 (+ y1 1) x1 x2)))))
  (define (right-of-way? g)
    (not (ormap (lambda (h) (ghost-in-bubble? g h)) ghost-lst)))
  (define (move-ghost g)
    (if (right-of-way? g)
        (struct-copy ghost g (roamer (realise-motion maze (ghost-roamer g))))
        g))
  (define transformed-ghosts (map transform-ghost ghost-lst))
  (map move-ghost
       (append
         (map (lambda (g) (follow-mission g maze))
              (assign-missions player-x player-y (chasing-ghosts transformed-ghosts) maze))
         (map (lambda (g) (run-like-the-wind player-x player-y g maze))
              (fleeing-ghosts transformed-ghosts))
         (map (lambda (g) (return-home g maze))
              (returning-ghosts transformed-ghosts)))))

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

(define (back-to-base g)
  (define (roamer-returning r)
    (struct-copy roamer r (speed returning-ghost-speed)
                          (next-speed #f)
                          (x (snap-coord (roamer-x r)))
                          (y (snap-coord (roamer-y r)))))
  (struct-copy ghost g (mode 'returning)
                       (roamer (roamer-returning (ghost-roamer g)))
                       (target (make-target (ghost-home-node g)
                                            (make-node 0 0 0)))))

;;;; ghost strategy 
;; fixme: it it not necessary to re-evalue this every frame as the player
;; will only move a fraction of a cell
;; we can save gc if we tune it to the player's speed

(define (snap-coord n) (floor (+ n (/ 1 2))))

(define (vantage-point r maze)
  (define direction (roamer-direction r))
  (values (modulo ((cond ((eq? direction 'left) floor)
                         ((eq? direction 'right) ceiling)
                         (else snap-coord))
                   (roamer-x r))
                  (maze-width maze))
          (modulo ((cond ((eq? direction 'up) floor)
                         ((eq? direction 'down) ceiling)
                         (else snap-coord))
                   (roamer-y r))
                  (maze-height maze))))
 
(define (zip lsts)
  (define remaining-lsts (filter (lambda (lst) (not (null? lst))) lsts))
  (cond ((null? remaining-lsts) '())
        (else (append (map car remaining-lsts) (zip (map cdr remaining-lsts))))))

(define (ghost-shortway g tgt maze)
  (define direction (roamer-direction (ghost-roamer g)))
  (let-values (((x y) (vantage-point (ghost-roamer g) maze)))
    (cell-to-node-shortest maze
                           x
                           y
                           (target-node tgt)
                           (target-not-via tgt)
                           (opposite-direction direction))))

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
                                  (shortway-direction (ghost-shortway g 
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
               (shortest-way (make-shortway 'none infinity)))
      (cond ((null? ghost-lst)
             (if cghost (struct-copy ghost cghost (target tgt)) #f))
            (else
              (let ((ghost-sw (ghost-shortway (car ghost-lst) tgt maze)))
                (if (fnof < shortway-distance ghost-sw shortest-way)
                    (loop (cdr ghost-lst) (car ghost-lst) ghost-sw)
                    (loop (cdr ghost-lst) cghost shortest-way)))))))
  (define (ghost-eq? ghost1 ghost2) (fnof = ghost-id ghost1 ghost2))
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

;; ghost fleeing strategy - short termist
;; just head for the direct node that is furthest away from the player
(define (run-like-the-wind player-x player-y g maze)
  (define px (modulo (snap-coord player-x) (maze-width maze)))
  (define py (modulo (snap-coord player-y) (maze-height maze)))
  (define (fingerpost-pair fp-lst)
    (if (= (length fp-lst) 2)
        (if (fnof < (compose node-id fingerpost-node) (car fp-lst) (cadr fp-lst))
            (cons (car fp-lst) (cadr fp-lst))
            (cons (cadr fp-lst) (car fp-lst)))
        #f))
  (let-values (((gx gy) (vantage-point (ghost-roamer g) maze)))
    (define p-fp-pair (fingerpost-pair (navcell maze px py)))
    (define g-fp-pair (fingerpost-pair (navcell maze gx gy)))
    ;; may get false positives here where two edges share same node pairing
    (define same-edge?
      (and p-fp-pair
           g-fp-pair
           (fnof eq? fingerpost-node (car p-fp-pair) (car g-fp-pair))
           (fnof eq? fingerpost-node (cdr p-fp-pair) (cdr g-fp-pair))))
    ; generally, about turns are frowned upon in the ghost community
    ; the exception to this rule is when a ghost finds that they are on the
    ; same edge as the player
    (define (dir-node-closer-to-ghost)
      (if (fnof <= fingerpost-distance (car g-fp-pair) (car p-fp-pair))
          (fingerpost-direction (car g-fp-pair))
          (fingerpost-direction (cdr g-fp-pair))))
    (define (fp-shortway fp)
      (make-shortway (fingerpost-direction fp)
                     (shortway-distance (cell-to-node-shortest maze
                                                               px
                                                               py
                                                               (fingerpost-node fp)
                                                               null-node 
                                                               #f))))
    (define opposite-dir
      (opposite-direction (roamer-direction (ghost-roamer g))))
    (define dir
      (if same-edge?
          (dir-node-closer-to-ghost)
          (shortway-direction
            (longest-shortway
              (map fp-shortway
                (filter (lambda (fp) (not (eq? (fingerpost-direction fp)
                                               opposite-dir)))
                        (navcell maze gx gy)))))))
    (struct-copy ghost g (roamer (change-direction (ghost-roamer g) dir)))))

; "dead" ghost return to its point of origin
(define (return-home g maze)
  (struct-copy
    ghost g 
    (roamer (change-direction (ghost-roamer g)
                              (shortway-direction (ghost-shortway g 
                                                                 (ghost-target g) 
                                                                 maze))))))

