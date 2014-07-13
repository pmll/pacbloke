#lang racket

;; maze data structure

(require "common.rkt")

(provide make-maze
         maze-width
         maze-height
         maze-cell
         navcell
         maze-member
         maze-count
         maze->2dvect
         cell-to-node-shortest
         longest-shortway
         ghost-home-nodes
         display-shortest-dists
         (struct-out node)
         (struct-out fingerpost)
         (struct-out shortway))

(define-struct node (id x y))

(define-struct fingerpost (node direction distance))

(define-struct shortcut (distance via))

(define-struct shortway (direction distance))

(define (longest-shortway sw-lst)
  (foldl (lambda (sw1 sw2) (if (fnof > shortway-distance sw1 sw2) sw1 sw2))
         (make-shortway 'none 0)
         sw-lst))

(define unassigned-shortcut (make-shortcut infinity #f))

(define null-shortcut (make-shortcut 0 #f))

(struct maze (width height grid nodes navgrid shortest-dists))

(define list->immutable-vector (compose vector->immutable-vector list->vector))

(define (char->maze-cell chr)
  (case chr
    ((#\@) 'wall)
    ((#\.) 'dot)
    ((#\o) 'powerpill)
    ((#\P) 'player)
    ((#\G) 'ghost)
    ((#\_) 'forcefield)
    ((#\~) 'void)
    (else 'gap)))

(define (string->maze-cells str) (map char->maze-cell (string->list str)))

(define (list->maze-cells lst) (flatten (map string->maze-cells lst)))

(define (coords->node nodes x y)
  (cond ((null? nodes) #f)
        ((and (= (node-x (car nodes)) x) (= (node-y (car nodes)) y)) (car nodes))
        (else (coords->node (cdr nodes) x y))))

;; the lst argument defines the maze layout. It is a list of rows of strings -
;; this allows us to design maze layouts in the code.
(define (make-maze width height lst)
  (define grid (list->immutable-2dvect width height (list->maze-cells lst)))
  (define (path? x y)
    (define cell-value (2dvect-ref grid (modulo x width) (modulo y height)))
    (and (not (eq? cell-value 'wall)) (not (eq? cell-value 'void))))
  (define (ghost? x y) (eq? (2dvect-ref grid x y) 'ghost))
  (define (exits x y)
    (+ (if (path? (- x 1) y) 1 0)
       (if (path? (+ x 1)  y) 1 0)
       (if (path? x (- y 1)) 1 0)
       (if (path? x (+ y 1)) 1 0)))
  ;; for our purposes, a node is any path cell that does not have two exits
  (define (node? x y) (or (not (= (exits x y) 2)) (ghost? x y)))
  (define nodes
    (let loop ((x 0) (y 0) (id 0))
      (cond ((= y height) '())
            ((= x width) (loop 0 (+ y 1) id))
            ((or (not (path? x y)) (not (node? x y))) (loop (+ x 1) y id))
            (else (cons (make-node id x y) (loop (+ x 1) y (+ id 1)))))))
  (define number-of-nodes (length nodes))
  (define (direct-node prev-x prev-y x y)
    (let loop ((prev-x prev-x) (prev-y prev-y) (x x) (y y) (dist 1))
      (define new-dist (+ dist 1))
      (define xy-node (coords->node nodes x y))
      (define (path-continues? x y)
        (and (not (and (= x prev-x) (= y prev-y))) (path? x y)))
      (define (adj-x v) (modulo (+ x v) width))
      (define (adj-y v) (modulo (+ y v) height))
      (cond (xy-node (values xy-node dist))
            ((path-continues? (adj-x 1) y) (loop x y (adj-x 1) y new-dist))
            ((path-continues? (adj-x -1) y) (loop x y (adj-x -1) y new-dist))
            ((path-continues? x (adj-y -1)) (loop x y x (adj-y -1) new-dist))
            ((path-continues? x (adj-y 1)) (loop x y x (adj-y 1) new-dist))
            ;; this should never happen if we have managed to find all
            ;; the nodes correctly, but just in case...
            (else (values 0 new-dist)))))
  (define (make-navcell-lst x y)
    (define (make-fingerpost-lst)
      (let loop ((offset-lst '((right 1 0) (left -1 0) (up 0 -1) (down 0 1))))
        (if (null? offset-lst)
            '()
            (let* ((offset (car offset-lst))
                   (direction (car offset))
                   (path-x (+ x (cadr offset)))
                   (path-y (+ y (caddr offset))))
              (if (path? path-x path-y)
                  (let-values (((node dist) (direct-node x y path-x path-y)))
                    (cons (make-fingerpost node direction dist)
                          (loop (cdr offset-lst))))
                  (loop (cdr offset-lst)))))))
    (cond ((= y height) '())
          ((= x width) (make-navcell-lst 0 (+ y 1)))
          ((not (path? x y)) (cons '() (make-navcell-lst (+ x 1) y)))
          (else (cons (make-fingerpost-lst) (make-navcell-lst (+ x 1) y)))))
  (define navgrid (list->immutable-2dvect width height (make-navcell-lst 0 0)))
  ;; Floyd Warshall algorithm used to calculate shortest distances between
  ;; nodes. Expressed functionally but with memoisation for performance
  (define memo-store (make-vector (* number-of-nodes (sqr number-of-nodes)) #f))
  (define (memo-value node1 node2 via-nodes)
    (vector-ref memo-store (+ (node-id node1)
                              (* (node-id node2) number-of-nodes)
                              (* (node-id (car via-nodes)) (sqr number-of-nodes)))))
  (define (memoise! node1 node2 via-nodes v)
    (vector-set! memo-store (+ (node-id node1)
                               (* (node-id node2) number-of-nodes)
                               (* (node-id (car via-nodes)) (sqr number-of-nodes))) v))
  (define (min-fingerpost fp-lst)
    (foldl (lambda (fp1 fp2) (if (fnof < fingerpost-distance fp1 fp2) fp1 fp2))
           (make-fingerpost #f 'none infinity)
           fp-lst))
  (define (min-shortcut sc1 sc2)
    (if (fnof < shortcut-distance sc1 sc2) sc1 sc2))
  (define (weight node1 node2)
    (define fps-from-node1 (2dvect-ref navgrid (node-x node1) (node-y node1)))
    (define fps-to-node2 (filter (lambda (fp) (eq? (fingerpost-node fp) node2))
                                 fps-from-node1))
    (define fp (min-fingerpost fps-to-node2))
    (cond ((eq? node1 node2) null-shortcut)
          ((eq? (fingerpost-distance fp) infinity) unassigned-shortcut)
          (else (make-shortcut (fingerpost-distance fp) node1))))
  (define (combine-shortcuts sc1 sc2)
    (make-shortcut (fnof + shortcut-distance sc1 sc2)
                   (if (zero? (shortcut-distance sc2))
                       (shortcut-via sc1)
                       (shortcut-via sc2))))
  (define (shortest node1 node2 via-nodes)
    (cond ((null? via-nodes) (weight node1 node2))
          ((memo-value node1 node2 via-nodes) (memo-value node1 node2 via-nodes))
          (else
            (memoise! node1
                      node2
                      via-nodes
                      (min-shortcut (shortest node1 node2 (cdr via-nodes))
                                    (combine-shortcuts (shortest node1
                                                                 (car via-nodes)
                                                                 (cdr via-nodes))
                                                       (shortest (car via-nodes)
                                                                 node2
                                                                 (cdr via-nodes)))))
            (memo-value node1 node2 via-nodes))))
  (define (shortest-alternatives node1 node2)
    (define fps-from-node2 (2dvect-ref navgrid (node-x node2) (node-y node2)))
    (if (eq? node1 node2)
        (list null-shortcut unassigned-shortcut)
        (sort (cons unassigned-shortcut
                    (map (lambda (fp)
                           (combine-shortcuts (shortest node1 (fingerpost-node fp) nodes)
                                              (weight (fingerpost-node fp) node2)))
                         fps-from-node2))
              (lambda (sc1 sc2) (fnof < shortcut-distance sc1 sc2)))))
  (define shortest-dists
    (let loop ((n1 nodes) (n2 nodes))
      (cond ((null? n2) '())
            ((null? n1) (loop nodes (cdr n2)))
            (else (cons (shortest-alternatives (car n1) (car n2))
                        (loop (cdr n1) n2))))))
  (maze width height grid nodes navgrid (list->immutable-2dvect number-of-nodes
                                                                number-of-nodes
                                                                shortest-dists)))

(define (maze-cell maze x y)
  (if (and (>= x 0)
           (< x (maze-width maze))
           (>= y 0)
           (< y (maze-height maze)))
      (2dvect-ref (maze-grid maze) x y)
      'gap))

(define (maze-member maze item) (2dvect-memq item (maze-grid maze)))

(define (maze-count maze item) (2dvect-count item (maze-grid maze)))

(define (maze->2dvect maze) (2dvect-copy (maze-grid maze)))

(define (navcell maze x y) (2dvect-ref (maze-navgrid maze) x y))

(define (node-to-node-shortest maze node1 node2 not-via)
  (define shortest-lst (2dvect-ref (maze-shortest-dists maze)
                                   (node-id node1)
                                   (node-id node2)))
  (cond ((eq? (shortcut-via (car shortest-lst)) not-via)
         (shortcut-distance (cadr shortest-lst)))
        (else (shortcut-distance (car shortest-lst)))))

(define (cell-to-node-shortest maze x y node not-via not-direction)
  (let loop ((fp-lst (navcell maze x y)) (shortest-way (make-shortway 'none infinity)))
    (cond ((null? fp-lst) shortest-way)
          (else 
            (define fp (car fp-lst))
            (define dist (+ (fingerpost-distance fp)
                            (node-to-node-shortest maze
                                                   (fingerpost-node fp)
                                                   node
                                                   not-via)))
            (define direction (fingerpost-direction fp))
            (loop (cdr fp-lst)
                  (if (or (eq? direction not-direction)
                          (>= dist (shortway-distance shortest-way)))
                      shortest-way
                      (make-shortway direction dist)))))))

(define (ghost-home-nodes maze)
  (filter (lambda (n) (eq? (maze-cell maze (node-x n) (node-y n)) 'ghost))
          (maze-nodes maze)))

(define (display-shortest-dists maze)
  (define number-of-nodes (car (maze-shortest-dists maze)))
  (define dist-vector (maze-shortest-dists maze))
  (let loop ((n1 0) (n2 0))
    (cond ((= n2 number-of-nodes) (newline))
          ((= n1 number-of-nodes) (loop 0 (+ n2 1)))
          (else
            (define scl (2dvect-ref dist-vector n1 n2))
            (printf "(~a,~a) ~a:~a ~a:~a~n"
                    n1
                    n2
                    (shortcut-distance (car scl))
                    (shortcut-via (car scl))
                    (shortcut-distance (cadr scl))
                    (shortcut-via (cadr scl)))
            (loop (+ n1 1) n2)))))

