#lang racket

;; maze data structure

(provide infinity
         make-maze
         maze-width
         maze-height
         maze-cell
         navcell
         maze-member
         maze-count
         maze->vector
         shortest
         (struct-out node)
         (struct-out fingerpost)
         (struct-out shortcut))

(define infinity 999999) ;-)

(define-struct node (id x y))

(define-struct fingerpost (node direction distance))

(define-struct shortcut (direction distance))

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
  (define grid (list->immutable-vector (list->maze-cells lst)))
  (define (path? x y)
    (define cell-value (vector-ref grid 
                                   (+ (modulo x width)
                                      (* (modulo y height) width))))
    (and (not (eq? cell-value 'wall)) (not (eq? cell-value 'void))))
  (define (ways-out x y)
    (+ (if (path? (- x 1) y) 1 0)
       (if (path? (+ x 1)  y) 1 0)
       (if (path? x (- y 1)) 1 0)
       (if (path? x (+ y 1)) 1 0)))
  ;; for our purposes, a node is any path cell that does not have two ways out
  (define (node? x y) (not (= (ways-out x y) 2)))
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
      (if xy-node
          (values xy-node dist)
          (cond ((path-continues? (adj-x 1) y) (loop x y (adj-x 1) y new-dist))
                ((path-continues? (adj-x -1) y) (loop x y (adj-x -1) y new-dist))
                ((path-continues? x (adj-y -1)) (loop x y x (adj-y -1) new-dist))
                ((path-continues? x (adj-y 1)) (loop x y x (adj-y 1) new-dist))
                ;; this should never happen if we have managed to find all
                ;; the nodes correctly, but just in case...
                (else (values 0 new-dist))))))
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
  (define navgrid (list->immutable-vector (make-navcell-lst 0 0)))
  ;; Floyd Warshall algo to get grid of shortest dists between nodes
  (define dist-vector (make-vector (sqr number-of-nodes)
                                   (make-shortcut 'none infinity)))
  (define (set-dist! node1 node2 direction distance)
    (vector-set! dist-vector (+ (node-id node1)
                                (* (node-id node2) number-of-nodes))
                 (make-shortcut direction distance)))
  (define (dist node1 node2)
    (shortcut-distance
     (vector-ref dist-vector (+ (node-id node1)
                                (* (node-id node2) number-of-nodes)))))
  (define (dir node1 node2)
    (shortcut-direction
     (vector-ref dist-vector (+ (node-id node1)
                                (* (node-id node2) number-of-nodes)))))
  (define (set-direct-node-dists! n)
    (set-dist! n n 'none 0)
    (for-each (lambda (fp) (set-dist! n
                                      (fingerpost-node fp)
                                      (fingerpost-direction fp)
                                      (fingerpost-distance fp)))
              (vector-ref navgrid (+ (node-x n) (* (node-y n) width)))))
  (for-each set-direct-node-dists! nodes)
  (define shortest-dists
    (let loop ((k nodes) (i nodes) (j nodes))
      (cond ((null? j) (loop k (cdr i) nodes))
            ((null? i) (loop (cdr k) nodes nodes))
            ((null? k) (cons number-of-nodes
                             (vector->immutable-vector dist-vector)))
            (else
             (let ((ni (car i))
                   (nj (car j))
                   (nk (car k)))
               (when (> (dist ni nj) (+ (dist ni nk) (dist nk nj)))
                 (set-dist! ni nj (dir ni nk) (+ (dist ni nk) (dist nk nj))))
               (loop k i (cdr j)))))))
  (maze width height grid nodes navgrid shortest-dists))

(define (maze-cell maze x y)
  (if (and (>= x 0)
           (< x (maze-width maze))
           (>= y 0)
           (< y (maze-height maze)))
      (vector-ref (maze-grid maze) (+ (* y (maze-width maze)) x))
      'gap))

(define (maze-member maze item)
  (let ((pos (vector-memq item (maze-grid maze))))
    (if pos 
       (values (remainder pos (maze-width maze))
               (quotient pos (maze-width maze)))
       (values #f #f))))

(define (maze-count maze item)
  (vector-count (lambda (v) (eq? v item)) (maze-grid maze)))

(define (maze->vector maze) (vector-copy (maze-grid maze)))

(define (navcell maze x y)
  (vector-ref (maze-navgrid maze) (+ x (* y (maze-width maze)))))

(define (shortest maze node1 node2)
  (define number-of-nodes (car (maze-shortest-dists maze)))
  (define dist-vector (cdr (maze-shortest-dists maze)))
  (vector-ref dist-vector (+ (node-id node1)
                             (* (node-id node2) number-of-nodes))))
