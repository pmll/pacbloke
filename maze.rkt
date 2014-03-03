#lang racket

;; maze data structure

(require racket/gui/base)

(provide make-maze
         maze-width
         maze-height
         maze-map
         maze-cell)

(define (char->maze-cell chr)
  (case chr
    ((#\@) 'wall)
    ((#\space) 'gap)
    ((#\.) 'dot)
    ((#\o) 'powerpill)
    ((#\P) 'player)
    ((#\G) 'ghost)
    ((#\_) 'forcefield)
    (else 'gap)))

;; the lst argument defines the maze layout. It is a list of rows of strings -
;; this allows us to design maze layouts in the code.
(define (make-maze width height lst)
  (let ((maze-vector (make-vector (* width height))))
    (define (set-cell! row-no col-no cell-value)
      (vector-set! maze-vector (+ (* row-no width) col-no) cell-value))
    (define (set-row! row-no row-str)
      (let loop ((col-no 0) (row-lst lst))
        (when (< col-no width)
          (set-cell! row-no col-no (char->maze-cell (string-ref row-str col-no)))
          (loop (+ col-no 1) (cdr row-lst)))))
    (define (set-maze! maze-lst)
      (let loop ((row-no 0) (maze-lst lst))
        (when (< row-no height)
          (set-row! row-no (car maze-lst))
          (loop (+ row-no 1) (cdr maze-lst)))))
    (set-maze! lst)
    (list width height maze-vector)))

(define (maze-width maze) (car maze))

(define (maze-height maze) (cadr maze))

(define (maze-map maze) (caddr maze))

(define (maze-cell maze x y)
  (if (and (>= x 0)
           (< x (maze-width maze))
           (>= y 0)
           (< y (maze-height maze)))
      (vector-ref (maze-map maze) (+ (* y (maze-width maze)) x))
      'gap))
