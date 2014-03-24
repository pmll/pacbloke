#lang racket

;; create image bitmaps

(require racket/gui/base)

(provide loan-square
         top-terminator bottom-terminator left-terminator right-terminator
         horizontal vertical
         top-left-corner top-right-corner bottom-left-corner bottom-right-corner
         top-t bottom-t left-t right-t
         cross
         forcefield
         dot pill
         blank
         bloke-shut
         bloke-ajar-left bloke-ajar-right bloke-ajar-up bloke-ajar-down
         bloke-open-left bloke-open-right bloke-open-up bloke-open-down
         ghost scared-ghost
         score-200 score-400 score-800 score-1600)
          
(define (make-proto-bitmap width height lst)
  (define (make-row lst)
    (let ((len (length lst)))
      (cond ((< len width) (append lst (make-list (- width len) 0)))
            ((> len width) (take lst width))
            (else lst))))
  (define (make-map lst row-no )
    (cond ((= row-no height) '())
          ((null? lst) (cons (make-row '()) (make-map lst (+ row-no 1))))
          (else (cons (make-row (car lst)) (make-map (cdr lst) (+ row-no 1))))))
  (list width height (make-map lst 0)))

(define (proto-bitmap-width pbmp) (car pbmp))

(define (proto-bitmap-height pbmp) (cadr pbmp))

(define (proto-bitmap-map pbmp) (caddr pbmp))

(define (vertical-flip pbmp)
  (make-proto-bitmap (proto-bitmap-width pbmp)
                     (proto-bitmap-height pbmp)
                     (reverse (proto-bitmap-map pbmp))))

(define (horizontal-flip pbmp)
  (make-proto-bitmap (proto-bitmap-width pbmp)
                     (proto-bitmap-height pbmp)
                     (let loop ((rows (proto-bitmap-map pbmp)))
                       (if (null? rows)
                           '()
                           (cons (reverse (car rows)) (loop (cdr rows)))))))

(define (rotate pbmp)
  (define (args->reverse-list . lst) (reverse lst))
  (make-proto-bitmap (proto-bitmap-height pbmp)
                     (proto-bitmap-width pbmp)
                     (apply map args->reverse-list (proto-bitmap-map pbmp))))

(define (proto-bitmap->bitmap pbmp)
  (define (take-8 lst)
    (if (< (length lst) 8)
        lst
        (take lst 8)))
  (define (drop-8 lst)
    (if (< (length lst) 8)
        '()
        (drop lst 8)))
  (define (pixels->byte lst b v)
    (if (null? lst)
        (bytes b)
        (pixels->byte (cdr lst) (+ b (* v (car lst))) (* v 2))))
  (define (row->bytes lst b)
    (if (null? lst)
        b
        (row->bytes (drop-8 lst) (bytes-append b (pixels->byte (take-8 lst) 0 1)))))
  (define (map->bytes lst b)
    (if (null? lst)
        b
        (map->bytes (cdr lst) (bytes-append b (row->bytes (car lst) #"")))))
  (make-object bitmap%
    (map->bytes (proto-bitmap-map pbmp) #"")
    (proto-bitmap-width pbmp)
    (proto-bitmap-height pbmp)))

;; bitmaps for maze walls
;; ~~~~~~~~~~~~~~~~~~~~~~

;; No points of contact - loan square
(define p-loan-square
  (make-proto-bitmap 20
                     20
                     '(()
                       ()
                       ()
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1)
                       (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1))))

;; one point of contact - terminator
(define p-bottom-terminator
  (make-proto-bitmap 20
                     20
                     '((0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1)
                       (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1))))
                    
(define p-top-terminator (vertical-flip p-bottom-terminator))

(define p-left-terminator (rotate p-bottom-terminator))

(define p-right-terminator (horizontal-flip p-left-terminator))

;; two points of contact - straight through
(define p-horizontal
  (make-proto-bitmap 20
                     20
                     '(() () ()
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       () () () () () () () () () ()
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))

(define p-vertical (rotate p-horizontal))
                       
;; two points of contact - corner
(define p-bottom-right-corner
  (make-proto-bitmap 20
                     20
                     '((0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))

(define p-bottom-left-corner (horizontal-flip p-bottom-right-corner))

(define p-top-right-corner (vertical-flip p-bottom-right-corner))

(define p-top-left-corner (horizontal-flip p-top-right-corner))

;; three points of contact - T piece
(define p-bottom-t
  (make-proto-bitmap 20
                     20
                     '((0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1)
                       (1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
                       (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)
                       () () () () () () () () () ()
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))

(define p-top-t (vertical-flip p-bottom-t))

(define p-left-t (rotate p-bottom-t))

(define p-right-t (horizontal-flip p-left-t))

;; four points of contact - cross
(define p-cross
  (make-proto-bitmap 20
                     20
                     '((0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1)
                       (1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
                       (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)
                       () () () () () () () () () ()                       
                       (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)
                       (1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
                       (0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1)
                       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1))))

(define p-forcefield
  (make-proto-bitmap 20
                     20
                     '(() () () () () () () ()
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
                       (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))

(define loan-square (proto-bitmap->bitmap p-loan-square))
(define bottom-terminator (proto-bitmap->bitmap p-bottom-terminator))
(define top-terminator (proto-bitmap->bitmap p-top-terminator))
(define left-terminator (proto-bitmap->bitmap p-left-terminator))
(define right-terminator (proto-bitmap->bitmap p-right-terminator))
(define horizontal (proto-bitmap->bitmap p-horizontal))
(define vertical (proto-bitmap->bitmap p-vertical))
(define bottom-right-corner (proto-bitmap->bitmap p-bottom-right-corner))
(define top-right-corner (proto-bitmap->bitmap p-top-right-corner))
(define bottom-left-corner (proto-bitmap->bitmap p-bottom-left-corner))
(define top-left-corner (proto-bitmap->bitmap p-top-left-corner))
(define top-t (proto-bitmap->bitmap p-top-t))
(define bottom-t (proto-bitmap->bitmap p-bottom-t))
(define left-t (proto-bitmap->bitmap p-left-t))
(define right-t (proto-bitmap->bitmap p-right-t))
(define cross (proto-bitmap->bitmap p-cross))
(define forcefield (proto-bitmap->bitmap p-forcefield))
     
;; Maze content
;; ~~~~~~~~~~~~
;; todo: fruity things
(define p-dot
  (make-proto-bitmap 20
                     20
                     '(() () () () () () ()
                       (0 0 0 0 0 0 0 0 1 1 1 1)
                       (0 0 0 0 0 0 0 0 1 1 1 1)
                       (0 0 0 0 0 0 0 0 1 1 1 1)
                       (0 0 0 0 0 0 0 0 1 1 1 1))))

(define p-pill
  (make-proto-bitmap 20
                     20
                     '(() () () ()
                       (0 0 0 0 0 0 0 0 1 1 1 1)
                       (0 0 0 0 0 0 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 0 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 0 0 0 1 1 1 1))))

(define dot (proto-bitmap->bitmap p-dot))
(define pill (proto-bitmap->bitmap p-pill))
(define blank (make-object bitmap% (make-bytes (* 3 20) 0) 20 20))

;; Pacbloke various stages of animation...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define p-bloke-shut
  (make-proto-bitmap 20
                     20
                     '((0 0 0 0 0 0 0 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 0 0 1 1 1 1 1 1))))

(define p-bloke-ajar-right
  (make-proto-bitmap 20
                     20
                     '((0 0 0 0 0 0 0 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1) 
                       (1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 0 0 1 1 1 1 1 1))))

(define p-bloke-open-right
  (make-proto-bitmap 20
                     20
                     '((0 0 0 0 0 0 0 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1) 
                       (0 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1) 
                       (1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 0 0 0 0 1 1 1 1 1 1))))

(define p-bloke-ajar-left (horizontal-flip p-bloke-ajar-right))
(define p-bloke-open-left (horizontal-flip p-bloke-open-right))
(define p-bloke-ajar-down (rotate p-bloke-ajar-right))
(define p-bloke-open-down (rotate p-bloke-open-right))
(define p-bloke-ajar-up (vertical-flip p-bloke-ajar-down))
(define p-bloke-open-up (vertical-flip p-bloke-open-down))

(define bloke-shut (proto-bitmap->bitmap p-bloke-shut))
(define bloke-ajar-right (proto-bitmap->bitmap p-bloke-ajar-right))
(define bloke-open-right (proto-bitmap->bitmap p-bloke-open-right))
(define bloke-ajar-left (proto-bitmap->bitmap p-bloke-ajar-left))
(define bloke-open-left (proto-bitmap->bitmap p-bloke-open-left))
(define bloke-ajar-down (proto-bitmap->bitmap p-bloke-ajar-down))
(define bloke-open-down (proto-bitmap->bitmap p-bloke-open-down))
(define bloke-ajar-up (proto-bitmap->bitmap p-bloke-ajar-up))
(define bloke-open-up (proto-bitmap->bitmap p-bloke-open-up))

;; ghosts
;; ~~~~~~

(define p-ghost
  (make-proto-bitmap 20
                     20
                     '((0 0 0 0 0 0 0 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1)
                       (0 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1)
                       (0 1 1 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 1)
                       (1 1 1 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 1 1)
                       (1 1 1 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 1 1)
                       (1 1 1 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 1 1)
                       (1 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1 1)
                       (1 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1)
                       (1 1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 1 1)
                       (1 1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 1 1))))

(define p-scared-ghost
  (make-proto-bitmap 20
                     20
                     '((0 0 0 0 0 0 0 1 1 1 1 1 1)
                       (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)
                       (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (0 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1)
                       (0 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1)
                       (1 1 1 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1)
                       (1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1)
                       (1 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1 0 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                       (1 1 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1)
                       (1 1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 1 1)
                       (1 1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 1 1))))

(define ghost (proto-bitmap->bitmap p-ghost))
(define scared-ghost (proto-bitmap->bitmap p-scared-ghost))

;; ghost scores
;; ~~~~~~~~~~~~

(define p-score-200
  (make-proto-bitmap 20
                     20
                     '(() () () () () ()
                       (0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1)
                       (0 0 0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1))))

(define p-score-400
  (make-proto-bitmap 20
                     20
                     '(() () () () () ()
                       (0 0 1 0 0 0 0 0 1 1 1 1 0 0 1 1 1 1)
                       (0 0 1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 0 1 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 0 1 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 0 0 1 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 0 0 1 0 0 0 1 1 1 1 0 0 1 1 1 1))))

(define p-score-800
  (make-proto-bitmap 20
                     20
                     '(() () () () () ()
                       (0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1)
                       (0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1))))

(define p-score-1600
  (make-proto-bitmap 20
                     20
                     '(() () () () () ()
                       (0 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1)
                       (1 1 0 0 1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 1 0 0 1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 1 0 0 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1)
                       (0 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1))))

(define score-200 (proto-bitmap->bitmap p-score-200))
(define score-400 (proto-bitmap->bitmap p-score-400))
(define score-800 (proto-bitmap->bitmap p-score-800))
(define score-1600 (proto-bitmap->bitmap p-score-1600))
