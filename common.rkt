#lang racket

(provide infinity
         fnof
         list->immutable-2dvect
         2dvect-ref
         2dvect-set!
         2dvect-memq
         2dvect-count
         2dvect-copy)

(define infinity 999999)  ; near enough ;-)

(define (fnof fn accessor . objs) (apply fn (map accessor objs)))

(define (make-2dvect w h val) (list w h (make-vector (* w h) val)))

(define (list->immutable-2dvect w h lst)
  (list w h (vector->immutable-vector (list->vector lst))))

(define (index x y w) (+ x (* y w)))

(define (2dvect-ref v x y) (vector-ref (caddr v) (index x y (car v))))

(define (2dvect-set! v x y val) (vector-set! (caddr v) (index x y (car v)) val))

(define (2dvect-memq item v)
  (define pos (vector-memq item (caddr v)))
  (cond (pos (values (remainder pos (car v)) (quotient pos (cadr v))))
        (else (values #f #f))))

(define (2dvect-count item v)
  (vector-count (lambda (e) (eq? e item)) (caddr v)))

(define (2dvect-copy v) (list (car v) (cadr v) (vector-copy (caddr v))))


