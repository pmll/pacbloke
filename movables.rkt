#lang racket

(provide (struct-out roamer)
         (struct-out ghost)
         (struct-out target))

(define-struct roamer (x y speed direction next-direction blocked-by next-speed))

(define-struct ghost (roamer id mode flee-frame home-node target))

(define-struct target (node not-via))
