#lang racket

;; settings for pacbloke

(require "mazes.rkt")

(provide (all-defined-out))

(define debug #f)
(define frame-interval 50)              ; millisec
(define initial-lives 3)
(define scared-ghost-frames 100)
(define ghost-flash-period 3)           ; in frames
(define ghost-flash-point 21)           ; in frames remaining
;; speeds should be such that the object in question is guaranteed to align
;; with a cell at some point as it travels through it
(define player-speed (/ 1 5))
(define ghost-speed (/ 1 5))
(define scared-ghost-speed (/ 1 10))
(define returning-ghost-speed (/ 1 2))
; fraction of overlapping area required between ghost and player to be
; considered a collision
(define min-collision-area (/ 1 10))
; these mazes will be played in an endless loop
(define maze-order (list mini-trad-maze traditional-maze))
