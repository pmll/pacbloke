#lang racket

;; plenty mutation

(require "common.rkt"
         "maze.rkt"
         "movement.rkt")

(provide make-maze-state
         cell-state
         eatables-left
         eat-cell!
         make-scorer
         set-last-input!
         last-input
         consume-last-input!
         last-input-a-direction?
         toggle-sound!
         sound-on?
         make-ghost-bounty
         )

(define (eatable? cell-content)
  (or (eq? cell-content 'dot) (eq? cell-content 'powerpill)))

(define (eatables maze)
  (+ (maze-count maze 'dot) (maze-count maze 'powerpill)))
  
(define (make-maze-state maze)
  (list (maze->2dvect maze)
        (maze-width maze)
        (maze-height maze)
        (box (eatables maze))))

(define (cell-state maze-state x y)
  (if (and (>= x 0) (< x (cadr maze-state)) (>= y 0) (< y (caddr maze-state)))
      (2dvect-ref (car maze-state) x y)
      'gap))

(define (eatables-left maze-state) (unbox (cadddr maze-state)))

(define (fractional-part n) (- n (floor n)))

;; we have to get over half way into a cell before we can eat its goodies
(define (eat-cell! maze-state maze-roamer)
  (define (meal x y)
    (let ((content (cell-state maze-state x y)))
      (if (eatable? content)
          (begin
            (2dvect-set! (car maze-state) x y 'gap)
            (set-box! (cadddr maze-state) (- (unbox (cadddr maze-state)) 1))
            content)
          #f)))
  (let ((half (/ 1 2))
        (x (roamer-x maze-roamer))
        (y (roamer-y maze-roamer)))
    (cond ((integer? x)
           (let ((y-encroachment (fractional-part y)))
             (cond ((> y-encroachment half) (meal x (+ (floor y) 1)))
                   ((< y-encroachment half) (meal x (floor y)))
                   (else #f))))
          ((integer? y)
           (let ((x-encroachment (fractional-part x)))
             (cond ((> x-encroachment half) (meal (+ (floor x) 1) y))
                   ((< x-encroachment half) (meal (floor x) y))
                   (else #f))))
          (else #f))))

(define (make-scorer init-value)
  (define score init-value)
  (lambda (score-adjustment)
    (set! score (+ score score-adjustment))
    score))

(define *last-input* #f)

(define (set-last-input! key-code)
  (set! *last-input* (case key-code
                       ('up 'up)
                       ('down 'down)
                       ('left 'left)
                       ('right 'right)
                       ((#\q #\Q) 'quit)
                       ((#\s #\S) 'togglesound)
                       ((#\p #\P) (if (eq? *last-input* 'pause) 'restart 'pause))
                       ((#\r #\R) 'restart)
                       (else *last-input*))))

(define (last-input) *last-input*)

(define (consume-last-input!) (set! *last-input* #f))

(define (last-input-a-direction?)
  (or (eq? *last-input* 'up)
      (eq? *last-input* 'down)
      (eq? *last-input* 'left)
      (eq? *last-input* 'right)))

(define *sound-on* #t)

(define (toggle-sound!) (set! *sound-on* (not *sound-on*)))

(define (sound-on?) *sound-on*)

(define (make-ghost-bounty init-value)
  (define bounty 0)
  (lambda (op)
    (cond ((eq? op 'reset) (set! bounty init-value) bounty)
          ((eq? op 'consume)
           (begin0
             bounty
             (set! bounty (* bounty 2))))
          (else bounty))))

