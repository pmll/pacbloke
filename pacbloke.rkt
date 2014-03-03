#lang racket

;; play the game pacbloke

(require racket/gui/base)

(require "mazes.rkt"
         "display.rkt"
         "movement.rkt"
         "state.rkt")

(define game-canvas%
  (class canvas%
    (define/override (on-char keyevent)
      (set-last-input! (send keyevent get-key-code)))
    (super-new)))

(define (play-maze score! lives! maze-lst)
  (let* ((maze (car maze-lst))
         (maze-state (make-maze-state maze))
         (player (make-player maze))
         (ghost-lst (make-ghosts maze))
         (frame-number 0))
  (define maze-frame (new frame% (label "Pacbloke")))
  (define score-canvas (new canvas% (parent maze-frame)
                                    (paint-callback
                                      (lambda (c dc)
                                        (render-score c
                                                      dc
                                                      (lives! 0)
                                                      (score! 0))))))
  (define maze-canvas
    (new game-canvas% (parent maze-frame)
                      (paint-callback (lambda (c dc) (render-maze c maze)))))
  (define maze-dc (send maze-canvas get-dc))
  (define ticker
    (new timer%
         (notify-callback
           (lambda ()
             (if (eq? (last-input) 'quit)
               (begin
                 (send maze-frame show #f)
                 (send ticker stop)
                 (consume-last-input!)
                 ;; really quit? dialogue here
                 )
               (let* ((new-player (player-movement player (if (last-input-a-direction?) (last-input) #f) maze))
                      (player-meal (eat-cell! maze-state new-player))
                      (new-ghost-lst (ghost-movement ghost-lst maze player-meal frame-number)))
                 (send maze-canvas suspend-flush)
                 (unrender (roamer-x player)
                           (roamer-y player)
                           maze-dc
                           maze-state)
                 (for-each (lambda (g)
                              (unrender (roamer-x (ghost-roamer g))
                                        (roamer-y (ghost-roamer g))
                                        maze-dc
                                        maze-state))
                           ghost-lst)
                 (render-bloke (roamer-x new-player)
                               (roamer-y new-player)
                               (roamer-direction new-player)
                               maze-dc
                               frame-number)
                 (for ((g new-ghost-lst))
                   (render-ghost (roamer-x (ghost-roamer g))
                                 (roamer-y (ghost-roamer g))
                                 (ghost-id g)
                                 (ghost-mode g)
                                 (+ (- scared-ghost-frames
                                       frame-number)
                                    (ghost-flee-frame g))
                                 maze-dc))
                 (send maze-canvas resume-flush)
                 (send maze-canvas flush)
                 (set! player new-player)
                 (set! ghost-lst new-ghost-lst)
                 (when (eq? (last-input) 'togglesound) (toggle-sound!))
                 (consume-last-input!)
                 (when (eq? player-meal 'dot)
                       (score! 10)
                       (render-dot-eaten)
                       (send score-canvas refresh))
                 (when (eq? player-meal 'powerpill)
                       (score! 50)
                       (render-powerpill-eaten)
                       (send score-canvas refresh))
                 (set! frame-number (+ frame-number 1))
                 (when (zero? (eatables-left maze-state))
                   (send ticker stop)
                   (sleep 1)
                   (send maze-frame show #f)
                   (play-maze score! lives! (append (cdr maze-lst) (list maze))))))))
         (interval 50)))
  (send maze-frame show #t)
  (send maze-canvas focus)
  (display "playing a maze") (newline)))

(play-maze (make-scorer 0) (make-scorer 3) (list mini-trad-maze))

