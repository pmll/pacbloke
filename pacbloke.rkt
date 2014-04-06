#lang racket

;; play the game pacbloke

(require racket/gui/base)

(require "mazes.rkt"
         "display.rkt"
         "movement.rkt"
         "state.rkt"
         "maze.rkt")

(define debug #t)

(define frame-interval 50)
;(define frame-interval 250)

(define game-canvas%
  (class canvas%
    (define/override (on-char keyevent)
      (set-last-input! (send keyevent get-key-code)))
    (super-new)))

(define (play-maze score! lives! maze-lst)
  (define maze (car maze-lst))
  (define maze-state (make-maze-state maze))
  (define player (make-player maze))
  (define ghost-lst (make-ghosts maze))
  (define frame-number 0)
  (define ghost-score! (make-ghost-bounty 200))
  (define maze-frame (new frame% (label "Pacbloke")))
  (define score-canvas
    (new canvas% (parent maze-frame)
                 (paint-callback (lambda (c dc)
                                   (render-score c dc (lives! 0) (score! 0))))))
  (define maze-canvas
    (new game-canvas% (parent maze-frame)
                      (paint-callback (lambda (c dc) (render-maze c maze)))))
  (define maze-dc (send maze-canvas get-dc))
  (define (unrender-all-roamers player ghost-lst)
    (unrender (roamer-x player) (roamer-y player) maze-dc maze-state)
    (for-each (lambda (g) (unrender (roamer-x (ghost-roamer g))
                                    (roamer-y (ghost-roamer g))
                                    maze-dc
                                    maze-state))
              ghost-lst)
    (when debug
          (for-each (lambda (g) (unrender (node-x (target-node (ghost-target g)))
                                          (node-y (target-node (ghost-target g)))
                                          maze-dc
                                          maze-state))
                    ghost-lst)))
  (define (render-player player)
    (render-bloke (roamer-x player)
                  (roamer-y player)
                  (roamer-direction player)
                  maze-dc
                  frame-number))
  (define (render-all-roamers player ghost-lst)
    (render-player player)
    (for ((g ghost-lst)) (render-ghost (roamer-x (ghost-roamer g))
                                       (roamer-y (ghost-roamer g))
                                       (ghost-id g)
                                       (ghost-mode g)
                                       (+ (- scared-ghost-frames
                                             frame-number)
                                          (ghost-flee-frame g))
                                       maze-dc))
    (when debug (for ((g ghost-lst))
                     (render-node-halo (node-x (target-node (ghost-target g)))
                                       (node-y (target-node (ghost-target g)))
                                       (ghost-id g)
                                       maze-dc))))
  (define (player-death)
    ;; an animation sequence of the death throes would be nice at this point.
    ;; for now, we pause a short while so as the calamity can be observed
    (send ticker stop)
    (render-lose-life)
    (sleep 2)
    (if (> (lives! -1) 0)
        (let ((new-player (make-player maze))
              (new-ghost-lst (make-ghosts maze)))
          (send maze-canvas suspend-flush)
          (unrender-all-roamers player ghost-lst)
          (render-all-roamers new-player new-ghost-lst)
          (send maze-canvas resume-flush)
          (send maze-canvas flush)
          (send score-canvas refresh)
          (set! player new-player)
          (set! ghost-lst new-ghost-lst)
          (send ticker start frame-interval))
        (send maze-frame show #f)))
  ;; update a normal gameplay frame
  (define (play-frame)
    (let* ((new-player (player-movement player (if (last-input-a-direction?) (last-input) #f) maze))
           (player-meal (eat-cell! maze-state new-player))
           (new-ghost-lst (ghost-movement ghost-lst
                                          maze
                                          player-meal
                                          frame-number
                                          (roamer-x player)
                                          (roamer-y player))))
      (send maze-canvas suspend-flush)
      (unrender-all-roamers player ghost-lst)
      (render-all-roamers new-player new-ghost-lst)
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
            (ghost-score! 'reset)
            (render-powerpill-eaten)
            (send score-canvas refresh))
      (set! frame-number (+ frame-number 1))
      (when (zero? (eatables-left maze-state))
            (send ticker stop)
            (sleep 1)
            (send maze-frame show #f)
            (play-maze score! lives! (append (cdr maze-lst) (list maze))))))
  ;; eat ghosts which involves displaying a score with a pause
  (define (eat-ghosts caught)
    (send ticker stop)
    (let ((new-ghost-lst
          (map (lambda (g)
                 (cond ((memq (ghost-id g) caught)
                        (send maze-canvas suspend-flush)
                        (unrender (roamer-x (ghost-roamer g))
                                  (roamer-y (ghost-roamer g))
                                  maze-dc
                                  maze-state)
                        ;; re-render player to restore what was taken
                        ;; out by ghost
                        (render-player player)
                        (render-ghost-score (roamer-x player)
                                            (roamer-y player)
                                            (roamer-x (ghost-roamer g))
                                            (roamer-y (ghost-roamer g))
                                            (ghost-score! 'read)
                                            maze-dc)
                        (send maze-canvas resume-flush)
                        (send maze-canvas flush)
                        (render-ghost-eaten)
                        ;; pause just long enough to read the score
                        (sleep 0.5)
                        (send maze-canvas suspend-flush)
                        (unrender-ghost-score (roamer-x player)
                                              (roamer-y player)
                                              (roamer-x (ghost-roamer g))
                                              (roamer-y (ghost-roamer g))
                                              maze-state
                                              maze-dc)
                        (send maze-canvas resume-flush)
                        (send maze-canvas flush)
                        (score! (ghost-score! 'consume))
                        (back-to-base g))
                       (else g)))
               ghost-lst)))
      (send score-canvas refresh)
      (set! ghost-lst new-ghost-lst)
      (send ticker start frame-interval)))
  (define ticker
    (new timer%
         ;; this callback proc represents the main game loop
         (notify-callback
           (lambda ()
             (let ((ghosts-captured (ghosts-caught player ghost-lst)))
               (cond ((eq? (last-input) 'quit)
                      (send maze-frame show #f)
                      (send ticker stop)
                      (consume-last-input!)
                      ;; really quit? dialogue here
                      )
                     ((player-caught? player ghost-lst) (player-death))
                     ((not (null? ghosts-captured)) (eat-ghosts ghosts-captured))
                     (else (play-frame))))))
         (interval frame-interval)))
  (send maze-frame show #t)
  (send maze-canvas focus)
  (when debug (display-shortest-dists maze))
  (display "playing a maze") (newline))

(play-maze (make-scorer 0)
           (make-scorer 3)
           (list mini-trad-maze example-maze traditional-maze))

