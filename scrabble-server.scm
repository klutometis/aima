#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:5]]

(include "scrabble.scm")

(use alist-lib
     debug
     loops
     medea
     srfi-1
     zmq)

(define (play game players)
  ((game-init game) (game-state game) players)
  (let iter ((round-robin (apply circular-list players)))
    ;; Players isn't part of the game-state, huh?
    (if ((game-terminal? game) (game-state game) players)
        (game-state game)
        (if ((game-play game) (game-state game) (car round-robin))
            ;; Successful move, go to the next player.
            (iter (cdr round-robin))
            ;; Circular; some failure to move. Should the player
            ;; forfeit? That's up to the game, isn't it?
            (iter round-robin)))))

;; ((move #(word (-8 . 13) (#\T #\R #\I #\M) #<procedure (right-of square186)>))
;;  ((player-rack player) (#\I #\R #\I #\T)))
;; ((move #(word (-5 . 13) (#\M #\R #(sentinel) #\G #\E) #<procedure (above square180)>))
;;  ((player-rack player) (#\I #\M #\R #\R #\I #\T #\G)))
;; ((move #(word (-1 . 22) (#\V #\A #\L #\E) #<procedure (below square182)>))
;;  ((player-rack player) (#\N #\A #\V #\L)))

(define (string->orientation string)
  (cond ((string=? string "RIGHT-OF") right-of)
        ((string=? string "LEFT-OF") left-of)
        ((string=? string "ABOVE") above)
        ((string=? string "BELOW") below)))

(define (json->move move)
  (match move
    ((('name . name)
      ('x . x)
      ('y . y)
      ('characters . characters)
      ('orientation . orientation))
     (make-word (make-square x y)
                (json->characters characters)
                (string->orientation orientation)))))

(define current-move (make-parameter #f))

(define (make-remote-scrabble-player socket name)
  (make-player
   (lambda (board rack)
     (current-move))
   0
   '()
   name))

(let ((lexicon (parameterize ((debug? #f))
                 (make-dag-from-file "words-four-letter.txt")))
      (socket (make-socket 'rep)))
  (let ((game (make-scrabble-game lexicon))
        ;; (players (list (make-scrabble-player lexicon)
        ;;                (make-scrabble-player lexicon)))
        (players (make-hash-table))
        (turns '()))
    (let ((board (scrabble-board (game-state game))))
      (board-set! board (make-square 0 0) #\A)
      (board-set! board (make-square 0 -1) #\B)
      (board-set! board (make-square 0 -2) #\L)
      (board-set! board (make-square 0 -3) #\E))
    (bind-socket socket "tcp://*:5555")
    (let iter ((message (read-json (receive-message* socket))))
      (debug 'server message)
      (if ((game-terminal? game) (game-state game) (hash-table-values players))
          (begin
            (board-display (scrabble-board scrabble))
            (debug (map player-score players)
                   (map player-rack players)))
          (let ((name (alist-ref message 'name)))
            (unless (hash-table-exists? players name)
              (let ((player (make-remote-scrabble-player socket name)))
                ((game-init game) (game-state game) (list player))
                (hash-table-set! players name player)
                (set! turns (apply circular-list (hash-table-keys players)))))
            (let ((player (hash-table-ref players name))
                  (turn (car turns)))
              (send-message
               socket
               (json->string
                `((board . ,(board->json (scrabble-board (game-state game))))
                  (rack . ,(characters->json (player-rack player)))
                  (turn . ,turn))))
              (if (and (eq? name turn) (alist-ref/default message 'x #f))
                  (begin
                    ;; Ugly
                    (set! turns (cdr turns))
                    (parameterize ((current-move (json->move message)))
                      ((game-play game) (game-state game) player))))
              (iter (read-json (receive-message* socket)))))))))

;; 5\.5:5 ends here
