#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:5]]

(include "scrabble.scm")

(use alist-lib
     debug
     loops
     medea
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

(define (make-remote-scrabble-player socket name)
  (make-player
   (lambda (board rack)
     (send-message
      socket
      (json->string
       `((board . ,(board->json board))
         (rack . ,(list->vector
                   (map (lambda (character)
                          (make-string 1 character))
                        rack))))
       ;; `((board ,(board->json board))
       ;;   ;; (rack . ,(list->vector
       ;;   ;;           (map (lambda (character) (make-string 1 character))
       ;;   ;;                rack)))
       ;;   )
       ))
     #f)
   0
   '()
   name))

(let ((lexicon (parameterize ((debug? #f))
                 (make-dag-from-file "words-sample.txt")))
      (socket (make-socket 'rep)))
  (let ((game (make-scrabble-game lexicon))
        ;; (players (list (make-scrabble-player lexicon)
        ;;                (make-scrabble-player lexicon)))
        (players (make-hash-table)))
    (let ((board (scrabble-board (game-state game))))
      (board-set! board (make-square 0 0) #\A)
      (board-set! board (make-square 0 -1) #\B)
      (board-set! board (make-square 0 -2) #\L)
      (board-set! board (make-square 0 -3) #\E))
    (bind-socket socket "tcp://*:5555")
    (let iter ((move (read-json (receive-message* socket))))
      (if ((game-terminal? game) (game-state game) (hash-table-values players))
          (begin
            (board-display (scrabble-board scrabble))
            (debug (map player-score players)
                   (map player-rack players)))
          (let ((name (alist-ref move 'name)))
            (unless (hash-table-exists? players name)
              (let ((player (make-remote-scrabble-player socket name)))
                ((game-init game) (game-state game) (list player))
                (hash-table-set! players name player)))
            (let ((player (hash-table-ref players name)))
              ((game-play game) (game-state game) player)))))))

;; 5\.5:5 ends here
