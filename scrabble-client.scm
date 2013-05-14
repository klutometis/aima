#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:4]]

(include "scrabble.scm")

(use debug
     matchable
     medea
     posix
     vector-lib
     zmq)

(define (letter->character letter)
  (car (string->list letter)))

(define (orientation->string orientation)
  (cond ((eq? orientation right-of) "RIGHT-OF")
        ((eq? orientation left-of) "LEFT-OF")
        ((eq? orientation above) "ABOVE")
        ((eq? orientation below) "BELOW")))

(let ((socket (make-socket 'req))
      (name (symbol->string (gensym)))
      (lexicon (parameterize ((debug? #f))
                 (make-dag-from-file "words-four-letter.txt"))))
  (connect-socket socket "tcp://*:5555")
  ;; (debug (json->string `((name . ,name))))
  (send-message socket (json->string `((name . ,name))))
  (let iter ((message (read-json (receive-message* socket))))
    (debug 'client message)
    (match message
      ((('board . tiles)
        ('rack . rack)
        ('turn . turn))
       (let ((board (make-board))
             (rack (map letter->character (vector->list rack))))
         (vector-for-each
          (lambda (i tile)
            (match tile
              ((('x . x)
                ('y . y)
                ('letter . letter))
               (board-set! board (make-square x y) (letter->character letter)))))
          tiles)
         (let ((moves (make-max-heap)))
           (calculate-moves! lexicon moves left-of board rack)
           (calculate-moves! lexicon moves above board rack)
           (if (heap-empty? moves)
               (send-message socket (json->string `((name . ,name))))
               (let ((move (heap-extract-extremum! moves)))
                 (send-message socket
                               (json->string `((name . ,name)
                                               (x . ,(square-x (word-start move)))
                                               (y . ,(square-y (word-start move)))
                                               ;; Need a move->json
                                               (characters . ,(characters->json (word-characters move)))
                                               (orientation
                                                . ,(orientation->string (word-orientation move))))))))))))
    (iter (read-json (receive-message* socket)))))

;; 5\.5:4 ends here
