#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.4][5\.4:1]]

(use debug
     define-record-and-printer
     extras
     format
     heap
     loops
     matchable
     srfi-1
     vector-lib)

(define-record-and-printer sentinel)
(define sentinel (make-sentinel))

(define (character->index character)
  (if (sentinel? character)
      26
      (- (char->integer character) 65)))

(define (index->character index)
  (if (= index 26)
      sentinel
      (integer->char (+ index 65))))

;;; Can get this down to bits?
(define (make-dag) (make-vector 28 #f))
(define (dag-terminal? dag) (vector-ref dag 27))
(define (dag-terminal?-set! dag terminal?)
  (vector-set! dag 27 terminal?))
(define (dag-ref dag character)
  (vector-ref dag (character->index character)))
(define (dag-set! dag character value)
  (vector-set! dag (character->index character) value))

(define (integrate-fixes dag prefix suffix)
  (let ((characters (if (null? suffix)
                      prefix
                      (append (append prefix (list sentinel))
                              suffix))))
    (dag-terminal?-set!
     (fold (lambda (character dag)
             (unless (dag-ref dag character)
               (dag-set! dag character (make-dag)))
             (dag-ref dag character))
           dag
           characters)
     #t)))

(define (update-dag! dag word)
  (let ((characters (string->list word)))
    (do ((prefix (list (car characters)) (cons (car suffix) prefix))
         (suffix (cdr characters) (cdr suffix)))
        ((null? suffix) (integrate-fixes dag prefix suffix))
      (integrate-fixes dag prefix suffix))))

(define (dag-debug dag depth)
  (vector-for-each
   (lambda (i x)
     (when (vector? x)
       ;; (debug (index->character i))
       (format #t "~a~a~%" (make-string depth #\_) (index->character i))
       (dag-debug x (add1 depth))))
   dag))

;;; Be nice to store these fuckers in a graph database or something.
(define (make-dag-from-file file)
  (let ((dag (make-dag)))
    (with-input-from-file file
      (lambda ()
        (do ((word (read-line) (read-line)))
            ((eof-object? word))
          (debug word)
          (update-dag! dag word))))
    dag))

(define make-square cons)
(define square-x car)
(define square-y cdr)

(define (above square)
  (cons (square-x square)
        (+ (square-y square) 1)))

(define (below square)
  (cons (square-x square)
        (- (square-y square) 1)))

(define (left-of square)
  (cons (- (square-x square) 1)
        (square-y square)))

(define (right-of square)
  (cons (+ (square-x square) 1)
        (square-y square)))

(define (above? s1 s2)
  (below? s2 s1))

(define (below? s1 s2)
  (= 1 (- (square-y s2) (square-y s1))))

(define (left-of? s1 s2)
  (right-of? s2 s1))

(define (right-of? s1 s2)
  (= 1 (- (square-x s1) (square-x s2))))

(define (insert-sentinel word)
  (cons* (car word)
         sentinel
         (cdr word)))

;;; To avoid that bizarre sentinel-insertion shit, why not reverse the
;;; letters?
(define (match? dag word)
  (let iter ((dag dag)
             ;; We have to insert a sentinel when matching because of
             ;; that idiosyncrasy of GADDAGs where a sentinel always
             ;; comes second (except when the suffix is ∅).
             (word (insert-sentinel word)))
    (if (null? word)
        (dag-terminal? dag)
        (let* ((character (car word))
               (subdag (dag-ref dag character)))
          (if subdag
              (iter subdag (cdr word))
              #f)))))

(define (match? dag word)
  (let iter ((dag dag)
             ;; We have to insert a sentinel when matching because of
             ;; that idiosyncrasy of GADDAGs where a sentinel always
             ;; comes second (except when the suffix is ∅).
             (word word))
    (if (null? word)
        (dag-terminal? dag)
        (let* ((character (car word))
               (subdag (dag-ref dag character)))
          (if subdag
              (iter subdag (cdr word))
              #f)))))

;;; NB: game -> board, in preparation for make-scrabble.
(define make-board make-hash-table)
(define (board-empty? board) (zero? (hash-table-size board)))
(define board-copy hash-table-copy)
(define board-set! hash-table-set!)
(define board-ref hash-table-ref)
(define board-ref/default hash-table-ref/default)

(define (reverse-of orientation)
  (cond ((eq? orientation right-of) left-of)
        ((eq? orientation left-of) right-of)
        ((eq? orientation above) below)
        ((eq? orientation below) above)))

(define (orthogonal-to orientation)
  (cond ((eq? orientation right-of) above)
        ((eq? orientation left-of) below)
        ((eq? orientation above) right-of)
        ((eq? orientation below) left-of)))

;;; For a given orientation, the direction in which we read.
(define (crosscheck-of orientation)
  (cond ((eq? orientation right-of) left-of)
        ((eq? orientation left-of) left-of)
        ((eq? orientation above) above)
        ((eq? orientation below) above)))

;;; Need a generic word which happens to take an orientation.
(define (word-scan board square next-square)
  (let ((previous-square (reverse-of next-square)))
    (do ((square square (next-square square)))
        ((not (board-ref/default board square #f))
         (do ((square (previous-square square)
                      (previous-square square))
              (word '() (cons (board-ref/default board square #f) word)))
             ((not (board-ref/default board square #f)) word))))))

(define (word-score word)
  (fold (lambda (character score)
          (+ (hash-table-ref character->points character)
             score))
        0
        word))

;;; This is a misnomer: it may not be a crosscheck, but merely a
;;; check, in the case where we're testing for parallel contiguous
;;; words.
(define (crosscheck dag word)
  ;; (debug word (match? dag word))
  (and (or (= (length word) 1)
           (match? dag word))
       (word-score word)))

(define (square-neighbors square)
  (list (left-of square)
        (right-of square)
        (above square)
        (below square)))

(define (square-occupied? board square)
  (board-ref/default board square #f))

(define (unoccupied-neighbors board square)
  (filter (cut (complement square-occupied?) board <>)
          (square-neighbors square)))

(define (board-display board)
  (match
      (hash-table-fold
       board
       (lambda (square character minimum-maximum)
         (match minimum-maximum
           ((minimum . maximum)
            (cons (make-square (inexact->exact (min (square-x minimum)
                                                    (square-x square)))
                               (inexact->exact (min (square-y minimum)
                                                    (square-y square))))
                  (make-square (inexact->exact (max (square-x maximum)
                                                    (square-x square)))
                               (inexact->exact (max (square-y maximum)
                                                    (square-y square))))))))
       (cons (make-square +inf.0 +inf.0)
             (make-square -inf.0 -inf.0)))
    ((minimum . maximum)
     (do ((y (square-y maximum) (sub1 y)))
         ((< y (square-y minimum)))
       (when (= y (square-y maximum))
         (display " ")
         (do ((x (square-x minimum) (add1 x)))
             ((> x (square-x maximum)))
           (display (abs (remainder x 10))))
         (newline))
       (display (abs (remainder y 10)))
       (do ((x (square-x minimum) (add1 x)))
           ((> x (square-x maximum)) (newline))
         (display (board-ref/default board (make-square x y) " ")))))))

(define (anchor? board square)
  (not (every (cut square-occupied? board <>)
              (square-neighbors square))))

(define (board-anchors board next-square)
  (let* ((above (orthogonal-to next-square))
         (below (reverse-of above)))
    (hash-table-fold
     board
     (lambda (square character anchors)
       (append
        (filter
         identity
         (list (and (anchor? board square)
                    square)
               (and (not (square-occupied? board (above square)))
                    (above square))
               (and (not (square-occupied? board (below square)))
                    (below square))))
        anchors))
     '())))

(define-record-and-printer word
  start
  characters
  orientation)

(define (normalize-characters characters)
  (if (member sentinel characters)
      (let iter ((characters characters)
                 (word '()))
        (if (null? characters)
            word
            (let ((character (car characters)))
              (if (sentinel? character)
                  (iter (reverse (cdr characters))
                        word)
                  (iter (cdr characters)
                        (cons character word))))))
      characters))

(define (word->string characters)
  (list->string (normalize-characters characters)))

(define delete-first
  (case-lambda
   ((x list) (delete-first x list equal?))
   ((x list =)
    (receive (prefix suffix)
      (break (lambda (y) (= x y)) list)
      (if (null? suffix)
          prefix
          (append prefix (cdr suffix)))))))

(define-record-and-printer player
  play
  score
  rack
  name)

;;; In additional to `reverse', need an `orthogonal' for orientations.
(define-record-and-printer scrabble-move
  start
  word
  orientation)

(define-record-and-printer scrabble
  board
  tiles
  lexicon)

(define-record-and-printer game
  state
  init
  terminal?
  ;; Updates score, &c.; takes an agent, calls play; updates score?
  play)

;; (dag-debug (make-dag-from-file "words.txt") 0)

;;; Scrabble is also known as: "game"; return a copy of the board, so
;;; we don't have to propagate later?
;;;
;;; Let's also test against the lexicon here instead of waiting for
;;; the end.
(define (scrabble-score scrabble player move)
  (let ((board (board-copy (scrabble-board scrabble)))
        (next-square (word-orientation move)))
    ;; (debug 'scrabble-score
    ;;        (word-orientation move)
    ;;        (word-characters move)
    ;;        next-square)
    (let iter ((square (word-start move))
               (characters (word-characters move))
               (score 0)
               (next-square next-square))
      ;; (debug square)
      ;; (board-display board)
      (if (null? characters)
          ;; Shit; we can also check the word incrementally; avoiding
          ;; expensive crosschecks, &c. It really is identical in
          ;; player and game.
          (begin
            ;; (debug "Characters are null.")
            (and (match? (scrabble-lexicon scrabble)
                         (word-scan board
                                    (word-start move)
                                    (crosscheck-of next-square)))
                 ;; Need some terminal arithmetic here for adjoining
                 ;; words.
                 (scrabble-board-set! scrabble board)
                 score))
          (let ((character (board-ref/default board square #f)))
            ;; TODO: We have to test this against their rack, don't
            ;; we? Every placement, for that matter.
            (if character
                (begin
                  ;; (debug "There is a character on the board.")
                  ;; (when (and (not (sentinel? (car characters)))
                  ;;            (not (char=? character (car characters))))
                  ;;   (debug "Character mismatch!"
                  ;;          character
                  ;;          (car characters)
                  ;;          move))
                  (if (sentinel? (car characters))
                      (iter square
                            (reverse (cdr characters))
                            score
                            next-square)
                      (and (char=? character (car characters))
                           (iter (next-square square)
                                 (cdr characters)
                                 (+ score (hash-table-ref character->points character))
                                 next-square))))
                (let ((character (car characters)))
                  ;; (debug "There is not a character on the board.")
                  (if (sentinel? character)
                      (begin
                        ;; (debug "The character is a sentinel.")
                        (iter square
                              (reverse (cdr characters))
                              score
                              next-square))
                      (begin
                        ;; (debug "The character is not a sentinel.")
                        (board-set! board square character)
                        ;; Let `word' reverse?
                        ;; (debug (orthogonal-to next-square))
                        (let* ((orthogonal
                                (word-scan board
                                           square
                                           (crosscheck-of
                                            (orthogonal-to next-square))))
                               ;; Can we put this logic in crosscheck itself?
                               (crosscheck (crosscheck (scrabble-lexicon scrabble)
                                                       orthogonal)))
                          ;; (debug orthogonal crosscheck)
                          (and crosscheck
                               (iter (next-square square)
                                     (cdr characters)
                                     (+ score crosscheck)
                                     next-square))))))))))))

(define n-tiles-in-rack (make-parameter 7))

(define (plenish-rack! scrabble player)
  (let ((rack-deficit (- (n-tiles-in-rack)
                         (length (player-rack player))))
        (n-tiles (length (scrabble-tiles scrabble))))
    (when (and (positive? rack-deficit)
               (positive? n-tiles))
      (let* ((n-new-tiles (min rack-deficit n-tiles))
             (new-tiles
              (take (scrabble-tiles scrabble) n-new-tiles)))
        (player-rack-set! player
                          (append (player-rack player)
                                  new-tiles))
        (scrabble-tiles-set! scrabble
                             (drop (scrabble-tiles scrabble)
                                   n-new-tiles))))))

(define n-wrong-moves (make-parameter 3))

(define (shuffle! v)
  (do ((n (vector-length v) (- n 1)))
      ((zero? n) v)
    (let* ((r (random n)) (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))

(define character->points
  (alist->hash-table
   `((,sentinel . 0)
     (#\E . 1)
     (#\A . 1)
     (#\I . 1)
     (#\O . 1)
     (#\N . 1)
     (#\R . 1)
     (#\T . 1)
     (#\L . 1)
     (#\S . 1)
     (#\U . 1)
     (#\D . 2)
     (#\G . 2)
     (#\B . 3)
     (#\C . 3)
     (#\M . 3)
     (#\P . 3)
     (#\F . 4)
     (#\H . 4)
     (#\V . 4)
     (#\W . 4)
     (#\Y . 4)
     (#\K . 5)
     (#\J . 8)
     (#\X . 8)
     (#\Q . 10)
     (#\Z . 10))))

;;; Fuck; forgot about points per tile. We can have that in a table
;;; elsewhere, though. Display it somehow?
(define (make-scrabble-tiles)
  (let ((tiles
         (list->vector
          (append
           (make-list 12 #\E)
           (make-list 9 #\A)
           (make-list 9 #\I)
           (make-list 8 #\O)
           (make-list 6 #\N)
           (make-list 6 #\R)
           (make-list 6 #\T)
           (make-list 4 #\L)
           (make-list 4 #\S)
           (make-list 4 #\U)
           (make-list 4 #\D)
           (make-list 3 #\G)
           (make-list 2 #\B)
           (make-list 2 #\C)
           (make-list 2 #\M)
           (make-list 2 #\P)
           (make-list 2 #\F)
           (make-list 2 #\H)
           (make-list 2 #\V)
           (make-list 2 #\W)
           (make-list 2 #\Y)
           (make-list 1 #\K)
           (make-list 1 #\J)
           (make-list 1 #\X)
           (make-list 1 #\Q)
           (make-list 1 #\Z)))))
    (shuffle! tiles)
    (vector->list tiles)))

(define (make-play-tiles)
  (list #\C #\A #\R #\E))

(define (make-scrabble-game lexicon)
  (let ((pass (make-hash-table))
        (wrong-moves (make-hash-table)))
    (make-game
     (make-scrabble
      (make-board)
      ;; We have a problem: srfi-1#delete deletes all instances. Can't
      ;; have multiple tiles therewith and delete only one. To hash
      ;; tables with histograms? Damn.
      ;;
      ;; That's ok: implemented delete-first.
      ;;
      ;; Actually, this is only a problem for agents; the game can shuffle
      ;; '(#\E #\C #\R #\A)
      ;; '(#\X)
      (make-scrabble-tiles)
      ;; (make-play-tiles)
      lexicon)
     (lambda (scrabble players)
       (for-each (cut plenish-rack! scrabble <>) players))
     ;; Also end the game if everyone passes.
     (lambda (scrabble players)
       ;; Also, six successive scoreless turns
       ;; (<http://en.wikipedia.org/wiki/Scrabble#Sequence_of_play>).
       ;;
       ;; Need to also check the players' racks.
       ;; (debug (scrabble-tiles scrabble)
       ;;        (map player-rack players)
       ;;        (hash-table->alist pass))
       (or (and (zero? (length (scrabble-tiles scrabble)))
                (any zero?
                     (map (lambda (player) (length (player-rack player)))
                          players)))
           (and (positive? (hash-table-size pass))
                (every values (hash-table-values pass)))))
     ;; Player forfeits turn on n bad moves (i.e. passes).
     (lambda (scrabble player)
       (plenish-rack! scrabble player)
       ;; (debug (player-rack player))
       (let ((move ((player-play player)
                    (scrabble-board scrabble)
                    (player-rack player))))
         (debug move (player-rack player))
         (if move
             (let ((score (scrabble-score scrabble player move)))
               ;; Must also take player into consideration: do they have the
               ;; appropriate tiles, &c.? Is it worthwhile calculating this
               ;; separately from the score, since there's some ovelap
               ;; (crosschecks, &c.); or all at once?
               ;;
               ;; Also, should `legal?' be a formal part of the game
               ;; structure? Nah; it's an ad-hoc subproblem.
               ;;
               ;; Score is an integer or `#f' if the move is illegal?
               ;; (debug move score)
               (debug score)
               (if score
                   (begin
                     (board-display (scrabble-board scrabble))
                     ;; (debug (player-score player) score)
                     (player-score-set! player (+ (player-score player) score))
                     (hash-table-set! wrong-moves (player-name player) 0)
                     (hash-table-set! pass (player-name player) #f)
                     ;; Normalize-characters and reading-of should be redundant
                     ;; (and even destructive, if the former reverses yet
                     ;; again).
                     (let iter ((characters (word-characters move))
                                (square (word-start move)))
                       (unless (null? characters)
                         (let ((character (car characters)))
                           ;; (board-set! (scrabble-board scrabble) square character)
                           (player-rack-set! player (delete-first character (player-rack player)))
                           (iter (cdr characters)
                                 ((word-orientation move) square))))))
                   (begin
                     (hash-table-update!/default
                      wrong-moves
                      (player-name player)
                      add1
                      0)
                     (and (= (hash-table-ref/default wrong-moves (player-play name) 0)
                             (n-wrong-moves))
                          (hash-table-set! pass (player-name player) #t)))))
             (begin
               (hash-table-set! pass (player-name player) #t)
               #t)))))))

(define (calculate-moves! lexicon moves next-square board rack)
  (for-each
      (lambda (square)
        (let iter ((current-square square)
                   (rack (cons sentinel rack))
                   (subdag lexicon)
                   (next-square next-square)
                   (score 0)
                   (board board)
                   (word '())
                   (played-yet? #f))
          ;; (debug current-square rack score (and subdag #t))
          ;; Need dag-checks and terminal checks.
          (when subdag
            ;; When we determine terminal, we also need to have the
            ;; word hitherto, don't we?
            ;;
            ;; Also crosscheck the sideways word on terminal, in case
            ;; we abut something horizontally.
            (when (and played-yet? (dag-terminal? subdag))
              ;; (debug word (word->string word))
              ;; (board-display board)
              ;; (let* ((crosscheck (crosscheck lexicon (reverse (word-horizontal board square))))
              ;; (debug square
              ;;        next-square
              ;;        (crosscheck-of next-square)
              ;;        (word-scan board square (crosscheck-of next-square))
              ;;        (crosscheck lexicon (word-scan board square (crosscheck-of next-square)))
              ;;        word)
              ;; We need to account for horizontally adjoining
              ;; words (if any): count the current word plus
              ;; horizontally adjoining words and the subtract
              ;; the current word.
              (let ((crosscheck (crosscheck lexicon (word-scan board square (crosscheck-of next-square)))))
                ;; (debug crosscheck)
                ;; (debug next-square)
                (when crosscheck
                  (let ((score (+ score (- crosscheck (length (delete sentinel word))))))
                    (heap-insert! moves
                                  score
                                  ;; Would be right-of or below,
                                  ;; since we've normalized the
                                  ;; characters.
                                  ;;
                                  ;; Should we denormalize?
                                  (make-word ((reverse-of next-square) current-square)
                                             word
                                             (reverse-of next-square)))))))
            (let ((character (board-ref/default board current-square #f)))
              ;; (debug 'preëxisting character)
              (if character
                  (iter (next-square current-square)
                        rack
                        (dag-ref subdag character)
                        next-square
                        (+ score (hash-table-ref character->points character))
                        board
                        (cons character word)
                        played-yet?)
                  (for-each (lambda (character)
                              ;; (debug 'iterate character)
                              (if (sentinel? character)
                                  (let ((next-square (reverse-of next-square)))
                                    (iter (next-square square)
                                          (delete sentinel rack)
                                          (dag-ref subdag sentinel)
                                          next-square
                                          score
                                          board
                                          (cons character word)
                                          played-yet?))
                                  (begin
                                    (let ((board (board-copy board)))
                                      (board-set! board current-square character)
                                      (let* ((orthogonal
                                              (word-scan board
                                                         current-square
                                                         (crosscheck-of
                                                          (orthogonal-to next-square))))
                                             (crosscheck (crosscheck lexicon orthogonal)))
                                        ;; (debug crosscheck)
                                        (when crosscheck
                                          (iter (next-square current-square)
                                                (delete-first character rack)
                                                (dag-ref subdag character)
                                                next-square
                                                (+ score crosscheck)
                                                board
                                                (cons character word)
                                                #t)))))))
                    rack))))))
    (board-anchors board next-square)))

;;; Make a player here; game has state; we have a rack. Authoritative?
;;; Ask the server? What happens when they diverge?
;;;
;;; Should the game communicate back what our state is, whether the
;;; move succeeded?
(define (make-scrabble-player lexicon)
  (make-player
   (lambda (board rack)
     (let ((moves (make-max-heap)))
       (calculate-moves! lexicon moves left-of board rack)
       (calculate-moves! lexicon moves above board rack)
       (and (not (heap-empty? moves))
            (begin
              ;; (debug (heap->alist moves))
              ;; Heap might be empty; might have to forfeit the turn.
              (heap-extract-extremum! moves)))))
   0
   '()))

;;; Generalize this at some point; game has a state and some
;;; termination function.
(define (play game players)
  ((game-init game) (game-state game) players)
  (let iter ((round-robin (apply circular-list players)))
    ;; (debug ((game-terminal? game) (game-state game) players))
    ;; Players isn't part of the game-state, huh?
    (if ((game-terminal? game) (game-state game) players)
        (game-state game)
        (if ((game-play game) (game-state game) (car round-robin))
            ;; Successful move, go to the next player.
            (iter (cdr round-robin))
            ;; Circular; some failure to move. Should the player
            ;; forfeit? That's up to the game, isn't it?
            (iter round-robin)))))

(define (make-play-dag)
  (let ((lexicon (make-dag)))
    (update-dag! lexicon "ABLE")
    (update-dag! lexicon "AR")
    (update-dag! lexicon "ABLER")
    (update-dag! lexicon "BE")
    (update-dag! lexicon "CABLE")
    (update-dag! lexicon "CARE")
    lexicon))

(let ((lexicon (make-dag-from-file "words-four-letter.txt")
               ;; (make-dag-from-file "words.txt")
               ;; (make-play-dag)
               ))
  (let ((game (make-scrabble-game lexicon))
        (players (list (make-scrabble-player lexicon)
                       ;; (make-scrabble-player lexicon)
                       )))
    (let ((scrabble (game-state game)))
      (let ((board (scrabble-board scrabble)))
        (board-set! board (make-square 0 0) #\A)
        (board-set! board (make-square 0 -1) #\B)
        (board-set! board (make-square 0 -2) #\L)
        (board-set! board (make-square 0 -3) #\E))
     (play game players)
     (board-display (scrabble-board scrabble))
     (debug (map player-score players)))))

;; 5\.4:1 ends here
