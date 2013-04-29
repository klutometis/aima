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
(define (reading-of orientation)
  (cond ((eq? orientation right-of) right-of)
        ((eq? orientation left-of) right-of)
        ((eq? orientation above) below)
        ((eq? orientation below) below)))

;;; Need a generic word which happens to take an orientation.
(define (word board square next-square)
  (let ((previous-square (reverse-of next-square)))
    (do ((square square (next-square square)))
        ((not (board-ref/default board square #f))
         (do ((square (previous-square square)
                      (previous-square square))
              (word '() (cons (board-ref/default board square #f) word)))
             ((not (board-ref/default board square #f)) word))))))

(define (word-vertical board square)
  (word board square below))

(define (word-horizontal board square)
  (word board square right-of))

;;; This is a misnomer: it may not be a crosscheck, but merely a
;;; check, in the case where we're testing for parallel contiguous
;;; words.
(define (crosscheck dag word)
  (and (match? dag word) (length word)))

(define (square-neighbors square)
  (list (left-of square)
        (right-of square)
        (above square)
        (below square)))

(define (square-occupied? board square)
  (board-ref/default board square #f))

(define (anchor? board square)
  (not (every (cut square-occupied? board <>)
              (square-neighbors square))))

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

;;; This is somehow orientation-dependent; how, is a little weird. It
;;; would be nice to think about this generally: why don't the
;;; horizontal neighbors of anchors "count" when thinking horizontal?
;;; They do: it's just that they're pruned; they get considered when
;;; proceeding horizontally from anchors.
;;;
;;; Vertical neighbors, on the other hand, don't get considered unless
;;; we explicitly add them in. Why is that? Why can't we solve this
;;; more generally?
(define (board-anchors board)
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
   '()))

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

;;; A word has: start, letters, orientation; it's a vector. A game
;;; having extents is more interesting, isn't it? Some constraints.
;;; The triple-word-score bullshit, too.
;;;
;;; Could also simply do a collection of square-character pairs; nah.
;;;
;;; A word will be a reversed collection of letters with a possible
;;; sentinel.
;;;
;;; The game server needs to do redundant cross checks, &c.; let's
;;; abstract this into a function.
;;;
;;; We'll also have to have some function which, given a direction,
;;; finds an inverse.
;;;
;;; Should we not do sentinels, &c.; to reduce the burden on other
;;; clients? Just: start, string, orientation?
;;;
;;; Left or down maps most naturally to the DAGGAD, incidentally.
;;;
;;; Christ, haven't yet dealt with blanks.
;;;
;;; Different versions of the game: some with unlimited extent;
;;; others, limited with word- and letter-scores? Three dimensional?
;;;
;;; Randomly pull off the top candidate.
;;;
;;; There's so much logic in here, I'd hate to reproduce it for the
;;; game server; we need to abstract it.
#;
(let ((board (make-board))
      (dag (make-dag))
      ;; The rack
      (tiles '(#\E #\C #\R #\A))
      (moves (make-max-heap)))
  ;; (debug? #f)
  (update-dag! dag "ABLE")
  (update-dag! dag "AR")
  (update-dag! dag "ABLER")
  (update-dag! dag "BE")
  (update-dag! dag "CABLE")
  (update-dag! dag "CARE")
  (board-set! board (make-square 0 0) #\A)
  (board-set! board (make-square 0 -1) #\B)
  (board-set! board (make-square 0 -2) #\L)
  (board-set! board (make-square 0 -3) #\E)
  ;; Copy game; place; try; erase.
  ;; (dag-debug dag 0)
  (debug (board-anchors board))
  (for-each
      (lambda (square)
        (let iter ((current-square square)
                   (rack (cons sentinel tiles))
                   (subdag dag)
                   (next-square left-of)
                   (score 0)
                   (board (board-copy board))
                   (word '()))
          (debug current-square rack score (and subdag #t))
          ;; Need dag-checks and terminal checks.
          (when subdag
            ;; When we determine terminal, we also need to have the
            ;; word hitherto, don't we?
            ;;
            ;; Also crosscheck the sideways word on terminal, in case
            ;; we abut something horizontally.
            (when (dag-terminal? subdag)
              (debug word (word->string word))
              (board-display board)
              (let* ((crosscheck (crosscheck dag (word-horizontal board square)))
                     ;; We need to account for horizontally adjoining
                     ;; words (if any): count the current word plus
                     ;; horizontally adjoining words and the subtract
                     ;; the current word.
                     (score (+ score (- crosscheck (length (delete sentinel word))))))
                (heap-insert! moves score (make-word current-square word left-of))))
            (let ((character (board-ref/default board current-square #f)))
              (debug 'preëxisting character)
              (if character
                  (iter (next-square current-square)
                        rack
                        (dag-ref subdag character)
                        next-square
                        (add1 score)
                        board
                        (cons character word))
                  (for-each (lambda (character)
                              (debug 'iterate character)
                              (if (sentinel? character)
                                  (let ((next-square right-of))
                                    (iter (next-square square)
                                          (delete sentinel rack)
                                          (dag-ref subdag sentinel)
                                          next-square
                                          score
                                          board
                                          (cons character word)))
                                  (begin
                                    (board-set! board current-square character)
                                    (let* ((vertical (word-vertical board current-square))
                                           (crosscheck (if (= (length vertical) 1)
                                                           1
                                                           (crosscheck dag vertical))))
                                      (debug crosscheck)
                                      (when crosscheck
                                        (iter (next-square current-square)
                                              (delete-first character rack)
                                              (dag-ref subdag character)
                                              next-square
                                              ;; Subtract 1 from cross
                                              ;; check; since we'll
                                              ;; add the score at the
                                              ;; terminal horizontal
                                              ;; crosscheck.
                                              ;;
                                              ;; This is to account
                                              ;; for the possibility
                                              ;; of horizontally
                                              ;; adjoining words.
                                              ;;
                                              ;; No, we'll add it
                                              ;; later.
                                              (+ score crosscheck)
                                              (board-copy board)
                                              (cons character word)))))))
                    rack))))))
    (board-anchors board))
  (debug (heap->alist moves)))

(define-record-and-printer scrabble
  board
  tiles
  scores)

(define-record-and-printer game
  state
  terminal?
  ;; Updates score, &c.; takes an agent, calls play; updates score?
  play)

;; (dag-debug (make-dag-from-file "words.txt") 0)

(define (shuffle! v)
  (do ((n (vector-length v) (- n 1)))
      ((zero? n) v)
    (let* ((r (random n)) (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))

(define (make-scrabble-game)
  (make-game
   (make-scrabble
    (make-board)
    ;; We have a problem: srfi-1#delete deletes all instances. Can't
    ;; have multiple tiles therewith and delete only one. To hash
    ;; tables with histograms? Damn.
    ;;
    ;; That's ok; implemented delete-first.
    ;;
    ;; Actually, this is only a problem for agents; the game can shuffle
    '(#\E #\C #\R #\A)
    (make-hash-table))
   (lambda (scrabble)
     ;; Also, six successive scoreless turns
     ;; (<http://en.wikipedia.org/wiki/Scrabble#Sequence_of_play>).
     (zero? (length (scrabble-tiles scrabble))))
   (lambda (scrabble player)
     (let ((move ((player-play player) scrabble)))
       (if (legal? scrabble move)
           (player-score-set! player (score scrabble move))
           #f)))))

;;; Generalize this at some point; game has a state and some
;;; termination function.
(define (play game players)
  (let iter ((players (apply circular-list players)))
    (if (game-terminal? game)
        (game-state game)
        (if (game-play game (car players))
            ;; Successful move, go to the next player.
            (iter (cdr players))
            ;; Circular; some failure to move.
            (iter players)))))

;; 5\.4:1 ends here
