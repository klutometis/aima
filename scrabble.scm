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

(define make-game make-hash-table)
(define game-copy hash-table-copy)
(define game-set! hash-table-set!)
(define game-ref hash-table-ref)
(define game-ref/default hash-table-ref/default)

(define (word-vertical game square)
  (do ((square square (below square)))
      ((not (game-ref/default game square #f))
       (do ((square (above square) (above square))
            (word '() (cons (game-ref/default game square #f) word)))
           ((not (game-ref/default game square #f)) word)))))

(define (word-horizontal game square)
  (do ((square square (right-of square)))
      ((not (game-ref/default game square #f))
       (do ((square (left-of square) (left-of square))
            (word '() (cons (game-ref/default game square #f) word)))
           ((not (game-ref/default game square #f)) word)))))

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

(define (square-occupied? game square)
  (game-ref/default game square #f))

(define (anchor? game square)
  (not (every (cut square-occupied? game <>)
              (square-neighbors square))))

(define (unoccupied-neighbors game square)
  (filter (cut (complement square-occupied?) game <>)
          (square-neighbors square)))

(define (game-display game)
  (match
      (hash-table-fold
       game
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
         (display (game-ref/default game (make-square x y) " ")))))))

(let ((game (make-game))
      (dag (make-dag))
      ;; The rack
      (tiles '(#\E #\C #\R #\A)))
  (debug? #f)
  (update-dag! dag "ABLE")
  (update-dag! dag "AR")
  (update-dag! dag "ABLER")
  (update-dag! dag "BE")
  (update-dag! dag "CABLE")
  (update-dag! dag "CARE")
  (game-set! game (make-square 0 0) #\A)
  (game-set! game (make-square 0 -1) #\B)
  (game-set! game (make-square 0 -2) #\L)
  (game-set! game (make-square 0 -3) #\E)
  ;; Copy game; place; try; erase.
  ;; (dag-debug dag 0)
  (hash-table-walk game
    (lambda (square character)
      (when (anchor? game square)
        (let iter ((current-square square)
                   (rack (cons sentinel tiles))
                   (subdag dag)
                   (next-square left-of)
                   (score 0)
                   (game (game-copy game))
                   ;; (word '())
                   )
          (debug current-square rack score (and subdag #t))
          ;; Need dag-checks and terminal checks.
          (when subdag
            ;; When we determine terminal, we also need to have the
            ;; word hitherto, don't we?
            (debug (dag-terminal? subdag)
                   (when (dag-terminal? subdag)
                     (hash-table->alist game)))
            (when (dag-terminal? subdag)
              (game-display game))
            (let ((character (game-ref/default game current-square #f)))
              (debug 'preëxisting character)
              (if character
                  (iter (next-square current-square)
                         rack
                         (dag-ref subdag character)
                         next-square
                         (add1 score)
                         game)
                  (for-each (lambda (character)
                              (debug 'iterate character)
                              (if (sentinel? character)
                                  (let ((next-square right-of))
                                    (iter (next-square square)
                                          (delete sentinel rack)
                                          (dag-ref subdag sentinel)
                                          next-square
                                          score
                                          game))
                                  (begin
                                    (game-set! game current-square character)
                                    (let* ((vertical (word-vertical game current-square))
                                           (crosscheck (if (= (length vertical) 1)
                                                           1
                                                           (crosscheck dag vertical))))
                                      (debug crosscheck)
                                      (when crosscheck
                                        (iter (next-square current-square)
                                              (delete character rack)
                                              (dag-ref subdag character)
                                              next-square
                                              (+ score crosscheck)
                                              (game-copy game)))))))
                    rack)))))))))

;; (dag-debug (make-dag-from-file "words.txt") 0)


;; 5\.4:1 ends here
