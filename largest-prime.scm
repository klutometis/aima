#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:1]]

(use call-with-query)

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (fast-prime-n? n)
  (fast-prime? n 10))

(call-with-dynamic-fastcgi-query
 (let ((current-player #f)
       (start-move #f)
       (score (make-hash-table)))
   (lambda (query)
     (display-status-&c.)
     (let ((number (query-client-any query 'number))
           (player (query-client-any query 'player)))
       (unless current-player
         (set! current-player player)
         (set! start-move (current-seconds)))
       (if (string=? current-player player)
           (let ((delta (* (- (current-seconds) start-move) number)))
             (let ((delta (if (fast-prime-n? (string->number number))
                              delta
                              (- delta))))
               (hash-table-update! score
                                   current-player
                                   (lambda (score)
                                     (+ score delta))
                                   0)
               (if (positive? delta)
                   (begin
                     (set! current-player #f)
                     delta)
                   delta)))
           "Nope")))))

;; 5\.5:1 ends here
