#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*6.10][6\.10:1]]

(use aima-tessellation
     aima-csp
     debug
     graphviz
     random-bsd
     shell
     srfi-1
     srfi-95)

(define (counter-clockwise? a b c)
  (> (* (- (point-y c) (point-y a))
        (- (point-x b) (point-x a)))
     (* (- (point-y b) (point-y a))
        (- (point-x c) (point-x a)))))

(define (intersect? a b c d)
  (and (neq? a b) (neq? a d) (neq? b c) (neq? b d)
       (xor (counter-clockwise? a c d) (counter-clockwise? b c d))
       (xor (counter-clockwise? a b c) (counter-clockwise? a b d))))

(define (random-points n)
  (list-tabulate n (lambda (i) (make-point (random-real) (random-real)))))

(define (sort-by-proximity point points)
  (sort points < (lambda (sortiendum) (point-distance point sortiendum))))

(define (make-labels points)
  (let ((labels (make-hash-table)))
    (for-each (lambda (point) (hash-table-set! labels point (gensym))) points)
    labels))

(define (intersects-other? connections point counter-point)
  (call/cc
   (lambda (return)
     (hash-table-walk connections
       (lambda (whence whithers)
         (for-each (lambda (whither)
                     (when (intersect? whence whither point counter-point)
                       (return #t)))
           whithers)))
     (return #f))))

(define (shuffle! v)
  (do ((n (vector-length v) (- n 1)))
      ((zero? n) v)
    (let* ((r (random n)) (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))

(define (shuffle list)
  (let ((vector (list->vector list)))
    (shuffle! vector)
    (vector->list vector)))

(let ((random-points (random-points 100))
      (connections (make-hash-table)))
  ;; Randomize the random-points.
  (let iter-point ((points random-points)
                   (modified? #f))
    ;; (debug 'points (length points))
    (if (null? points)
        (when modified?
          (iter-point (shuffle random-points) #f))
        (let ((point (car points)))
          (let iter-counter-point
              ((counter-points
                (sort-by-proximity point (delete point random-points))))
            ;; (debug 'counter-points (length counter-points))
            (if (null? counter-points)
                (iter-point (cdr points) modified?)
                (let ((counter-point (car counter-points)))
                  (if (member point
                            (hash-table-ref/default connections counter-point '()))
                      (iter-counter-point (cdr counter-points))
                      (if (intersects-other? connections point counter-point)
                          (iter-counter-point (cdr counter-points))
                          (begin
                            (hash-table-update!/default
                             connections
                             point
                             (lambda (counter-points)
                               (lset-adjoin eq? counter-points counter-point))
                             '())
                            (hash-table-update!/default
                             connections
                             counter-point
                             (lambda (points)
                               (lset-adjoin eq? points point))
                             '())
                            (iter-point (cdr points) #t))))))))))
  (with-output-to-file "harro.dot"
    (lambda () (write-dot-preamble)
       (let ((labels (make-labels random-points)))
         (for-each (lambda (point)
                     (write-node (hash-table-ref labels point)
                                 (point-x point)
                                 (point-y point)))
           random-points)
         (hash-table-walk connections
           (lambda (whence whithers)
             (let ((whence-label (hash-table-ref labels whence)))
               (for-each (lambda (whither)
                           (let ((whither-label (hash-table-ref labels whither)))
                             (write-edge whence-label whither-label)))
                 whithers)))))
       (write-dot-postscript)))
  (run (neato -n1 -Tpng -o harro.png harro.dot "&&" sxiv harro.png)))

;; 6\.10:1 ends here
