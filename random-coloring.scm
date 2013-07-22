#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*6.10][6\.10:1]]

(use aima-tessellation
     aima-csp
     debug
     files
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

(define (write-map-as-dot map dot)
  (with-output-to-file dot
    (let ((points (hash-table-keys map)))
      (lambda ()
        (write-dot-preamble)
        (let ((labels (make-labels points)))
          (for-each (lambda (point)
                      (write-node (hash-table-ref labels point)
                                  (point-x point)
                                  (point-y point)))
            points)
          (hash-table-walk map
            (lambda (whence whithers)
              (let ((whence-label (hash-table-ref labels whence)))
                (for-each (lambda (whither)
                            (let ((whither-label (hash-table-ref labels whither)))
                              (write-edge whence-label whither-label)))
                  whithers)))))
        (write-dot-postscript)))))


;; 6\.10:1 ends here
