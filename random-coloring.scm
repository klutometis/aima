#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*6.10][6\.10:1]]

(use ;; aima-tessellation
     aima-csp
     call-with-environment-variables
     debug
     define-record-and-printer
     files
     format
     graphviz
     matchable
     random-bsd
     shell
     srfi-1
     srfi-95)

(define (make-labels points)
  (let ((labels (make-hash-table)))
    (for-each (lambda (point) (hash-table-set! labels point (gensym))) points)
    labels))

(define (write-map-as-dot map solution dot)
  (with-output-to-file dot
    (let ((points (hash-table-keys map))
          (edges (make-hash-table)))
      (lambda ()
        (write-dot-preamble)
        (let ((labels (make-labels points)))
          (for-each (lambda (point)
                      (write-node (hash-table-ref labels point)
                                  (point-x point)
                                  (point-y point)
                                  (hash-table-ref solution point)))
            points)
          (hash-table-walk map
            (lambda (whence whithers)
              (let ((whence-label (hash-table-ref labels whence)))
                (for-each (lambda (whither)
                            (unless (hash-table-exists? edges (cons whither whence))
                              (hash-table-set! edges (cons whence whither) #t)
                              (let ((whither-label (hash-table-ref labels whither)))
                                (write-edge whence-label whither-label))))
                  whithers)))))
        (write-dot-postscript)))))

(define (write-map-as-png map solution png)
  (let ((dot (create-temporary-file)))
    (write-map-as-dot map solution dot)
    (run (neato -n1 -Tpng -o ,png < ,dot))))

(define (random-element list)
  (list-ref list (random (length list))))

(define (solution? assignment csp)
  (call/cc
   (lambda (return)
     (hash-table-walk (csp-neighbors csp)
       (lambda (whence whithers)
         (for-each
             (lambda (whither)
               (let ((constraint (hash-table-ref (csp-constraints csp) (cons whence whither))))
                 (unless (constraint (hash-table-ref assignment whence)
                                     (hash-table-ref assignment whither))
                   (return #f))))
           whithers)))
     (return #t))))

(define (conflicted-variables assignment csp)
  (hash-table-fold
   (csp-constraints csp)
   (lambda (scope constraint conflicts)
     (let ((x (car scope))
           (y (cdr scope)))
       (if (constraint (hash-table-ref assignment x)
                       (hash-table-ref assignment y))
           conflicts
           (lset-adjoin eq? conflicts x y))))
   '()))

(define max-steps (make-parameter 10000))

(define (min-conflicts csp)
  (let ((assignment (hash-table-fold
                     (csp-domains csp)
                     (lambda (variable domain assignment)
                       (hash-table-set! assignment variable (random-element domain))
                       assignment)
                     (make-hash-table))))
    (let iter ((variables (conflicted-variables assignment csp))
               (step (max-steps)))
      (cond ((null? variables) assignment)
            ((zero? step) failure)
            (else
             (let ((conflicted-variable (random-element variables)))
               (let ((neighbor-constraints
                      (map (lambda (neighbor)
                             (cons neighbor
                                   (hash-table-ref
                                    (csp-constraints csp)
                                    (cons conflicted-variable neighbor))))
                           (hash-table-ref (csp-neighbors csp) conflicted-variable)))
                     (domain (hash-table-ref (csp-domains csp) conflicted-variable)))
                 (let* ((value-conflicts
                         (sort
                          (map (lambda (value)
                                 (cons value
                                       (count (complement values)
                                              (map (match-lambda
                                                       ((neighbor . constraint)
                                                        (constraint
                                                         value
                                                         (hash-table-ref
                                                          assignment
                                                          neighbor))))
                                                   neighbor-constraints))))
                               domain)
                          <
                          cdr))
                        (min-conflict
                         (cdar value-conflicts))
                        (min-conflicts
                         (take-while (lambda (conflict) (= (cdr conflict) min-conflict)) value-conflicts))
                        (min-conflicting-value
                         (car (random-element min-conflicts))))
                   ;; (debug (hash-table->alist assignment)
                   ;;        conflicted-variable
                   ;;        value-conflicts
                   ;;        min-conflicts
                   ;;        min-conflicting-value)
                   (hash-table-set! assignment conflicted-variable min-conflicting-value)
                   (iter (conflicted-variables assignment csp) (sub1 step))))))))))

(define (display-map-as-png map solution)
  (let ((png (create-temporary-file ".png")))
    (write-map-as-png map solution png)
    (run (sxiv ,png))))

(define-syntax time+value
  (ir-macro-transformer
   (lambda (expression inject compare)
     `(begin (##sys#start-timer)
             (let ((value ,@(cdr expression)))
               (values (vector-ref (##sys#stop-timer) 0) value))))))

(define (experiment name solution)
  (let iter ((n 2))
    (let ((map (random-map n))
          (domains (make-hash-table))
          (constraints (make-hash-table)))
      (set-domains! domains (hash-table-keys map) '(red green blue yellow))
      (hash-table-walk map
        (lambda (whence whithers)
          (for-each (lambda (whither)
                      (set-bidirectional-constraint!
                       constraints
                       whence
                       whither
                       neq?
                       neq?))
            whithers)))
      (let ((csp (make-csp domains constraints map)))
        (call-with-values (lambda () (time+value (solution csp)))
          (lambda (time value)
            (unless (failure? value)
              (let ((file (call-with-environment-variables
                           '(("TMPDIR" . "random-coloring"))
                           (lambda () (create-temporary-file)))))
                (with-output-to-file file
                  (lambda () (format #t "~a,~a,~a~%" name n time)))))
            (debug name n time value)))))
    (iter (add1 n))))

(experiment "min-conflicts"
            (parameterize ((max-steps 10000000))
              min-conflicts))
;; (experiment "backtracking"
;;             (parameterize ((inference (lambda x (make-hash-table))))
;;               backtracking-search))
;; (experiment "forward-checking"
;;             backtracking-search)

;; (let ((map (random-map 50))
;;       (domains (make-hash-table))
;;       (constraints (make-hash-table)))
;;   (set-domains! domains (hash-table-keys map) '(red green blue yellow))
;;   (hash-table-walk map
;;     (lambda (whence whithers)
;;       (for-each (lambda (whither)
;;                   (set-bidirectional-constraint!
;;                    constraints
;;                    whence
;;                    whither
;;                    neq?
;;                    neq?))
;;         whithers)))
;;   (let ((csp (make-csp domains constraints map)))

;;     (let ((solution (parameterize ((max-steps 10000)) (min-conflicts csp))
;;            ;; (backtracking-search csp)
;;            ;; (parameterize ((inference (lambda x (make-hash-table))))
;;            ;;   (backtracking-search csp))
;;            ))
;;       (unless (failure? solution)
;;         (display-map-as-png map solution)
;;         (debug (hash-table->alist solution))))
;;     ))

;; 6\.10:1 ends here
