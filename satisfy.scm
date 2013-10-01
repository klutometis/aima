#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*Logical%20agents][Logical-agents:5]]

(use debug
     matchable
     srfi-1
     test)

(define (satisfy formula)
  (debug formula)
  (let* ((clauses (map clauses (clauses formula)))
         (all-variables (all-variables clauses)))
    (debug clauses)
    (let iter ((clauses clauses)
               (assignment '()))
      (call-with-values (lambda () (propagate-unit-clauses clauses assignment))
        (lambda (clauses assignment)
          (debug clauses assignment)
          (cond ((exists-empty-clause? clauses) #f)
                ;; This is where we'd do some dynamic shit and maybe a
                ;; call-cc.
                ((null? clauses) assignment #t)
                (else
                 (let ((variable (select-variable all-variables assignment)))
                   (or (iter (simplify clauses variable)
                             (cons variable assignment))
                       (iter (simplify clauses (negate variable))
                             (cons (negate variable) assignment)))))))))))

(define (conjuncts formula)
  (match formula
    (('and . ps) ps)
    (p (list p))))

(define (disjuncts formula)
  (match formula
    (('or . ps) ps)
    (p (list p))))

(define (clauses formula)
  (match formula
    (('and . conjuncts) conjuncts)
    (('or . disjuncts) disjuncts)
    (p (list p))))

(define (select-variable all-variables assignment)
  (let ((candidates (lset-difference eq? all-variables (variables assignment))))
    (list-ref candidates (random (length candidates)))))

(define (propagate-unit-clauses clauses assignment)
  (let iter ((clauses clauses)
             (assignment assignment))
    (if (exists-empty-clause? clauses)
        (values clauses assignment)
        (let ((unit-literals (unit-literals clauses)))
          (if (null? unit-literals)
              (values clauses assignment)
              (iter (simplify* clauses unit-literals)
                    (append unit-literals assignment)))))))

(define (simplify clauses literal)
  (let ((variable (variable literal)))
    (if (negative? literal)
        (remove-variable clauses variable)
        (filter-clauses clauses variable))))

(define (remove-variable clauses variable)
  (map (lambda (clause) (delete variable clause)) clauses))

(define (filter-clauses clauses variable)
  (filter (lambda (clause) (not (memq variable clause))) clauses))

(define (unit-clauses clauses)
  (filter (lambda (clause) (= (length clause) 1)) clauses))

(define (unit-literals clauses)
  (map car (unit-clauses clauses)))

(define (variable literal)
  (match literal
    (('not p) p)
    (p p)))

(define (variables clauses)
  (map variable clauses))

(define (negative? literal)
  (match literal
    (('not p) #t)
    (_ #f)))

(define (exists-empty-clause? clauses)
  (if (null? clauses)
      #f
      (if (null? (car clauses))
          #t
          (exists-empty-clause? (cdr clauses)))))

(define (simplify* clauses literals)
  (fold-right (lambda (literal clauses)
                (simplify clauses literal))
              clauses
              literals))

(define (negate literal)
  `(not ,literal))

(define (all-variables clauses)
  (delete-duplicates
   (fold-right
    (lambda (clause all-variables)
      (append (variables clause) all-variables))
    '()
    clauses)))

(define (atomic-clause? clause)
  (match clause
    (('and . p) #f)
    (('or . p) #f)
    (('=> . p) #f)
    (('<=> . p) #f)
    (_ #t)))

(define (literal-clause? clause)
  (or (atomic-clause? clause)
      (and (negative? clause)
           (atomic-clause? (car (clauses clause))))))

(define (eliminate-implications formula)
  (match formula
    ((? literal-clause?) formula)
    (('=> p q) `(or (not ,p) ,q))
    (('<=> p q)
     `(and (or ,p (not ,q))
           (or (not ,q) ,p)))
    ((op . ps) `(,op ,@(map eliminate-implications ps)))))

(define (move-not-inwards formula)
  (match formula
    (('not p) p)
    (('and . ps)
     `(or ,@(map move-not-inwards ps)))
    (('or . ps)
     `(and ,@(map move-not-inwards ps)))
    (p `(not ,p))))

(define (merge-disjuncts clauses)
  (case (length clauses)
    ((0) #f)
    ((1) (car clauses))
    (else
     `(and ,@(let ((result (make-parameter '())))
               (for-each
                   (lambda (p)
                     (for-each
                         (lambda (q)
                           (result (cons `(or ,@(append (disjuncts p)
                                                        (disjuncts q)))
                                         (result))))
                       (conjuncts (first clauses))))
                 (conjuncts (merge-disjuncts (cdr clauses))))
               (result))))))

(define (->cnf formula)
  (let ((formula (eliminate-implications formula)))
    (match formula
      (('not p) (let ((q (move-not-inwards p)))
                  (if (literal-clause? q) q (->cnf q))))
      (('and . ps)
       `(and ,@(append-map (lambda (p) (conjuncts (->cnf p))) ps)))
      (('or . ps)
       (merge-disjuncts (map ->cnf ps)))
      (p p))))

(define (tell knowledge-base p)
  (append knowledge-base (clauses (->cnf p))))

(define (tell* knowledge-base . ps)
  (fold-right (lambda (p knowledge-base)
                ;; (debug p (->cnf p))
                (tell knowledge-base (->cnf p)))
              knowledge-base
              ps))

(test (->cnf '(and (or B (and C (or M N) F) D) (or W(and P (or Q (and X Y) X A)))))
      '(and (or D C B) (or D N M B) (or D F B) (or A X Y Q W) (or A X X Q W) (or P W)))

(let ((knowledge-base '(and)))
  (satisfy
   (tell* knowledge-base
          '(and (not s11)
                (not s21)
                s12
                (not b11)
                b21
                (not b12))
          '(=> (not s11) (and (not w11)
                              (not w12)
                              (not w13)))
          '(=> (not s21) (and (not w11)
                              (not w21)
                              (not w22)
                              (not w31)))
          '(=> (not s12) (and (not w11)
                              (not w12)
                              (not w22)
                              (not w13)))
          '(=> s12 (or w13
                       w12
                       w22
                       w11)))))

;; (satisfy '(and s (not s)))
;; (satisfy '(and s))
;; (satisfy '(and (or W13 W12 W22 W11) (or S12 (not W11)) (or S12 (not W12))
;;                (or S12 (not W22)) (or S12 (not W13)) (or S21 (not W11))
;;                (or S21 (not W21)) (or S21 (not W22)) (or S21 (not W31))
;;                (or S11 (not W11)) (or S11 (not W12)) (or S11 (not W13)) (not S11)
;;                (not S21) S12 (not B11) B21 (not B12)))

;; Logical-agents:5 ends here
