#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*Logical%20agents][Logical-agents:5]]

(use debug
     matchable
     srfi-1
     test)

(define (satisfy formula)
  (let* ((clauses (conjuncts formula))
         (all-variables (all-variables clauses)))
    (debug clauses all-variables)
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
        (let ((unit-clauses (unit-clauses clauses)))
          (if (null? unit-clauses)
              (values clauses assignment)
              (iter (simplify* clauses unit-clauses)
                    (append unit-clauses assignment)))))))

(define-syntax
  xor
  (lambda (expression rename compare)
    (match expression
      ((_ x y)
       (let ((%or (rename 'or)) (%and (rename 'and)) (%not (rename 'not)))
                       `(,%and (,%or ,x ,y) (,%not (,%and ,x ,y))))))))

(define (simplify clauses literal)
  (let ((literal-variable (variable literal))
        (negative? (negative-clause? literal)))
    (fold-right (lambda (clause simplifications)
                  (cons 'or
                        (let iter ((clause (disjuncts clause))
                                   (simplification '()))
                          (if (null? clause)
                              (cons simplification simplifications)
                              (let* ((term (car clause))
                                     (negative-term? (negative-clause? term)))
                                (if (eq? literal-variable (variable term))
                                    (if (or (and negative? negative-term?)
                                            (and (not negative?) (not negative-term?)))
                                        simplifications
                                        (iter (cdr clause)
                                              simplification))
                                    (iter (cdr clause)
                                          (cons term simplification))))))))
                '()
                clauses)
    ;; (if (negative-clause? literal)
    ;;     (remove-variable clauses variable)
    ;;     (filter-clauses clauses variable))
    ))

(trace simplify)

;;; This also needs to handle e.g. negatives.
;; (define (remove-variable clauses variable)
;;   (map (lambda (clause)
;;          (filter (lambda (terms) )
;;                  (disjuncts clause)))
;;        clauses))

(define (filter-clauses clauses variable)
  (filter (lambda (clause) (not (memq variable clause))) clauses))

(define (unit-clauses clauses)
  (filter literal-clause? clauses))

(define (unit-literals clauses)
  (map variable (unit-clauses clauses)))

(define (negative-clause? literal)
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

(define (variable literal)
  (match literal
    (('not p) p)
    (p p)))

(define args cdr)

(define (variables clause)
  (if (atomic-clause? clause)
      (list (variable clause))
      (map variable (args clause))))

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
      (and (negative-clause? clause)
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
