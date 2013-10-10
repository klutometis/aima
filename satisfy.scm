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
          ;; (debug clauses assignment)
          (cond ((exists-empty-clause? clauses) #f)
                ;; This is where we'd do some dynamic shit and maybe a
                ;; call-cc.
                ((null? clauses) assignment)
                (else
                 (debug clauses)
                 (let ((variable (select-variable all-variables assignment)))
                   (if variable
                       (or (iter (simplify clauses variable)
                                 (cons variable assignment))
                           (iter (simplify clauses (negate variable))
                                 (cons (negate variable) assignment)))
                       assignment)))))))))

;; (trace satisfy)

(define (conjuncts formula)
  (match formula
    (('and . ps) ps)
    (p (list p))))

(define (disjuncts formula)
  (match formula
    (('or . ps) ps)
    (p (list p))))

(define (disjunction clause)
  (if (list? clause)
      (if (= (length clause) 1)
          (car clause)
          (cons 'or clause))
      clause))

(define (conjunction clause)
  (if (list? clause)
      (if (= (length clause) 1)
          (car clause)
          (cons 'and clause))
      clause))

(define (clauses formula)
  (match formula
    (('and . conjuncts) conjuncts)
    (('or . disjuncts) disjuncts)
    (p (list p))))

(define (select-variable all-variables assignment)
  (debug assignment (variables assignment))
  (let ((candidates (lset-difference eq? all-variables (variables assignment))))
    (and (not (null? candidates))
         (list-ref candidates (random (length candidates))))))

(trace select-variable)

(define (propagate-unit-clauses clauses assignment)
  (let iter ((clauses clauses)
             (assignment assignment))
    ;; (debug clauses assignment)
    (if (exists-empty-clause? clauses)
        (values clauses assignment)
        (let ((unit-clauses (unit-clauses clauses)))
          (debug unit-clauses)
          (if (null? unit-clauses)
              (values clauses assignment)
              (let ((unit-clause (car unit-clauses)))
                (iter (simplify clauses unit-clause)
                      (cons unit-clause assignment))))))))

(trace propagate-unit-clauses)

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
    (let ((simplification
           (fold-right (lambda (clause simplifications)
                         ;; It's not going to be a disjunct, because
                         ;; we've put it in a minimalist form.
                         (let iter ((clause ;; (disjuncts clause)
                                     (match clause
                                       ((? symbol?) (list clause))
                                       ((? negative-clause?) (list clause))
                                       ((or . ps) ps)
                                       (p p))
                                     ;; (cond ((symbol? clause) (list clause))
                                     ;;       ((negative-clause? clause) (list clause))
                                     ;;       (else clause))
                                     )
                                    (simplification '()))
                           (debug clause simplification)
                           (if (null? clause)
                               (cons (if (= (length simplification) 1)
                                         (car simplification)
                                         simplification) simplifications)
                               (let* ((term (car clause))
                                      (negative-term? (negative-clause? term)))
                                 (debug term negative-term? (variable term))
                                 (if (eq? literal-variable (variable term))
                                     (if (or (and negative? negative-term?)
                                             (and (not negative?) (not negative-term?)))
                                         simplifications
                                         (iter (cdr clause)
                                               simplification))
                                     (iter (cdr clause)
                                           (cons term simplification)))))))
                       '()
                       clauses)))
      (delete-duplicates simplification))))

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
  (filter (lambda (clause)
            (or (not (list? clause))
                (= (length clause) 1)
                (negative-clause? clause)))
          clauses))

(trace unit-clauses)

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

;; (trace exists-empty-clause?)

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
  (debug clause (atomic-clause? clause))
  (if (atomic-clause? clause)
      (if (and (list? clause)
               (not (negative-clause? clause)))
          (map variable clause)
          (list (variable clause)))
      (map variable (args clause))))

(trace variables)

(define (all-variables clauses)
  (delete-duplicates
   (fold-right
    (lambda (clause all-variables)
      (append (variables clause) all-variables))
    '()
    clauses)))

(trace all-variables)

(define (atomic-clause? clause)
  (match clause
    (('and . p) #f)
    (('or . p) #f)
    ;; (('not . p) #f)
    (('=> . p) #f)
    (('<=> . p) #f)
    ;; (p (or (not (list? p))
    ;;        (or (negative-clause? p)
    ;;            (= (length p) 1))))
    (_ #t)))

;; (trace atomic-clause?)

(define (literal-clause? clause)
  (or (atomic-clause? clause)
      (and (negative-clause? clause)
           (atomic-clause? (car (clauses clause))))))

;; (trace literal-clause?)

(define (eliminate-implications formula)
  (match formula
    ((? literal-clause?) formula)
    ;; (('=> p . qs) `(or (not ,p) ,@qs))
    ;; (('<=> p . qs)
    ;;  `(and (or ,p (not ,@qs))
    ;;        (or (not ,@qs) ,p)))
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
  (debug formula (eliminate-implications formula))
  (let ((formula (eliminate-implications formula)))
    (match formula
      (('not p) (let ((q (move-not-inwards p)))
                  (if (literal-clause? q) q (->cnf q))))
      (('and . ps)
       `(and ,@(append-map (lambda (p) (conjuncts (->cnf p))) ps)))
      (('or . ps)
       (merge-disjuncts (map ->cnf ps)))
      (p p))))

(trace ->cnf)

(define (tell knowledge-base p)
  (append knowledge-base (clauses (->cnf p))))

(define (tell* knowledge-base . ps)
  (fold-right (lambda (p knowledge-base)
                ;; (debug p (->cnf p))
                (tell knowledge-base (->cnf p)))
              knowledge-base
              ps))

;; (test (->cnf '(and (or B (and C (or M N) F) D) (or W(and P (or Q (and X Y) X A)))))
;;       '(and (or D C B) (or D N M B) (or D F B) (or A X Y Q W) (or A X X Q W) (or P W)))

;; (test '(() t) (simplify '((not s) t) 's))
;; (test '(t) (simplify '((not s) t) '(not s)))
;; (test '(t) (simplify '(s t) 's))
;; (test '(() t) (simplify '(s t) '(not s)))
;; (test '(()) (simplify '((not s) s) 's))
;; (test '(()) (propagate-unit-clauses '((not s) s) '()))
;; (call-with-values (lambda () (propagate-unit-clauses '((not s) s) '()))
;;   (lambda (clauses assignment)
;;     (test clauses '(()))
;;     (test assignment '((not s)))))
;; (test-assert (not (satisfy '(and s (not s)))))
;; (test '((not t) s) (satisfy '(and s (not t))))

;; (debug (atomic-clause? '(x y z)))

;; (let ((knowledge-base '(and)))
;;   (debug
;;    (satisfy
;;     (tell* knowledge-base
;;            's
;;            ;; '(not t)
;;            't
;;            ;; '(not x)
;;            '(=> x y)
;;            '(or (not x) y)))))

;; (debug (literal-clause? '((not w13) s12))
;;        (negative-clause? '((not w13) s12))
;;        (unit-clauses '((not w13) s12))
;;        (literal-clause? (disjunction 't))
;;        (literal-clause? (disjunction '((not w13) s12))))

;; (let ((knowledge-base '(and)))
;;   (debug
;;    (satisfy
;;     (tell* knowledge-base
;;            '(and (not s11)
;;                  (not s21)
;;                  s12
;;                  (not b11)
;;                  b21
;;                  (not b12))
;;            '(=> (not s11) (and (not w11)
;;                                (not w12)
;;                                (not w13)))
;;            '(=> (not s21) (and (not w11)
;;                                (not w21)
;;                                (not w22)
;;                                (not w31)))
;;            '(=> (not s12) (and (not w11)
;;                                (not w12)
;;                                (not w22)
;;                                (not w13)))
;;            '(=> s12 (or w13
;;                         w12
;;                         w22
;;                         w11))))))

;; (debug
;;  (tell* '(and)
;;         '(and (not s11)
;;               (not s21)
;;               s12
;;               (not b11)
;;               b21
;;               (not b12))
;;         '(=> (not s11) (and (not w11)
;;                             (not w12)
;;                             (not w13)))
;;         '(=> (not s21) (and (not w11)
;;                             (not w21)
;;                             (not w22)
;;                             (not w31)))
;;         '(=> (not s12) (and (not w11)
;;                             (not w12)
;;                             (not w22)
;;                             (not w13)))
;;         '(=> s12 (or w13
;;                      w12
;;                      w22
;;                      w11))))

;; (satisfy '(and s (not s)))
;; (satisfy '(and s))
;; (satisfy '(and (or W13 W12 W22 W11) (or S12 (not W11)) (or S12 (not W12))
;;                (or S12 (not W22)) (or S12 (not W13)) (or S21 (not W11))
;;                (or S21 (not W21)) (or S21 (not W22)) (or S21 (not W31))
;;                (or S11 (not W11)) (or S11 (not W12)) (or S11 (not W13)) (not S11)
;;                (not S21) S12 (not B11) B21 (not B12)))

(define (make-knowledge-base) '(and))

(define (ask knowledge-base query)
  (and (satisfy (tell knowledge-base query)) #t))

;; (let ((knowledge-base (tell* (make-knowledge-base)
;;                              '(not b11)
;;                              '(=> (not b11) (and (not p12) (not p21)))
;;                              'b21
;;                              '(=> b21 (or p11 p22 p31)))))
;;   (test knowledge-base
;;         '(and (or (not b21) p11 p22 p31)
;;               b21
;;               (or b11 (not p21))
;;               (or b11 (not p12))
;;               (not b11))))

(let ((knowledge-base (tell* (make-knowledge-base)
                             '(not b11)
                             '(=> (not b11) (and (not p12) (not p21)))

                             ;; '(not s11)
                             ;; '(=> (not s11) (and (not w12) (not w21)))

                             ;; '(not p11)
                             ;; '(not w11)

                             ;; 'b21
                             '(=> b21 (or p11 p22 p31))

                             ;; '(not s21)
                             ;; '(=> (not s21) (and (not w11) (not w22) (not w31)))

                             ;; '(not p21)
                             ;; '(not w21)
                             )))
  (debug (satisfy knowledge-base)
         (ask knowledge-base '(not p11))
         (ask knowledge-base 'p11)))

;; Logical-agents:5 ends here
