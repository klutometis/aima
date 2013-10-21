#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*Logical%20agents][Logical-agents:5]]

(use combinatorics
     debug
     matchable
     srfi-1
     test)

(define (satisfy formula)
  (let* ((clauses (conjuncts formula))
         (all-variables (all-variables clauses)))
    (let iter ((clauses clauses)
               (assignment '()))
      (call-with-values (lambda () (propagate-unit-clauses clauses assignment))
        (lambda (clauses assignment)
          (cond ((exists-empty-clause? clauses) #f)
                ;; This is where we'd do some dynamic shit and maybe a
                ;; call-cc.
                ((null? clauses) assignment)
                (else
                 (let ((variable (select-variable all-variables assignment)))
                   (if variable
                       (or (iter (simplify clauses variable)
                                 (cons variable assignment))
                           (iter (simplify clauses (negate variable))
                                 (cons (negate variable) assignment)))
                       assignment)))))))))

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
  (let ((candidates (lset-difference eq? all-variables (variables assignment))))
    (and (not (null? candidates))
         (list-ref candidates (random (length candidates))))))

(define (propagate-unit-clauses clauses assignment)
  (let iter ((clauses clauses)
             (assignment assignment))
    (if (exists-empty-clause? clauses)
        (values clauses assignment)
        (let ((unit-clauses (unit-clauses clauses)))
          (if (null? unit-clauses)
              (values clauses assignment)
              (let ((unit-clause (car unit-clauses)))
                (iter (simplify clauses unit-clause)
                      (cons unit-clause assignment))))))))

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
                                       (('or . ps) ps)
                                       (p p)))
                                    (simplification '()))
                           (if (null? clause)
                               (cons (if (= (length simplification) 1)
                                         (car simplification)
                                         simplification) simplifications)
                               (let* ((term (car clause))
                                      (negative-term? (negative-clause? term)))
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

(define (filter-clauses clauses variable)
  (filter (lambda (clause) (not (memq variable clause))) clauses))

(define (unit-clauses clauses)
  (filter (lambda (clause)
            (or (not (list? clause))
                (= (length clause) 1)
                (negative-clause? clause)))
          clauses))

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
      (if (and (list? clause)
               (not (negative-clause? clause)))
          (map variable clause)
          (list (variable clause)))
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
    ;; (('not . p) #f)
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
           (or (not ,p) ,q)))
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
                (tell knowledge-base (->cnf p)))
              knowledge-base
              ps))

(test (->cnf '(and (or B (and C (or M N) F) D) (or W(and P (or Q (and X Y) X A)))))
      '(and (or D C B) (or D N M B) (or D F B) (or A X Y Q W) (or A X X Q W) (or P W)))

(test '(() t) (simplify '((not s) t) 's))
(test '(t) (simplify '((not s) t) '(not s)))
(test '(t) (simplify '(s t) 's))
(test '(() t) (simplify '(s t) '(not s)))
(test '(()) (simplify '((not s) s) 's))
(test '(()) (propagate-unit-clauses '((not s) s) '()))
(call-with-values (lambda () (propagate-unit-clauses '((not s) s) '()))
  (lambda (clauses assignment)
    (test clauses '(()))
    (test assignment '((not s)))))
(test-assert (not (satisfy '(and s (not s)))))
(test '((not t) s) (satisfy '(and s (not t))))

;;; "a entails b iff (and a (not b)) is unsatisfiable."
(define (ask knowledge-base query)
  (not (satisfy (tell knowledge-base `(not ,query)))))

(define (make-knowledge-base) '(and))

(let ((knowledge-base (tell* (make-knowledge-base)
                             '(not b11)
                             '(=> (not b11) (and (not p12) (not p21)))
                             'b21
                             '(=> b21 (or p11 p22 p31)))))
  (test-assert (not (ask knowledge-base 'b11))))

(let ((kb (tell* (make-knowledge-base)
                 'a)))
  (test-assert (not (ask kb '(not a))))
  (test-assert (satisfy kb)))

(let ((kb (tell* (make-knowledge-base)
                 'b11
                 '(=> b11 (or p01 p10 p12 p21))
                 '(not b00)
                 '(=> (not b00) (and (not p10) (not p01))))))
  (test-assert (not (ask kb 'p01)))
  (test-assert (ask kb '(not p01)))
  (test-assert (satisfy kb))
  (test-assert (not (ask kb '(not p12))))
  (test-assert (not (ask kb 'p12))))

(define (var . name-subscripts)
  (string->symbol (string-join (map ->string name-subscripts) ",")))

(define (not-var . name-subscripts)
  `(not ,(apply var name-subscripts)))

(define (subscripts symbol)
  (let ((name-subscripts (string-split (symbol->string symbol) ",")))
    (cons (string->symbol (car name-subscripts)) (map string->number (cdr name-subscripts)))))

(define (wumpus-tell kb n m tell)
  (let iter-n ((n n)
               (kb kb))
    (if (negative? n)
        kb
        (let iter-m ((m m)
                     (kb kb))
          (if (negative? m)
              (iter-n (- n 1) kb)
              (iter-m (- m 1)
                      (tell kb n m)))))))

(define (wumpus-tell-physics kb n m)
  (wumpus-tell kb
               n
               m
               (lambda (kb n m)
                 (tell* kb
                        `(<=> ,(var 'breeze n m)
                              (or ,(var 'pit (- n 1) m)
                                  ,(var 'pit n (- m 1))
                                  ,(var 'pit (+ n 1) m)
                                  ,(var 'pit n (+ m 1))))
                        `(<=> (not ,(var 'breeze n m))
                              (and (not ,(var 'pit (- n 1) m))
                                   (not ,(var 'pit n (- m 1)))
                                   (not ,(var 'pit (+ n 1) m))
                                   (not ,(var 'pit n (+ m 1)))))
                        `(<=> ,(var 'stench n m)
                              (or ,(var 'wumpus (- n 1) m)
                                  ,(var 'wumpus n (- m 1))
                                  ,(var 'wumpus (+ n 1) m)
                                  ,(var 'wumpus n (+ m 1))))
                        `(<=> (not ,(var 'stench n m))
                              (and (not ,(var 'wumpus (- n 1) m))
                                   (not ,(var 'wumpus n (- m 1)))
                                   (not ,(var 'wumpus (+ n 1) m))
                                   (not ,(var 'wumpus n (+ m 1)))))))))

(define (wumpi n m)
  (append-map
   (lambda (i)
     (map (lambda (j)
            (var 'wumpus i j))
          (iota m)))
   (iota n)))

(define (there-exists-a-wumpus kb n m)
  (tell kb `(or ,@(wumpi n m))))

(define (there-is-only-one-wumpus kb n m)
  (unordered-subset-fold
   (lambda (x-y kb) (match x-y ((x y) (tell kb `(or (not ,x) (not ,y))))))
   kb
   (wumpi n m)
   2))

(define (make-wumpus-kb n m)
  (there-is-only-one-wumpus
   (there-exists-a-wumpus
    (wumpus-tell-physics (make-knowledge-base) n m)
    n m)
   n m))

(define (wumpus-tell-location kb n m t)
  (wumpus-tell kb
               n
               m
               (lambda (kb n m)
                 (tell* kb
                        `(<=> ,(var 'location (+ t 1) n m)
                              (or (and ,(var 'location t n m)
                                       (or ,(not-var 'forward t)
                                           ,(not-var 'bump (+ t 1))))
                                  (and ,(var 'location t n (+ m 1))
                                       (or ,(var 'south 0)
                                           ,(var 'forward 0)))
                                  (and ,(var 'location t (+ n 1) m)
                                       (or ,(var 'west 0)
                                           ,(var 'forward 0)))
                                  (and ,(var 'location t n (- m 1))
                                       (or ,(var 'north 0)
                                           ,(var 'forward 0)))
                                  (and ,(var 'location t (- n 1) m)
                                       (or ,(var 'east 0)
                                           ,(var 'forward 0)))))))))

;; (let ((kb (make-wumpus-kb 3 3)))
;;   (debug kb 
;;          (ask (tell kb `(not ,(var 'breeze 1 1)))
;;               `(not ,(var 'pit 0 1)))
;;          (ask (tell kb (var 'breeze 1 1))
;;               (not-var 'pit 2 1))
;;          (ask (tell kb (var 'breeze 1 1))
;;               (var 'pit 2 1))))

(define (wumpus-tell-ok kb n m t)
  (wumpus-tell kb
               n
               m
               (lambda (kb n m)
                 (tell kb
                       `(<=> ,(var 'ok t n m)
                             (and ,(not-var 'pit n m)
                                  (not (and ,(var 'wumpus n m)
                                            ,(var 'wumpus-alive t)))))))))

(define (wumpus-tell-stench kb n m t)
  (wumpus-tell kb
               n
               m
               (lambda (kb n m)
                 (tell kb
                       `(<=> (and ,(var 'stench t)
                                  ,(var 'location t n m))
                             ,(var 'stench n m))))))

(define (wumpus-tell-breeze kb n m t)
  (wumpus-tell kb
               n
               m
               (lambda (kb n m)
                 (tell kb
                       `(<=> (and ,(var 'breeze t)
                                  ,(var 'location t n m))
                             ,(var 'breeze n m))))))

(define (make-wumpus-agent n m)
  (let ((kb (make-parameter
             (tell* (make-wumpus-kb n m)
                    (var 'north 0)
                    (var 'location 0 0 0)
                    (var 'arrow 0)
                    (var 'wumpus-alive 0))))
        (time (make-parameter 0))
        (plan (make-parameter '())))
    (lambda (stench breeze glitter bump scream)
      (kb (wumpus-tell-ok
           (wumpus-tell-location
            (tell* (kb)
                   (if stench
                       (var 'stench (time))
                       (not-var 'stench (time)))
                   (if breeze
                       (var 'breeze (time))
                       (not-var 'breeze (time)))
                   (if glitter
                       (var 'glitter (time))
                       (not-var 'glitter (time)))
                   `(<=> ,(var 'arrow (+ (time) 1))
                         (and ,(var 'arrow (time))
                              ,(not-var 'shoot (time)))))
            n
            m
            (time))
           n
           m
           (time)))
      (debug (ask (kb) (var 'ok 0 0 0)))
      (debug (ask (kb) (var 'ok 0 0 1)))
      (debug (ask (kb) (var 'ok 0 1 0)))
      (debug (ask (kb) (var 'ok 0 1 1)))
      (debug (ask (kb) (var 'pit 0 1)))
      (debug (ask (kb) (var 'pit 1 0)))
      (debug (ask (kb) (var 'wumpus 0 1)))
      (debug (ask (kb) (var 'wumpus 1 0)))
      (debug (ask (kb) (var 'wumpus-alive 0)))
      (debug (ask (kb) (not-var 'pit 1 0)))
      (debug (ask (kb) (not-var 'pit 0 1)))
      (values (var 'forward (time)) (kb)))))

(let ((agent (make-wumpus-agent 2 2)))
  (call-with-values (lambda () (agent #f #f #f #f #f))
    (lambda (action kb)
      (debug action kb
             ))))

;; Logical-agents:5 ends here
