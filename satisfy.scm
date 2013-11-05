#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*Logical%20agents][Logical-agents:5]]

(use combinatorics
     debug
     matchable
     miscmacros
     srfi-1
     srfi-69
     stack
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

(define (wumpus-tell kb tell)
  (let iter-n ((n (n))
               (kb kb))
    (if (negative? n)
        kb
        (let iter-m ((m (m))
                     (kb kb))
          (if (negative? m)
              (iter-n (- n 1) kb)
              (iter-m (- m 1)
                      (tell kb n m)))))))

(define (wumpus-ask kb question)
  (let iter-i ((i (- (n) 1))
               (answers '()))
    (if (negative? i)
        answers
        (let iter-j ((j (- (m) 1))
                     (answers answers))
          (if (negative? j)
              (iter-i (- i 1) answers)
              (iter-j (- j 1)
                      (let ((question (question i j)))
                        (if (ask kb question)
                            (cons question answers)
                            answers))))))))

(define (wumpus-ask-not kb question)
  (let iter-i ((i (- (n) 1))
               (answers '()))
    (if (negative? i)
        answers
        (let iter-j ((j (- (m) 1))
                     (answers answers))
          (if (negative? j)
              (iter-i (- i 1) answers)
              (iter-j (- j 1)
                      (let ((question (question i j)))
                        (if (not (ask kb question))
                            (cons question answers)
                            answers))))))))

(define (wumpus-tell-physics kb)
  (wumpus-tell
   kb
   (lambda (kb i j)
     (tell* kb
            `(<=> ,(var 'breeze i j)
                  (or ,(var 'pit (- i 1) j)
                      ,(var 'pit i (- j 1))
                      ,(var 'pit (+ i 1) j)
                      ,(var 'pit i (+ j 1))))
            `(<=> (not ,(var 'breeze i j))
                  (and (not ,(var 'pit (- i 1) j))
                       (not ,(var 'pit i (- j 1)))
                       (not ,(var 'pit (+ i 1) j))
                       (not ,(var 'pit i (+ j 1)))))
            `(<=> ,(var 'stench i j)
                  (or ,(var 'wumpus (- i 1) j)
                      ,(var 'wumpus i (- j 1))
                      ,(var 'wumpus (+ i 1) j)
                      ,(var 'wumpus i (+ j 1))))
            `(<=> (not ,(var 'stench i j))
                  (and (not ,(var 'wumpus (- i 1) j))
                       (not ,(var 'wumpus i (- j 1)))
                       (not ,(var 'wumpus (+ i 1) j))
                       (not ,(var 'wumpus i (+ j 1)))))))))

(define (wumpi)
  (append-map
   (lambda (i)
     (map (lambda (j)
            (var 'wumpus i j))
          (iota (m))))
   (iota (n))))

;;; Might need to make this time-sensitive: either I haven't detected
;;; a stench yet, or &c.
(define (there-exists-a-wumpus kb)
  (tell kb `(or ,@(wumpi))))

(define (there-is-at-most-one-wumpus kb)
  (unordered-subset-fold
   (lambda (x-y kb) (match x-y ((x y) (tell kb `(or (not ,x) (not ,y))))))
   kb
   (wumpi)
   2))

;;; This version causes an inconsistency: I can't say there's a wumpus
;;; anywhere: don't have enough information. The KB fails trivially.
;; (define (make-wumpus-kb)
;;   (there-is-at-most-one-wumpus
;;    (there-exists-a-wumpus
;;     (wumpus-tell-physics
;;      (make-knowledge-base)))))

(define (make-wumpus-kb)
  (there-is-at-most-one-wumpus
   (wumpus-tell-physics
    (make-knowledge-base))))

(define n (make-parameter 2))
(define m (make-parameter 2)) 

(define (wumpus-tell-location kb t)
  (if (null? (wumpus-ask kb (lambda (i j) (var 'move (- t 1) i j))))
      (wumpus-tell
       kb
       (lambda (kb i j)
         (tell* kb
                `(<=> ,(var 'location t i j)
                      ,(var 'location (- t 1) i j)))))
      (wumpus-tell
       kb
       (lambda (kb i j)
         (tell* kb
                `(<=> ,(var 'location t i j)
                      ,(var 'move (- t 1) i j)))))))

(define (wumpus-tell-ok kb t)
  (wumpus-tell 
   kb
   (lambda (kb i j)
     (tell kb
           `(<=> ,(var 'ok t i j)
                 (and ,(not-var 'pit i j)
                      (not (and ,(var 'wumpus i j)
                                ,(var 'wumpus-alive t)))))))))

(define (wumpus-tell-stench kb t)
  (wumpus-tell
   kb
   (lambda (kb i j)
     (tell kb
           `(<=> (and ,(var 'stench t)
                      ,(var 'location t i j))
                 ,(var 'stench i j))))))

(define (wumpus-tell-breeze kb t)
  (wumpus-tell
   kb
   (lambda (kb i j)
     (tell kb
           `(<=> (and ,(var 'breeze t)
                      ,(var 'location t i j))
                 ,(var 'breeze i j))))))

;; (define (wumpus-ask-ok kb)
;;   )

(define (safe-squares kb t)
  (map (lambda (ok)
         (match (subscripts ok)
           (('ok t i j) (cons i j))))
       (wumpus-ask kb (lambda (i j) (var 'ok t i j)))))

(define (unvisited-squares kb t)
  (let ((unvisited-squares (make-hash-table)))
    (let iter ((t t))
      (unless (negative? t)
          (for-each
              (match-lambda (('location t i j)
                        (hash-table-set! unvisited-squares (cons i j) #t)))
            (map
             subscripts
             (wumpus-ask-not
              kb
              (lambda (i j) (var 'location t i j)))))
          (iter (- t 1))))
    (let iter ((t t))
      (unless (negative? t)
          (for-each
              (match-lambda (('location t i j)
                        (hash-table-delete! unvisited-squares (cons i j))))
            (map
             subscripts
             (wumpus-ask
              kb
              (lambda (i j) (var 'location t i j)))))
          (iter (- t 1))))
    (hash-table-keys unvisited-squares)))

(define (plan-route current goals allowed)
  (match (car goals)
    ((i . j) (var 'move i j))))

(define (make-wumpus-agent)
  (let ((kb (make-parameter
             (tell* (make-wumpus-kb)
                    (var 'location 0 0 0)
                    (var 'arrow 0)
                    (var 'wumpus-alive 0))))
        (t (make-parameter 0))
        (plan (make-parameter '())))
    (lambda (stench breeze glitter scream)
      (kb (wumpus-tell-stench
           (wumpus-tell-breeze
            (wumpus-tell-ok
             (wumpus-tell-location
              (tell* (kb)
                     (if stench
                         (var 'stench (t))
                         (not-var 'stench (t)))
                     (if breeze
                         (var 'breeze (t))
                         (not-var 'breeze (t)))
                     (if glitter
                         (var 'glitter (t))
                         (not-var 'glitter (t)))
                     `(<=> ,(var 'arrow (+ (t) 1))
                           (and ,(var 'arrow (t))
                                ,(not-var 'shoot (t)))))
              (t))
             (t))
            (t))
           (t)))
      (let ((safe-squares (safe-squares (kb) (t))))
        (cond ((ask (kb) (var 'glitter (t)))
               (plan (list (var 'grab (t))
                           (var 'teleport (t) 0 0)
                           (var 'climb (t)))))
              ((null? (plan))
               (let ((unvisited-squares (unvisited-squares (kb) (t))))
                 (debug (lset-intersection equal? unvisited-squares safe-squares))))))
      (values (var 'forward (t)) (kb)))))

(let ((agent (make-wumpus-agent)))
  (call-with-values (lambda () (agent #f #f #f #f))
    (lambda (action kb)
      (debug action
             ;; kb
             (and (satisfy kb) #t)))))

;; Logical-agents:5 ends here
