#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*6.7][6\.7:2]]

(use debug
     matchable)

(import-for-syntax matchable)

(use define-record-and-printer
     srfi-1
     srfi-95
     list-utils
     alist-lib)

(include "~/prg/scm/aima-chicken/aima-csp-core.scm")

(define (neighbors-from-constraints constraints)
  (let ((neighbors (make-hash-table)))
    (for-each (match-lambda ((x . y)
                        (hash-table-update!/default
                         neighbors
                         x
                         (lambda (Y) (cons y Y))
                         '())))
      (hash-table-keys constraints))
    neighbors))

(define (set-domains! domains variables domain)
  (for-each (lambda (variable) (hash-table-set! domains variable domain))
    variables))

(define-syntax xor
  (lambda (expression rename compare)
    (match expression
      ((_ x y)
       (let ((%or (rename 'or))
             (%and (rename 'and))
             (%not (rename 'not)))
         `(,%and (,%or ,x ,y)
                 (,%not (,%and ,x ,y))))))))

(define (set-pairwise-constraints! constraints X Y relation) 
  R(for-each
      (lambda (x)
        (for-each
            (lambda (y)
              ;; Or do we want to merge these with some binary
              ;; operation?
              (hash-table-set! constraints (cons x y) relation))
          (delete x Y)))
    X))

(define (set-bidirectional-constraint! constraints x y constraint-x constraint-y)
  (hash-table-update!/default constraints
                              (cons x y)
                              (lambda (constraints-x)
                                (lambda (x y) (and (constraint-x x y)
                                              (constraints-x x y))))
                              (lambda (x y) #t))
  (hash-table-update!/default constraints
                              (cons y x)
                              (lambda (constraints-y)
                                (lambda (y x) (and (constraint-y y x)
                                              (constraints-y y x))))
                              (lambda (x y) #t)))

(define (set-pairwise-bidirectional-constraints! constraints X Y constraint-x constraint-y)
  (for-each
      (lambda (x y)
        (set-bidirectional-constraint! constraints x y constraint-x constraint-y))
    X
    Y))

(define (set-alldiff-constraints! constraints variables)
  (set-pairwise-constraints! constraints variables variables neq?))

(let ((domains (make-hash-table))
      (constraints (make-hash-table)))
  (set-domains! domains
                '(n1)
                '(norwegian))
  (set-domains! domains
                '(n2 n3 n4 n5)
                '(english spanish japanese ukrainian))
  (set-domains! domains
                '(p1 p2 p3 p4 p5)
                '(dog zebra snail fox horse))
  (set-domains! domains
                '(s1 s2 s3 s4 s5)
                '(smarties snickers milky-way kit-kat hershey))
  (set-domains! domains
                '(c1 c2 c3 c4 c5)
                '(blue yellow ivory red green))
  (set-domains! domains
                '(d1 d2 d4 d5) 
                '(water coffee orange-juice tea))
  (set-domains! domains
                '(d3) 
                '(milk))
  (set-alldiff-constraints! constraints '(n1 n2 n3 n4 n5))
  (set-alldiff-constraints! constraints '(d1 d2 d3 d4 d5))
  (set-alldiff-constraints! constraints '(c1 c2 c3 c4 c5))
  (set-alldiff-constraints! constraints '(s1 s2 s3 s4 s5))
  (set-alldiff-constraints! constraints '(p1 p2 p3 p4 p5))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(n1 n2 n3 n4 n5)
   '(c1 c2 c3 c4 c5)
   (lambda (n c)
     (xor (and (eq? n 'english)
               (eq? c 'red))
          (and (neq? n 'english)
               (neq? c 'red))))
   (lambda (c n)
     (xor (and (eq? n 'english)
               (eq? c 'red))
          (and (neq? n 'english)
               (neq? c 'red)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(n1 n2 n3 n4 n5)
   '(p1 p2 p3 p4 p5)
   (lambda (n p)
     (xor (and (eq? n 'spanish)
               (eq? p 'dog))
          (and (neq? n 'spanish)
               (neq? p 'dog))))
   (lambda (p n)
     (xor (and (eq? n 'spanish)
               (eq? p 'dog))
          (and (neq? n 'spanish)
               (neq? p 'dog)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(c1 c1 c1 c2 c2 c3 c3 c4 c4 c5 c5 c5)
   '(c3 c4 c5 c4 c5 c1 c5 c1 c2 c1 c2 c3)
   (lambda (c1 c2)
     (not (or (and (eq? c1 'ivory)
                   (eq? c2 'green))
              (and (eq? c2 'ivory)
                   (eq? c1 'green)))))
   (lambda (c2 c1)
     (not (or (and (eq? c1 'ivory)
                   (eq? c2 'green))
              (and (eq? c2 'ivory)
                   (eq? c1 'green))))))  
  (set-pairwise-bidirectional-constraints!
   constraints
   '(c1 c2 c3 c4)
   '(c2 c3 c4 c5)
   (lambda (left right)
     (if (and (or (eq? right 'ivory)
                  (eq? left 'ivory))
              (or (eq? right 'green)
                  (eq? left 'green)))
         (and (eq? left 'ivory)
              (eq? right 'green))
         #t))
   (lambda (right left)
     (if (and (or (eq? right 'ivory)
                  (eq? left 'ivory))
              (or (eq? right 'green)
                  (eq? left 'green)))
         (and (eq? left 'ivory)
              (eq? right 'green))
         #t)))
  ;; (set-pairwise-bidirectional-constraints!
  ;;  constraints
  ;;  '(s1 s2 s2 s3 s3 s4 s4 s5)
  ;;  '(p2 p1 p3 p2 p4 p3 p5 p4)
  ;;  (lambda (s p)
  ;;    (not (and (eq? s 'hershey)
  ;;              (neq? p 'fox))))
  ;;  (lambda (p s)
  ;;    (not (and (eq? s 'hershey)
  ;;              (neq? p 'fox)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(s1 s1 s1 s1 s2 s2 s2 s3 s3 s3 s4 s4 s4 s5 s5 s5 s5)
   '(p1 p3 p4 p5 p2 p4 p5 p3 p1 p5 p4 p1 p2 p5 p1 p2 p3)
   (lambda (s p)
     (not (or (and (eq? s 'hershey)
                   (eq? p 'fox))
              (and (eq? s 'kit-kat)
                   (eq? p 'horse)))))
   (lambda (p s)
     (not (or (and (eq? s 'hershey)
                   (eq? p 'fox))
              (and (eq? s 'kit-kat)
                   (eq? p 'horse))))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(s1 s2 s3 s4 s5)
   '(c1 c2 c3 c4 c5)
   (lambda (s c)
     (xor (and (eq? s 'kit-kat)
               (eq? c 'yellow))
          (and (neq? s 'kit-kat)
               (neq? c 'yellow))))
   (lambda (c s)
     (xor (and (eq? s 'kit-kat)
               (eq? c 'yellow))
          (and (neq? s 'kit-kat)
               (neq? c 'yellow)))))
  ;; (set-pairwise-bidirectional-constraints!
  ;;  constraints
  ;;  '(n1 n2 n2 n3 n3 n4 n4 n5)
  ;;  '(c2 c1 c3 c2 c4 c3 c5 c4)
  ;;  (lambda (n c)
  ;;    (not (and (eq? n 'norwegian)
  ;;              (neq? c 'blue))))
  ;;  (lambda (c n)
  ;;    (not (and (eq? c 'blue)
  ;;              (neq? n 'norwegian)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(n1 n1 n1 n1 n2 n2 n2 n3 n3 n3 n4 n4 n4 n5 n5 n5 n5)
   '(c1 c3 c4 c5 c2 c4 c5 c3 c1 c5 c4 c1 c2 c5 c1 c2 c3)
   (lambda (n c)
     (not (and (eq? n 'norwegian)
               (eq? c 'blue))))
   (lambda (c n)
     (not (and (eq? c 'blue)
               (eq? n 'norwegian)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(s1 s2 s3 s4 s5)
   '(p1 p2 p3 p4 p5)
   (lambda (s p)
     (xor (and (eq? s 'smarties)
               (eq? p 'snail))
          (and (neq? s 'smarties)
               (neq? p 'snail))))
   (lambda (p s)
     (xor (and (eq? s 'smarties)
               (eq? p 'snail))
          (and (neq? s 'smarties)
               (neq? p 'snail)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(s1 s2 s3 s4 s5)
   '(d1 d2 d3 d4 d5)
   (lambda (s d)
     (xor (and (eq? s 'snickers)
               (eq? d 'orange-juice))
          (and (neq? s 'snickers)
               (neq? d 'orange-juice))))
   (lambda (d s)
     (xor (and (eq? s 'snickers)
               (eq? d 'orange-juice))
          (and (neq? s 'snickers)
               (neq? d 'orange-juice)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(n1 n2 n3 n4 n5)
   '(d1 d2 d3 d4 d5)
   (lambda (n d)
     (xor (and (eq? n 'ukrainian)
               (eq? d 'tea))
          (and (neq? n 'ukrainian)
               (neq? d 'tea))))
   (lambda (d n)
     (xor (and (eq? n 'ukrainian)
               (eq? d 'tea))
          (and (neq? n 'ukrainian)
               (neq? d 'tea)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(n1 n2 n3 n4 n5)
   '(s1 s2 s3 s4 s5)
   (lambda (n s)
     (xor (and (eq? n 'japanese)
               (eq? s 'milky-way))
          (and (neq? n 'japanese)
               (neq? s 'milky-way))))
   (lambda (s n)
     (xor (and (eq? n 'japanese)
               (eq? s 'milky-way))
          (and (neq? n 'japanese)
               (neq? s 'milky-way)))))
  ;; (set-pairwise-bidirectional-constraints!
  ;;  constraints
  ;;  '(s1 s1 s1 s2 s2 s3 s3 s4 s4)
  ;;  '(p3 p4 p5 p4 p5 p1 p5 p1 p2)
  ;;  (lambda (s p)
  ;;    (not (and (eq? s 'hershey)
  ;;              (eq? p 'fox))))
  ;;  (lambda (p s)
  ;;    (not (and (eq? s 'hershey)
  ;;              (eq? p 'fox)))))
  ;; (set-pairwise-bidirectional-constraints!
  ;;  constraints
  ;;  '(s1 s2 s2 s3 s3 s4 s4 s5)
  ;;  '(p2 p1 p3 p2 p4 p3 p5 p4)
  ;;  (lambda (s p)
  ;;    (not (and (eq? s 'kit-kat)
  ;;              (neq? p 'horse))))
  ;;  (lambda (p s)
  ;;    (not (and (eq? p 'horse)
  ;;              (neq? s 'kit-kat)))))
  (set-pairwise-bidirectional-constraints!
   constraints
   '(d1 d2 d3 d4 d5)
   '(c1 c2 c3 c4 c5)
   (lambda (d c)
     (xor (and (eq? d 'coffee)
               (eq? c 'green))
          (and (neq? d 'coffee)
               (neq? c 'green))))
   (lambda (c d)
     (xor (and (eq? d 'coffee)
               (eq? c 'green))
          (and (neq? d 'coffee)
               (neq? c 'green)))))
  (let* ((neighbors (neighbors-from-constraints constraints))
         (csp (make-csp domains constraints neighbors))
         (result (backtracking-search csp)))
    ;; (debug (hash-table->alist neighbors))
    ;; (debug (hash-table->alist constraints))
    (unless (failure? result)
      (debug (sort (hash-table->alist (backtracking-search csp))
                   string<?
                   (compose symbol->string car))))))

;; 6\.7:2 ends here
