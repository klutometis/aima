#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*6.7][6\.7:7]]

(use debug
     ;; expand-full
     list-utils
     symbol-utils)

(use data-structures
     define-record-and-printer
     matchable
     srfi-1
     srfi-69
     srfi-95)

(import-for-syntax matchable symbol-utils)

(import-for-syntax symbol-utils)

(include "~/prg/scm/aima-chicken/aima-csp-core.scm")

(define-for-syntax (scope-order scope)
  (sort scope symbol-printname<?))

(define (scope-order scope)
  (sort scope symbol-printname<?))

(define-syntax constraint-lambda
  (lambda (expression rename compare)
    (match expression
      ((_
        scope
        body)
       (let ((scope (if (list? scope)
                        (scope-order scope)
                        scope)))
         `(lambda ,scope ,body))))))

;; (define-record-and-printer node
;;   neighbors
;;   constraint)

;; (define (constraint-update! node scope constraint)
;;   (if (null? (cdr scope))
;;       (node-constraint-set! node constraint)
;;       (let ((variable (car scope)))))
;;   (let ((node (hash-table-ref constraints))))
;;   (let iter ((scope scope)
;;              (node (hash-table-ref constraints
;;                                    constraint
;;                                    (lambda () (make-node (make-hash-table) #f)))))
;;     (if (null? scope)
;;         (node-datum-set! node constraint)
;;         (let* ((variable (car scope))
;;                (next-node (hash-table-ref (node-neighbors node) variable (lambda () (make-node (make-hash-table) #f)))))
;;           (begin
;;             (hash-table-set! (node-neighbors node) variable next-node)
;;             (iter (cdr scope)
;;                   next-node))))))

(define-syntax constraint-set!
  (lambda (expression rename compare)
    (match expression
      ((_
        constraints
        scope
        body)
       `(hash-table-set! ,constraints
                         (quote ,(scope-order scope))
                         (constraint-lambda ,scope ,body))
       ;; `(constraint-update! ,constraints (quote ,(scope-order scope)) (constraint-lambda ,scope ,body))
       ;; (let ((constraint (constraint-lambda scope body)))
       ;;   `(hash-table-update!/default
       ;;     constraints
       ;;     (lambda (pre-constraint)
       ;;       (if pre-constraint
       ;;           (constraint-lambda scope (and ))))
       ;;     #f))
       ))))

(define-syntax constraint-set!/lambda
  (lambda (expression rename compare)
    (match expression
      ((_
        constraints
        scope
        lambda)
       `(hash-table-set! ,constraints
                         (quote ,(scope-order scope))
                         ,lambda)
       ;; `(constraint-update! ,constraints (quote ,(scope-order scope)) ,lambda)
       ;; (let ((constraint (constraint-lambda scope body)))
       ;;   `(hash-table-update!/default
       ;;     constraints
       ;;     (lambda (pre-constraint)
       ;;       (if pre-constraint
       ;;           (constraint-lambda scope (and ))))
       ;;     #f))
       ))))

(define (neighbors-from-constraints constraints)
  (let ((neighbors (make-hash-table)))
    (for-each (lambda (neighborhood)
                (for-each (lambda (neighbor)
                            (hash-table-update!/default
                             neighbors
                             neighbor
                             (lambda (neighbors)
                               (lset-union eq? neighbors (delete neighbor neighborhood)))
                             '()))
                  neighborhood))
      (hash-table-keys constraints))
    neighbors))

(define (n-ary-consistent? variable value assignment csp)
  ;; Use the default case when there are no neighbors.
  (let* ((neighbors (hash-table-ref/default (csp-neighbors csp) variable '()))
         (assigned-neighbors (filter (lambda (neighbor) (assigned? (hash-table-ref assignment neighbor)))
                                     neighbors))
         (assignment (alist->hash-table (alist-cons variable
                                                    value
                                                    (zip-alist assigned-neighbors
                                                               (map (lambda (neighbor)
                                                                      (hash-table-ref assignment neighbor))
                                                                    assigned-neighbors)))))
         (variables (hash-table-keys assignment)))
    (let ((constraints (csp-constraints csp)))
      (let iter ((scopes (hash-table-keys constraints)))
        (if (null? scopes)
            #t
            (let ((scope (car scopes)))
              (if (applicable? scope variables)
                  (let ((applicable-variables (lset-intersection eq? scope variables))
                        (constraint (hash-table-ref constraints scope)))
                    (if (constraint-apply constraint applicable-variables assignment)
                        (iter (cdr scopes))
                        #f))
                  (iter (cdr scopes)))))))))

(define (constraint-apply constraint variables assignment)
  (let ((values
         (map (lambda (variable) (hash-table-ref assignment variable)) variables)))
    (apply constraint values)))

(define (applicable? scope variables)
  (lset<= eq? scope variables))

(define alldiff (constraint-lambda x (equal? x (delete-duplicates x))))

(define english-red (constraint-lambda (n c) (xor (and (eq? n 'english)
                                                  (eq? c 'red))
                                             (and (neq? n 'english)
                                                  (neq? c 'red)))))

(define (backtracking-search/domains+constraints domains constraints)
  (let* ((neighbors (neighbors-from-constraints constraints))
         (csp (make-csp domains constraints neighbors)))
    (backtracking-search csp)))

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
  (constraint-set!/lambda constraints (n1 n2 n3 n4 n5) alldiff)
  (constraint-set!/lambda constraints (d1 d2 d3 d4 d5) alldiff)
  (constraint-set!/lambda constraints (c1 c2 c3 c4 c5) alldiff)
  (constraint-set!/lambda constraints (s1 s2 s3 s4 s5) alldiff)
  (constraint-set!/lambda constraints (p1 p2 p3 p4 p5) alldiff)

  (constraint-set!/lambda constraints (n1 c1) english-red)
  (constraint-set!/lambda constraints (n2 c2) english-red)
  (constraint-set!/lambda constraints (n3 c3) english-red)
  (constraint-set!/lambda constraints (n4 c4) english-red)
  (constraint-set!/lambda constraints (n5 c5) english-red)

  (parameterize ((consistent? n-ary-consistent?)
                 (inference (lambda x (make-hash-table))))
    (let ((result (backtracking-search/domains+constraints domains constraints)))
      (unless (failure? result)
        (debug (hash-table->alist result))))))

;; 6\.7:7 ends here
