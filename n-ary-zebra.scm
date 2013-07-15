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

;; (include "~/prg/scm/aima-chicken/aima-csp-core.scm")

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

(define (consistent? variable value assignment csp)
  ;; Use the default case when there are no neighbors.
  (let* ((neighbors (hash-table-ref/default (csp-neighbors csp) variable '()))
         (assigned-neighbors (filter (lambda (neighbor) (assigned? (hash-table-ref assignment neighbor)))
                                     neighbors)))
    (let ((results (map (lambda (neighbor) ((hash-table-ref (csp-constraints csp) (cons variable neighbor))
                                       value
                                       (hash-table-ref assignment neighbor)))
                        assigned-neighbors)))
      ;; (debug variable value neighbors assigned-neighbors results)
      (every values results))))

(let ((domains (make-hash-table))
      (constraints (make-hash-table)))
  ;; (set-domains! domains '(a b c) '(1 2 3))
  (debug (expand '(constraint-set! constraints (b c a) (and (> b a) (> c b)))))
  (constraint-set! constraints (b c a) (and (> b a) (> c b)))
  (let ((alldiff (constraint-lambda x (equal? x (delete-duplicates x)))))
    (constraint-set!/lambda constraints (a b) alldiff))
  (debug (hash-table->alist constraints))
  (debug (hash-table->alist (neighbors-from-constraints constraints)))
  (let ((variable-values (zip-alist '(b c a) '(2 3 1))))
    (debug
           (apply
            (hash-table-ref constraints (scope-order '(b c a)))
            (map cdr (sort variable-values symbol-printname<? car)))))
  (let ((variable-values (zip-alist '(b a) '(3 3))))
    (debug
           (apply
            (hash-table-ref constraints (scope-order '(b a)))
            (map cdr (sort variable-values symbol-printname<? car)))))
  (debug ((constraint-lambda (b c a) (and (> b a) (> c b))) 1 2 3))
  )

;; 6\.7:7 ends here
