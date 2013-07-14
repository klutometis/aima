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
       ;; (let ((constraint (constraint-lambda scope body)))
       ;;   `(hash-table-update!/default
       ;;     constraints
       ;;     (lambda (pre-constraint)
       ;;       (if pre-constraint
       ;;           (constraint-lambda scope (and ))))
       ;;     #f))
       ))))

(let ((domains (make-hash-table))
      (constraints (make-hash-table)))
  ;; (set-domains! domains '(a b c) '(1 2 3))
  (debug (expand '(constraint-set! constraints (b c a) (and (> b a) (> c b)))))
  (constraint-set! constraints (b c a) (and (> b a) (> c b)))
  (let ((alldiff (constraint-lambda x (list? x))))
    (constraint-set! constraints (a b) alldiff))
  (debug (hash-table->alist constraints))
  (let ((variable-values (zip-alist '(b c a) '(2 3 1))))
    (debug (hash-table->alist constraints)
           (apply
            (hash-table-ref constraints (scope-order '(b c a)))
            (map cdr (sort variable-values string<? (compose symbol->string car))))
           (apply
            (hash-table-ref constraints (scope-order '(b a)))
            (map cdr (sort variable-values string<? (compose symbol->string car))))))
  (debug ((constraint-lambda (b c a) (and (> b a) (> c b))) 1 2 3))
  )

;; 6\.7:7 ends here
