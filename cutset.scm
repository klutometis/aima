#!/usr/bin/env chicken-scheme

(use aima-csp
     debug
     files
     list-utils
     shell
     srfi-95)

(define (australia)
  (alist->hash-table '((wa nt sa)
                       (nt wa sa)
                       (sa wa nt q nsw v)
                       (q nt sa nsw)
                       (nsw q sa v)
                       (v nsw sa)
                       (t))))

(define (visited? visited node)
  (hash-table-ref/default visited node #f))

(define (visited-set! visited node)
  (hash-table-set! visited node #t))

(define make-visited make-hash-table)

(define (non-trivial-cycle? graph visited root)
  (call/cc
   (lambda (return)
     (let visit ((parent root)
                 (path '()))
       (unless (visited? visited parent)
         (visited-set! visited parent)
         (for-each (lambda (child)
                     (when (and (visited? visited child)
                                (not (null? path))
                                (not (eq? child (car path))))
                       (return #t))
                     (visit child (cons parent path)))
           (graph-children graph parent))))
     #f)))

(define (sort-nodes-by-degree graph nodes)
  (sort nodes > (lambda (node) (length (hash-table-ref graph node)))))

(define graph-nodes hash-table-keys)

(define (graph-children graph parent)
  (hash-table-ref/default graph parent '()))

;;; Assuming that the graph is rigorously bidirectional.
(define (graph-remove! graph whence)
  (let ((whithers (graph-children graph whence)))
    (for-each (lambda (whither)
                (hash-table-update! graph whither (lambda (whences) (delete whence whences))))
      whithers))
  (hash-table-delete! graph whence))

(define (cutset graph)
  (let ((graph (hash-table-copy graph)))
    (let iter ((nodes (sort-nodes-by-degree graph (graph-nodes graph)))
               (cutset '())
               (visited (make-visited)))
      (if (null? nodes)
          (values cutset graph)
          (let ((node (car nodes)))
            (if (non-trivial-cycle? graph visited node)
                (begin
                  (graph-remove! graph node)
                  (iter (cdr nodes) (cons node cutset) (make-visited)))
                (iter (cdr nodes) cutset visited)))))))

(define (subgraph graph nodes)
  (let ((subgraph (make-hash-table)))
    (for-each
        (lambda (node)
          (hash-table-set!
           subgraph
           node
           (lset-intersection eq? nodes (hash-table-ref graph node))))
      nodes)
    subgraph))

(define (compare-graph-and-complement-as-png graph subgraph complement-graph)
  (let ((nodes (hash-table-keys graph))
        (complement-nodes (hash-table-keys complement-graph))
        (graph-png (create-temporary-file ".png"))
        (complement-png (create-temporary-file ".png"))
        (comparison-png (create-temporary-file ".png")))
    (write-map-as-png
     graph
     (alist->hash-table
      (zip-alist nodes
                 (map (lambda (node)
                        (if (hash-table-exists? subgraph node)
                            'yellow
                            'blue))
                      nodes)))
     graph-png)
    (write-map-as-png
     complement-graph
     (alist->hash-table
      (zip-alist complement-nodes
                 (make-list (length complement-nodes) 'blue)))
     complement-png)
    (run (convert ,graph-png ,complement-png -append ,comparison-png "&&"
                  sxiv ,comparison-png)))  )

(define (make-csp-from-map map)
  (let ((domains (make-hash-table))
        (constraints (make-hash-table)))
    (set-domains! domains
                  (hash-table-keys map)
                  '(red green blue yellow))
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
    (make-csp domains constraints map)))

(define (random-element list)
  (list-ref list (random (length list))))

(let* ((graph (random-map 40))
       (csp (make-csp-from-map graph)))
  (call-with-values (lambda () (cutset graph))
    (lambda (cutset complement-graph)
      (let ((complement-csp (make-csp-from-map complement-graph)))
        (let* ((subgraph (subgraph graph cutset))
               (subcsp (make-csp-from-map subgraph))
               (subsolution (backtracking-search subcsp)))
          ;; (compare-graph-and-complement-as-png graph subgraph complement-graph)       
          ;; (display-map-as-png subgraph (backtracking-search subcsp))
          (call/cc
           (lambda (return)      
             (for-each
                 (lambda (subsolution)
                   (let ((complement-csp (csp-copy complement-csp)))
                     (for-each (lambda (complement-node)
                                 (let ((neighbors (lset-intersection
                                                   eq?
                                                   (hash-table-ref graph complement-node)
                                                   cutset)))
                                   (for-each
                                       (lambda (neighbor)
                                         (let ((constraint (hash-table-ref
                                                            (csp-constraints csp)
                                                            (cons complement-node neighbor)))
                                               (neighbor-value
                                                (hash-table-ref subsolution neighbor)))
                                           (let ((domain (filter (lambda (value)
                                                                   (debug value
                                                                          neighbor-value
                                                                          (constraint value neighbor-value))
                                                                   (constraint value neighbor-value))
                                                                 (hash-table-ref
                                                                  (csp-domains complement-csp)
                                                                  complement-node))))
                                             (hash-table-set! (csp-domains complement-csp)
                                                              complement-node
                                                              domain))))
                                     neighbors)))
                       (graph-nodes complement-graph))
                     ;; (debug (length cutset)
                     ;;        (hash-table->alist graph)
                     ;;        (hash-table->alist subgraph))
                     (let ((solution (backtracking-search complement-csp)))
                       ;; (display-map-as-png
                       ;;  complement-graph
                       ;;  (alist->hash-table
                       ;;   (hash-table-map (csp-domains complement-csp)
                       ;;                   (lambda (variable domain)
                       ;;                     (cons variable (if (null? domain)
                       ;;                                        'black
                       ;;                                        (if (= (length domain) 1)
                       ;;                                            (car domain)
                       ;;                                            ;; (random-element domain)
                       ;;                                            'white)))))))
                       (debug solution (hash-table->alist (csp-domains complement-csp)))
                       (when (success? solution)
                         (display-map-as-png graph (hash-table-merge subsolution (backtracking-search complement-csp)))
                         (return solution))))

                   ;; (display-map-as-png graph (hash-table-merge subsolution (backtracking-search complement-csp)))
                   )
               (backtracking-enumeration 10 subcsp))))
           ;; (display-map-as-png complement-graph
           ;;                     (backtracking-search (make-csp-from-map complement-graph)))
           )))))
