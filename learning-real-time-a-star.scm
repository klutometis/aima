#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*4.13][4\.13:1]]

(include "online-navigation.scm")
(use heap)

(define (write-dot-preamble width height title)
 (display "digraph G {")
 (display "node [shape=point];")
 (let ((width-in-inches (/ width 96))
       (height-in-inches (/ height 96)))
   (format #t "graph [fontsize=48, label=\"~a\", ratio=fill, viewport=\"~a,~a\", size=\"~a,~a!\", labelloc=t];"
           title
           (* width-in-inches 72)
           (* height-in-inches 72)
           width-in-inches
           height-in-inches)))

;;; Let's keep track of the goal and visualize a straight-line
;;; heuristic thither in light grey (with the updated distance) for
;;; those states for which we have information.
(define (make-agent-lrta* start next-frame)
  (make-agent
   start
   0
   (let ((result (make-hash-table))
         (cost-estimate (make-hash-table))
         (coordinates (make-hash-table))
         (previous-state #f)
         (previous-action #f)
         (time 0)
         (labels (make-hash-table)))
     (define (lrta*-cost previous-state previous-action state)
       (if state
           (+ (point-distance origin previous-action)
              (hash-table-ref cost-estimate state))
           0))

     ;; This is slow in the sense that we create hash-tables for every
     ;; non-existent entry; or not? Default memoizes, I think.
     (define (result-ref state action)
       (hash-table-ref/default
        (hash-table-ref/default
         result
         state
         (make-hash-table))
        action
        #f))

     (define (update-result! state)
       (hash-table-update!/default
        result
        previous-state
        (lambda (action->state)
          (hash-table-set!
           action->state
           previous-action
           state)
          action->state)
        (make-hash-table)))

     (define (update-cost-estimate!)
       (let ((minimum-cost
              (apply
               min
               (map (lambda (action)
                      (lrta*-cost
                       previous-state
                       action
                       (result-ref previous-state action)))
                    previous-state))))
         (hash-table-set! cost-estimate previous-state minimum-cost)))

     (define (cheapest-action state)
       (let ((actions (make-min-heap)))
         (for-each (lambda (action)
                     (heap-insert! actions
                                   (lrta*-cost state action (result-ref state action))
                                   action))
           state)
         (heap-extremum actions)))

     (define (reset-state!)
       (set! previous-state #f)
       (set! previous-action #f)
       (inc! time))

     (define (update-labels! state)
       (unless (hash-table-exists? labels state)
         (hash-table-set!
          labels
          state
          (gensym))))

     (define (update-coordinates! state)
       (let* ((previous-coordinate
               (hash-table-ref/default
                coordinates
                previous-state
                (make-coordinate origin time)))
              (previous-point
               (coordinate-point previous-coordinate)))
         (if (hash-table-exists? coordinates state)
             (let* ((coordinate (hash-table-ref coordinates state))
                    (point (coordinate-point coordinate)))
               (when (< (coordinate-time coordinate) time)
                 (let ((delta-x
                        (- (point-x point)
                           (+ (point-x previous-point)
                              (point-x previous-action))))
                       (delta-y
                        (- (point-y point)
                           (+ (point-y previous-point)
                              (point-y previous-action)))))
                   (hash-table-walk coordinates
                     (lambda (state old-coordinate)
                       (when (< (coordinate-time old-coordinate) time)
                         (coordinate-time-set! old-coordinate time)
                         (let ((old-point (coordinate-point old-coordinate)))
                           (coordinate-point-set!
                            old-coordinate
                            (make-point (- (point-x old-point) delta-x)
                                        (- (point-y old-point) delta-y))))))))))
             (hash-table-set!
              coordinates
              state
              (make-coordinate
               (make-point (+ (point-x previous-point)
                              (point-x previous-action))
                           (+ (point-y previous-point)
                              (point-y previous-action)))
               time)))))

     (define (write-agent-as-dot state)
       (let ((displayed (make-hash-table))
             (linear-scale (* 5 72)))

         (define (node-display state label)
           (unless (hash-table-exists? displayed state)
             (hash-table-set! displayed state #t)
             (let ((coordinate
                    (hash-table-ref/default coordinates
                                            state
                                            (make-coordinate origin time))))
               (format #t "~a [pos=\"~a,~a\", xlabel=\"~,2f\"];"
                       label
                       (* (point-x (coordinate-point coordinate)) linear-scale)
                       (* (point-y (coordinate-point coordinate)) linear-scale)
                       (hash-table-ref cost-estimate state)))))

         ;; (write-dot-preamble 800 450 "Random walk with error correction")
         (write-dot-preamble 1600 900 "LRTA*")

         (hash-table-walk result
           (lambda (whence whither->action)
             (hash-table-walk whither->action
               (lambda (action whither)
                 (let ((whence-label
                        (hash-table-ref labels whence))
                       (whither-label
                        (hash-table-ref labels whither)))
                   (node-display whence whence-label)
                   (node-display whither whither-label)
                   (format #t "~a -> ~a [color=~a, penwidth=~a];"
                           whence-label
                           whither-label
                           (if (equal? whence previous-state)
                               "orange"
                               "blue")
                           (if (equal? whence previous-state)
                               2
                               1)))))))
         (display "}")))

     (define (write-agent-as-png png state)
       (let ((dot (create-temporary-file ".dot")))
         (with-output-to-file dot
           (lambda ()
             (write-agent-as-dot state)))
         (run (neato -n1 -Tpng -o ,png < ,dot))))

     (lambda (state goal? score)
       (update-labels! state)
       (when previous-state
         (update-coordinates! state))
       (unless (zero? (hash-table-size result))
         (write-agent-as-png (next-frame) state))
       (if goal?
           (begin
             (reset-state!)
             zero-motion)
           (begin
             (unless (hash-table-exists? cost-estimate state)
               ;; We could refine this by providing the Euclidian
               ;; distance, if we have one.
               (hash-table-set! cost-estimate state 0))
             (when previous-state
               (update-result! state)
               (update-cost-estimate!))
             (let ((action (cheapest-action state)))
               (set! previous-action action)
               (set! previous-state state)
               action)))))))

(define (make-agent-random-walk start next-frame)
  (make-agent
   start
   0
   (lambda (points goal? score)
     (if goal?
         zero-motion
         (list-ref points (random (length points)))))))

(simulate-navigation make-agent-lrta*
                     ;; make-agent-random-walk
                     n-nodes: 10
                     n-steps: 100
                     animation-file: "learning-real-time-a-star.avi")

;; 4\.13:1 ends here
