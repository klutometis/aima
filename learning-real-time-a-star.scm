#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*4.13][4\.13:1]]

(include "online-navigation.scm")

;;; Let's keep track of the goal and visualize a straight-line
;;; heuristic thither in light grey (with the updated distance) for
;;; those states for which we have information.
(define (make-agent-lrta* start next-frame)
  (make-agent
   start
   0
   (let ((result (make-hash-table))
         (cost-estimate (make-hash-table))
         (previous-state #f)
         (previous-action #f))
     (lambda (state goal? score)
       (if goal?
           zero-motion
           (begin
             (unless (hash-table-exists? cost-estimate state)
               (hash-table-set! cost-estimate))))))))

(simulate-navigation make-agent-lrta*)

;; 4\.13:1 ends here
