#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*4.13][4\.13:1]]

(include "online-navigation.scm")

(define (make-agent-lrta* start next-frame)
  (make-agent
   start
   0
   (lambda (state goal? score)
     (car state))))

(simulate-navigation make-agent-lrta*)

;; 4\.13:1 ends here
