#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*Non-determinism%20and%20random-walk][Non-determinism-and-random-walk:1]]

(include "online-navigation.scm")

(use heap)

(define (make-agent-random-walk start next-frame)
  (make-agent
   start
   0
   (let ((state->action->states (make-hash-table))
         (state->state->actions (make-hash-table))
         (previous-state #f)
         (previous-action #f)
         (expectations 0)
         (met-expectations 0)
         (expected-states (make-stack)))
     (lambda (state goal? score)
       (if (and previous-action previous-state)
           (begin
             (hash-table-update!
              state->action->states
              previous-state
              (lambda (action->states)
                (hash-table-update!
                 action->states
                 previous-action
                 (lambda (states)
                   (if (heap-member? states state)
                       (heap-change-key! states
                                         state
                                         (add1 (heap-key states state)))
                       (heap-insert! states
                                     1
                                     state))
                   states)
                 (lambda () (make-max-heap)))
                action->states)
              ;; Too bad we don't have multi-dimensional hash-tables.
              (lambda () (make-hash-table)))
             (hash-table-update!
              state->state->actions
              previous-state
              (lambda (state->actions)
                (hash-table-update!
                 state->actions
                 state
                 (lambda (actions)
                   (if (heap-member? actions previous-action)
                       (heap-change-key! actions
                                         previous-action
                                         (add1 (heap-key actions previous-action)))
                       (heap-insert! actions
                                     1
                                     previous-action))
                   actions)
                 (lambda () (make-max-heap)))
                state->actions)
              (lambda () (make-hash-table)))
             ;; Should we short-circuit error-correction for the sake of
             ;; the goal?
             (if goal?
                 (begin
                   (set! previous-state #f)
                   (set! previous-action #f)
                   (set! expected-states (make-stack))
                   zero-motion)
                 ;; Given that we just added a key corresponding to
                 ;; state, previous-state and previous-action; this should
                 ;; always return at least something.
                 ;;
                 ;; The question is whether it defies our expectations.
                 (begin
                   (inc! expectations)
                   (set! previous-state state)
                   (let* ((possible-states
                           (hash-table-ref/default
                            (hash-table-ref/default
                             state->action->states
                             previous-state
                             (make-hash-table))
                            previous-action
                            (make-max-heap)))
                          (expected-state
                           (and (not (heap-empty? possible-states))
                                (heap-extremum possible-states)))
                          (expected-state?
                           (or (heap-empty? possible-states)
                               (equal? expected-state state))))
                     ;; (debug expected-state)
                     ;; (debug expected-state?)
                     ;; This is also false if we haven't seen it
                     ;; before, right? Need to distinguish between the
                     ;; two.
                     (if expected-state?
                         (begin
                           (inc! met-expectations))
                         (begin
                           (stack-push! expected-states expected-state)
                           ;; Handle the non no-op case.
                           (unless (equal? state previous-state)
                             (stack-push! expected-states previous-state))))
                     ;; (debug (/ met-expectations expectations))
                     )
                   (while (and (not (stack-empty? expected-states))
                               (equal? (stack-peek expected-states)
                                       state))
                     (stack-pop! expected-states))
                   ;; Handle expectations.
                   (if (stack-empty? expected-states)
                       (let ((action
                              (list-ref state (random (length state)))))
                         (set! previous-state state)
                         (set! previous-action action)
                         action)
                       (let ((expected-state (stack-peek expected-states)))
                         ;; (debug expected-state
                         ;;        state
                         ;;        (stack-count expected-states))
                         (debug
                          ;; expected-state
                          (stack-count expected-states))
                             ;; Figure out if there's a A, x -> B: where A
                             ;; is state, B is expected-state; otherwise,
                             ;; random. (In which case: push state unto
                             ;; expected-states.)
                         (let* ((return
                                 (hash-table-ref/default
                                  (hash-table-ref/default
                                   state->state->actions
                                   state
                                   (make-hash-table))
                                  previous-state
                                  (make-max-heap)))
                                (return
                                 (and (not (heap-empty? return))
                                      (heap-extremum return))))
                           (let ((action (if return
                                             return
                                             (list-ref state (random (length state))))))
                             (set! previous-action action)
                             action)))))))
           (let ((action
                  (list-ref state (random (length state)))))
             (set! previous-state state)
             (set! previous-action action)
             action))))))

(simulate-navigation make-agent-random-walk
                     n-points: 10
                     n-steps: 1000
                     p-slippage: 0.3
                     animation-file: #f)

;; Non-determinism-and-random-walk:1 ends here
