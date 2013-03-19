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
         (contingency-plans (make-stack)))
     (define (update-statistics! state)
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
        (lambda () (make-hash-table))))
     (define (maybe-update-goals! previous-state expected-state state)
       (if (not-unexpected-state? expected-state state)
           (begin
             (debug "This state is not unexpected."))
           (begin
             (debug "This state is statistically anomolous.")
             (debug "Pushing the expected-state unto contingency-plans.")
             (debug expected-state)
             ;; (stack-push! contingency-plans expected-state)
             (stack-push! contingency-plans (lambda (state) expected-state))
             (unless (equal? previous-state state)
               (debug "Pushing the previous-state unto contingency-plans.")
               (debug previous-state)
               (stack-push! contingency-plans (lambda (state) previous-state))))))
     (define (expected-state)
       (let* ((possible-states
               (hash-table-ref/default
                (hash-table-ref/default
                 state->action->states
                 previous-state
                 (make-hash-table))
                previous-action
                (make-max-heap))))
         (and (not (heap-empty? possible-states))
              (heap-extremum possible-states))))
     (define (not-unexpected-state? expected-state state)
       (or (not expected-state)
           (equal? state expected-state)))
     (define (reset!)
       (set! previous-state #f)
       (set! previous-action #f)
       (set! contingency-plans (make-stack)))
     (define (move state action)
       (set! previous-state state)
       (set! previous-action action)
       (debug action)
       action)
     (define (move-randomly state)
       (debug "Moving randomly.")
       (move state (list-ref state (random (length state)))))
     (define (move-backwards-or-randomly expected-state state)
       (let* ((return
               (hash-table-ref/default
                (hash-table-ref/default
                 state->state->actions
                 state
                 (make-hash-table))
                expected-state
                (make-max-heap)))
              (return
               (and (not (heap-empty? return))
                    (heap-extremum return))))
         (if return
             (begin
               (debug "Attempting to return.")
               (move state return))
             (begin
               (debug "Can't return.")
               (debug "Pushing a contingency unto contingency plans.")
               (stack-push! contingency-plans
                            (lambda (state)
                              (if (equal? state expected-state)
                                  expected-state
                                  state)))
               (move-randomly state)))))
     (define (iterate-over-goals state)
       (if (stack-empty? contingency-plans)
           (begin
             (debug "There are no expected states.")
             (move-randomly state))
           (begin
             ;; (debug (map (lambda (plan) (plan state))
             ;;             (stack->list contingency-plans)))
             (debug (stack-count contingency-plans))
             (let ((expected-state ((stack-peek contingency-plans) state)))
               (debug expected-state state)
               (if (equal? state expected-state)
                   (begin
                     (debug "We're at the expected state; popping contingency plans.")
                     (stack-pop! contingency-plans)
                     (iterate-over-goals state))
                   (begin
                     (debug "We're not at the expected state; trying to backtrack.")
                     (move-backwards-or-randomly expected-state state)))))))
     (lambda (state goal? score)
       (debug state goal?)
       (if goal?
           (begin
             (debug "Found goal.")
             (reset!)
             zero-motion)
           (if previous-action         ; Implied: previous-state, too.
               (begin
                 (update-statistics! state)
                 (let ((expected-state (expected-state)))
                   (maybe-update-goals! previous-state expected-state state)
                   (iterate-over-goals state)))
               (move-randomly state)))))))

(simulate-navigation make-agent-random-walk
                     n-points: 100
                     n-steps: 1000
                     p-slippage: 0.3
                     animation-file: #f)

;; Non-determinism-and-random-walk:1 ends here
