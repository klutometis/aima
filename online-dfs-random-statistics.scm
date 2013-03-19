#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*Non-determinism%20and%20random-walk][Non-determinism-and-random-walk:1]]

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
         (contingency-plans (make-stack))
         (coordinates (make-hash-table))
         (labels (make-hash-table))
         (time 0))

     (define (update-labels! state)
       (unless (hash-table-exists? labels state)
         (hash-table-set!
          labels
          state
          (gensym))))

     ;; Coordinates are a little different here: takes best-guess into
     ;; account.
     (define (update-coordinates! state)
       (let* ((possible-actions
               (hash-table-ref/default
                (hash-table-ref/default
                 state->state->actions
                 previous-state
                 (make-hash-table))
                state
                (make-max-heap)))
              (action
               (if (heap-empty? possible-actions)
                   previous-action
                   (heap-extremum possible-actions))))
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
                                (point-x action))
                             (+ (point-y previous-point)
                                (point-y action)))
                 time))))))

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

     (define (update-goals! previous-state expected-state state)
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
       (set! contingency-plans (make-stack))
       (inc! time))

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

     (define (write-agent-as-dot state)
       (let ((displayed (make-hash-table))
             (linear-scale (* 5 72))
             (contingencies (map (lambda (contingency-plan)
                                   (contingency-plan state))
                                 (stack->list contingency-plans))))

         (define (node-display state label)
           (unless (hash-table-exists? displayed state)
             (hash-table-set! displayed state #t)
             (let ((coordinate
                    (hash-table-ref/default coordinates
                                            state
                                            (make-coordinate origin time))))
               (format #t "~a [pos=\"~a,~a\"~a];"
                       label
                       (* (point-x (coordinate-point coordinate)) linear-scale)
                       (* (point-y (coordinate-point coordinate)) linear-scale)
                       (if (member state contingencies equal?)
                           ", shape=circle, label=E, color=red"
                           "")))))

         (write-dot-preamble 800 450 "Random walk with error correction")
         ;; (write-dot-preamble 1600 900 "Random walk with error correction")
         ;; Let's just take the top one for now?
         (hash-table-walk state->state->actions
           (lambda (whence state->actions)
             (hash-table-walk state->actions
               (lambda (whither actions)
                 (let ((whence-label (hash-table-ref labels whence))
                       (whither-label (hash-table-ref labels whither)))
                   (node-display whence whence-label)
                   (node-display whither whither-label)
                   (format #t "~a -> ~a [color=~a];"
                           whence-label
                           whither-label
                           (if (equal? whence previous-state)
                               "orange"
                               "blue")))))))
         (display "}")))

     (define (write-agent-as-png png state)
       (unless (zero? (hash-table-size state->state->actions))  
       (let ((dot (create-temporary-file ".dot")))
           (with-output-to-file dot
             (lambda ()
               (write-agent-as-dot state)))
           (run (neato -n1 -Tpng -o ,png < ,dot)))))

     (lambda (state goal? score)
       (debug state goal?)
       (update-labels! state)

       (when previous-action
         (update-statistics! state)
         (update-coordinates! state))

       (write-agent-as-png (next-frame) state)

       (if goal?
           (begin
             (debug "Found goal.")
             (reset!)
             zero-motion)
           (if previous-action         ; Implied: previous-state, too.
               (let ((expected-state (expected-state)))
                 (update-goals! previous-state expected-state state)
                 (iterate-over-goals state))
               (move-randomly state)))))))

(parameterize ((debug? #f))
  (simulate-navigation make-agent-random-walk
                       n-points: 10
                       n-steps: 100
                       p-slippage: 0.3
                       animation-file: "online-dfs-random-statistics.avi"))

;; Non-determinism-and-random-walk:1 ends here
