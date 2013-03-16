#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*4.11c][4\.11c:1]]

(include "online-navigation.scm")

(define (maybe-update-untried! untried state)
  (unless (hash-table-exists? untried state)
    (hash-table-set!
     untried
     state
     (list->stack state))))

(define (maybe-update-labels! labels state)
  (unless (hash-table-exists? labels state)
    (hash-table-set!
     labels
     state
     (gensym))))

(define (maybe-update-result! result previous-state previous-action state)
  (hash-table-update!
   result
   previous-state
   (lambda (state->action)
     (hash-table-set! state->action state previous-action)
     state->action)
   (lambda () (make-hash-table))))

(define (maybe-update-unbacktracked! unbacktracked backtrack? previous-state state)
  (unless backtrack?
    (hash-table-update!
     unbacktracked
     state
     (lambda (backtracks)
       (stack-push! backtracks previous-state)
       backtracks)
     (lambda () (make-stack)))))

(define (maybe-update-coordinates! coordinates previous-state previous-action state time)
  ;; We could probably set this as a state variable
  ;; and avoid the lookup.
  (let* ((previous-coordinate
          (hash-table-ref coordinates previous-state))
         (previous-point (coordinate-point previous-coordinate)))
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
                    ;; (debug ;; (coordinate-time old-coordinate)
                    ;;  ;; time
                    ;;  delta-x
                    ;;  delta-y)
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

(define (maybe-update-origin! coordinates state time)
  (unless (hash-table-exists? coordinates state)
    (hash-table-set!
     coordinates
     state
     (make-coordinate origin time))))

(define (untried? untried state)
  (stack-empty? (hash-table-ref/default untried state (make-stack))))

(define (unbacktracked? unbacktracked state)
  (stack-empty? (hash-table-ref/default unbacktracked state (make-stack))))

(define (make-agent-online-dfs start next-frame)
  (make-agent
   start
   0
   (let ((coordinates (make-hash-table))
         (labels (make-hash-table))
         ;; state->state->[[action_0, p_0], ..., [action_n, p_n]]
         (result (make-hash-table))
         ;; state->action->[[state_0, p_0], ..., [state_n, p_n]]
         (state->action->state (make-hash-table))
         (untried (make-hash-table))
         (unbacktracked (make-hash-table))
         (previous-state #f)
         (previous-action #f)
         (backtrack? #f)
         (time 0)
         (start #f)
         (goal #f))
     (define (reset-state! state)
       (set! goal state)
       (set! start #f)
       (set! previous-state #f)
       (set! previous-action #f)
       (set! backtrack? #f)
       (set! untried (make-hash-table))
       (set! unbacktracked (make-hash-table))
       (inc! time)
       ;; (if previous-state
       ;;     (hash-table-update!
       ;;      result
       ;;      previous-state
       ;;      (lambda (state->action)
       ;;        (hash-table-set! state->action state previous-action)
       ;;        state->action)
       ;;      (lambda () (make-hash-table))))
       )
     (define (maybe-write-agent-as-png state score)
       (unless (zero? (hash-table-size result))
         (write-agent-as-png (next-frame)
                             state
                             coordinates
                             labels
                             result
                             untried
                             unbacktracked
                             previous-state
                             previous-action
                             start
                             goal
                             score)))
     (lambda (state goal? score)
       (unless start
         (set! start state))
       (maybe-write-agent-as-png state score)
       (maybe-update-untried! untried state)
       (maybe-update-labels! labels state)
       (if previous-state
           (begin
             ;; (hash-table-update!
             ;;  state->action->state
             ;;  previous-state
             ;;  (lambda (action->state)
             ;;    (let* ((expected-state
             ;;            (hash-table-ref/default
             ;;             action->state
             ;;             previous-action
             ;;             #f))
             ;;           (navigation-error?
             ;;            (and expected-state
             ;;                 (equal? expected-state state))))
             ;;      (if expected-state
             ;;          (when navigation-error?
             ;;            (debug 'navigation-error))
             ;;          (hash-table-set!
             ;;           action->state
             ;;           previous-action
             ;;           state)))
             ;;    action->state)
             ;;  (lambda () (make-hash-table)))
             (maybe-update-result! result previous-state previous-action state)
             (maybe-update-unbacktracked! unbacktracked backtrack? previous-state state)
             (maybe-update-coordinates! coordinates previous-state previous-action state time))
           ;; We've been teleported to some previously unknown
           ;; location; let's designate it as the origin.
           (maybe-update-origin! coordinates state time))
       (if goal?
           (begin
             (debug "Goal found -- ONLINE-DFS-AGENT")
             (reset-state! state)
             zero-motion)
           (begin
             (if (untried? untried state)
                 (if (unbacktracked? unbacktracked state)
                     (begin
                       ;; (set! previous-action stop)
                       ;; (error "Goal not found -- AGENT-ONLINE-DFS")
                       (set! previous-action (list-ref state (random (length state))))
                       (debug "Goal not found -- AGENT-ONLINE-DFS"))
                     (let* ((backtrack (stack-pop! (hash-table-ref unbacktracked state)))
                            (backtrack-action
                             (hash-table-ref (hash-table-ref result state) backtrack)))
                       (debug 'backtrack)
                       (set! backtrack? #t)
                       (set! previous-action backtrack-action)
                       (set! previous-action (list-ref state (random (length state))))))
                 (begin
                   (set! backtrack? #f)
                   (set! previous-action (stack-pop! (hash-table-ref untried state)))))
             (set! previous-state state)
             previous-action))))))

(simulate-navigation make-agent-online-dfs
                     n-points: 10
                     n-steps: 100
                     p-slippage: 0
                     animation-file: "slippage.avi")

;; 4\.11c:1 ends here
