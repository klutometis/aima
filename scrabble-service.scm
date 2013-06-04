#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:10]]

(use call-with-query
     define-record-and-printer
     hostinfo
     matchable
     medea)

(define pubsub-host
  (make-parameter (current-hostname)))

(define pubsub-port
  (make-parameter 5555))

(define pubsub-url
  (make-parameter (format "~a:~a" (pubsub-host) (pubsub-port))))

(define games (make-hash-table))
(define players (make-hash-table))

;;; Something more like a random SHA1 hash?
(define (generate-id) (symbol->string (gensym)))

(define-record-and-printer game
  id
  pubsub-url)

(define-record-and-printer user
  id)

(call-with-dynamic-fastcgi-query
 (lambda (query)
   (match (query-any query 'path-info)
     ;; Needs the type of game, theoretically.
     ("/new"
      (let* ((game-id (generate-id))
             (game (make-game game-id (pubsub-url))))
        (hash-table-set! games game-id game)
        (display-content-type-&c. 'json)
        (write-json `((game-id . ,game-id)))))
     ;; Also, watch.
     ("/join"
      (let ((game-id (query-client-any query 'game-id))
            (player-id (generate-id)))
        (let ((game (hash-table-ref/default games game-id #f)))
          (if game
              (begin
                (display-content-type-&c. 'json)
                (write-json `((player-id . ,player-id)
                              (pubsub-url . ,(game-pubsub-url game)))))
              (display-status-&c. status-not-found)))))
     ("/list"
      (display-content-type-&c. 'json)
      (write-json `((game-ids . ,(list->vector (hash-table-keys games))))))
     (_ (pp "yes")))))

;; 5\.5:10 ends here
