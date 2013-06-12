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

(define-record-and-printer player
  id)

;; (thread-start!
;;  (lambda ()
;;    ;; (let ((socket (make-socket 'pub)))
;;    ;;   (bind-socket socket "tcp://*:5555")
;;    ;;   (let iter ()
;;    ;;     (debug (send-message socket (->string (hash-table->alist games))))
;;    ;;     (iter)))
;;    2))

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
        (let ((game (hash-table-ref/default games game-id #f))
              (player (make-player player-id)))
          (if game
              (let* ((player-id (generate-id))
                     (player (make-player player-id)))
                (hash-table-set! players player-id player)
                (display-content-type-&c. 'json)
                (write-json `((player-id . ,player-id)
                              (pubsub-url . ,(game-pubsub-url game)))))
              (display-status-&c. status-not-found)))))
     ("/list-games"
      (display-content-type-&c. 'json)
      (write-json `((game-ids . ,(list->vector (hash-table-keys games))))))
     ("/list-players"
      (display-content-type-&c. 'json)
      (write-json `((players-ids . ,(list->vector (hash-table-keys players))))))
     ("/play"
      (let ((game-id (query-client-any query 'game-id))
            (player-id (query-client-any query 'player-id))
            (move (query-client-any query 'move)))
        (let ((game (hash-table-ref/default games game-id #f))
              (player (hash-table-ref/default players player-id #f)))
          (if (and game player)
              (display-content-type-&c. 'json)
              (display-status-&c. status-not-found)))))
     (_ (display-status-&c. status-bad-request)))))

;; 5\.5:10 ends here
