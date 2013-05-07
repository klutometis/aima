#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:4]]

(use debug
     medea
     posix
     zmq)
(let ((socket (make-socket 'req))
      (name (symbol->string (gensym))))
  (connect-socket socket "tcp://*:5555")
  ;; (debug (json->string `((name . ,name))))
  (send-message socket (json->string `((name . ,name))))
  (debug (receive-message* socket)))

;; 5\.5:4 ends here
