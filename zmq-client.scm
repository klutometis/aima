#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:2]]

(use debug posix zmq)
(let ((socket (make-socket 'req))
      (name (number->string (current-seconds))))
  (connect-socket socket "tcp://*:5555")
  (send-message socket name)
  (debug name (receive-message socket))
  (sleep 2)
  (send-message socket name)
  (debug name (receive-message socket)))

;; 5\.5:2 ends here
