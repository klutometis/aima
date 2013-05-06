#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.5][5\.5:3]]

(use debug loops zmq)

(let ((socket (make-socket 'rep)))
  (bind-socket socket "tcp://*:5555")
  (do-times i 4
            (let ((message (receive-message* socket)))
              (send-message socket message))))

;; 5\.5:3 ends here
