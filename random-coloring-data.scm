#!/usr/bin/env chicken-scheme

(use debug
     matchable
     posix
     srfi-13)

(with-output-to-file "random-coloring-data.csv"
  (lambda ()
    (for-each
        (lambda (file)
          (match (string-split
                  (with-input-from-file file read-line) ",")
            ((index n time)
             (format #t "~a,~a,~a~%" index n time))))
      (find-files "random-coloring"))))
