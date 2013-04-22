#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.4][5\.4:1]]

(use debug
     define-record-and-printer
     extras
     srfi-1
     vector-lib)

(define-record-and-printer sentinel)
(define sentinel (make-sentinel))

(define-record-and-printer letter
  terminal?
  subdag)

(define (make-dag) (make-vector 27 #f))

(define (make-terminal-letter)
  (make-letter #t (make-dag)))

(define (make-non-terminal-letter)
  (make-letter #f (make-dag)))

(define (character->index character)
  (if (sentinel? character)
      26
      (- (char->integer character) 65)))

(define (index->character index)
  (if (= index 26)
      sentinel
      (integer->char (+ index 65))))

(define (integrate-fixes dag prefix suffix)
  (let ((characters (if (null? suffix)
                      prefix
                      (append (append prefix (list sentinel))
                              suffix))))
    (fold (lambda (character letter)
            (let ((dag (letter-subdag letter))
                  (index (character->index character)))
              (unless (vector-ref dag index)
                (vector-set! dag index (make-non-terminal-letter)))
              (vector-ref dag index)))
          (make-letter #f dag)
          characters)))

(define (update-dag! dag word)
  (let ((characters (string->list word)))
    (do ((prefix (list (car characters)) (cons (car suffix) prefix))
         (suffix (cdr characters) (cdr suffix)))
        ((null? suffix) (integrate-fixes dag prefix suffix))
      (integrate-fixes dag prefix suffix))))

(define (dag-debug dag depth)
  (vector-for-each
   (lambda (i x)
     (when x
       ;; (debug (index->character i))
       (format #t "~a~a~%" (make-string depth #\space) (index->character i))
       (dag-debug (letter-subdag x) (add1 depth))))
   dag))

;;; Be nice to store these fuckers in a graph database or something.
(let ((dag (make-vector 27 #f)))
  ;; (update-dag! dag "HARRO")
  ;; (dag-debug dag 0)
  (with-input-from-file "words.txt"
    (lambda ()
      (do ((word (read-line) (read-line)))
          ((eof-object? word))
        (debug word)
        (update-dag! dag word))))
  (dag-debug dag 0))

;; 5\.4:1 ends here
