#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.4][5\.4:1]]

(use debug
     define-record-and-printer
     extras
     srfi-1
     vector-lib)

(define-record-and-printer sentinel)
(define sentinel (make-sentinel))

;;; Can get this down to bits?
(define (make-dag) (make-vector 28 #f))

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
    (vector-set!
     (fold (lambda (character dag)
             (let ((index (character->index character)))
               (unless (vector-ref dag index)
                 (vector-set! dag index (make-dag)))
               (vector-ref dag index)))
           dag
           characters)
     27
     #t)))

(define (update-dag! dag word)
  (let ((characters (string->list word)))
    (do ((prefix (list (car characters)) (cons (car suffix) prefix))
         (suffix (cdr characters) (cdr suffix)))
        ((null? suffix) (integrate-fixes dag prefix suffix))
      (integrate-fixes dag prefix suffix))))

(define (dag-debug dag depth)
  (vector-for-each
   (lambda (i x)
     (when (vector? x)
       ;; (debug (index->character i))
       (format #t "~a~a~%" (make-string depth #\space) (index->character i))
       (dag-debug x (add1 depth))))
   dag))

;;; Be nice to store these fuckers in a graph database or something.
(let ((dag (make-dag)))
  ;; (update-dag! dag "HARRO")
  (with-input-from-file "words-head.txt"
    (lambda ()
      (do ((word (read-line) (read-line)))
          ((eof-object? word))
        (debug word)
        (update-dag! dag word))))
  (dag-debug dag 0))

;; 5\.4:1 ends here
