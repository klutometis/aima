#!/usr/bin/env chicken-scheme

;; [[file:~/prg/scm/aima/aima.org::*5.4][5\.4:1]]

(use debug
     define-record-and-printer
     extras
     srfi-1)

(define-record-and-printer sentinel)
(define sentinel (make-sentinel))

(define-record-and-printer letter
  terminal?
  subdag)

(define (make-terminal-letter)
  (make-letter #t (make-hash-table)))

(define (make-non-terminal-letter)
  (make-letter #f (make-hash-table)))

(define (integrate-fixes dag prefix suffix)
  (let ((characters (if (null? suffix)
                      prefix
                      (append (append prefix (list sentinel))
                              suffix))))
    (letter-terminal?-set!
     (fold (lambda (character letter)
             (hash-table-update!
              (letter-subdag letter)
              character
              identity
              make-non-terminal-letter))
           ;; This is a trojan-horse letter containing the root dag:
           ;; the fold expects a letter.
           (make-letter #f dag)
           characters)
     #t)))

(define (integrate-fixes dag prefix suffix)
  (let ((characters (if (null? suffix)
                      prefix
                      (append (append prefix (list sentinel))
                              suffix))))
    (fold (lambda (character dag)
            (hash-table-update!
             dag
             character
             identity
             make-hash-table))
          ;; This is a trojan-horse letter containing the root dag:
          ;; the fold expects a letter.
          dag
          characters)))

(define (update-dag! dag word)
  (let ((characters (string->list word)))
    (do ((prefix (list (car characters)) (cons (car suffix) prefix))
         (suffix (cdr characters) (cdr suffix)))
        ((null? suffix) (integrate-fixes dag prefix suffix))
      (integrate-fixes dag prefix suffix))))

(define (dag-debug dag depth)
  (hash-table-walk dag
    (lambda (character letter)
      (format #t "~a~a~%"
              (make-string depth #\space)
              character)
      (dag-debug (letter-subdag letter) (add1 depth)))))

(define (dag-debug dag depth)
  (hash-table-walk dag
    (lambda (character subdag)
      (format #t "~a~a~%"
              (make-string depth #\space)
              character)
      (dag-debug subdag (add1 depth)))))

(let ((dag (make-hash-table)))
  ;; (update-dag! dag "harro")
  ;; (dag-debug dag 0)
  (with-input-from-file "words.txt"
    (lambda ()
      (do ((word (read-line) (read-line)))
          ((eof-object? word))
        (debug word)
        (update-dag! dag word))))
  ;; (dag-debug dag 0)
  )

;; 5\.4:1 ends here
