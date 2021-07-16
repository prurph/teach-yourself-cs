#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.53

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; Without running the program, describe the elements of the stream defined by:
(define s (cons-stream 1 (add-streams s s)))
;; This is the stream of powers of 2: 2, 4, 8, 16, 32. Each value is double the
;; prior.

