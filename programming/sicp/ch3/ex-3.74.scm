#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.74

(define (sign-change-detector v1 v2)
  (cond ((and (> v1 0) (< v2 0)) -1)
        ((and (< v1 0) (> v2 0))  1)
        (else 0)))

(define sense-data (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

;; Need to use (cons-stream 0 sense-data) not (stream-cdr sense-data) so that
;; we emit something at the first element of the input stream (always 0).
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(display-stream zero-crossings)
