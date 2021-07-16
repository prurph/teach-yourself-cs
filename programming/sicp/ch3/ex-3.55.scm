#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.55

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; Given a stream S, return the stream whose elements are S0, S0+S1, S0+S1+S2
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))
