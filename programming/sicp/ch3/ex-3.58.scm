#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.58

(define (expand num den radix)
  (cons-stream (quotient (* num radix) den)
               (expand (remainder (* num radix) den) den radix)))

;; This procedure produces a stream of digits representing num/den in base radix

(expand 1 7 10)
;; 1 4 2 8 (from 1/7 = 0.14285714)

(expand 3 8 10)
;; 3 7 5 0 (from 3/8 = 0.375)
