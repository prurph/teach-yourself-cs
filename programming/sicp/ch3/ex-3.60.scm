#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.60

;; (a0 + a1x + a2x² + ...)(b0 + b1x + b2x² + ...)
;; a0b0 + (a1b0 + a0b1)x + (a2b0 + b2a0 + a1b1)x²
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))
