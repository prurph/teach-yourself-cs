#lang sicp

(#%require "stream.scm")
(#%require "ex-3.59.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.62

;; From 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

;; From 3.61
(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s)
                                           (invert-unit-series s))
                               -1)))

;; Invert a series generically. Find its constant term, scale by dividing it to
;; make a unit series, then use `invert-unit-series` then rescale by the
;; constant. Obviously in the case where it is a unit series, the scaling is by
;; 1 and has no effect.
(define (invert-series s)
  (let ((c (stream-car s)))
    (scale-stream (invert-unit-series (scale-stream s (/ 1 c)))
                  c)))

(define (div-series s1 s2)
  (if (zero? (stream-car s2))
      (error ("Zero constant term in denominator: DIV-SERIES"))
      (mul-series s1 (invert-series s2))))

(define tangent-series (div-series sine-series cosine-series))
;; 0, 1, 0, 1/3, 0, 2/15 matches tangent series from [proofwiki.org](https://proofwiki.org/wiki/Power_Series_Expansion_for_Tangent_Function)
