#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.61

;; From 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

;; S is a power series whose constant term is 1. Find 1/S by the following:
;;   X  = 1/S
;;   SX = 1
;; Let S_R be the part of S after the constant term; that is, S = 1 + S_R. Thus:
;;   (1 + S_R)*X = 1
;;   X + S_R * X = 1
;;   X = 1 - S_R * X
(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s)
                                           (invert-unit-series s))
                               -1)))
