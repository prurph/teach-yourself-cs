#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.33

(#%require "constraint.scm")

(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(probe "A" A)
(probe "B" B)
(probe "C" C)

(averager A B C)

(set-value! A 2 'user)  ; A = 2, B = ?, C = ?
(set-value! B 8 'user)  ; A = 2, B = 8, C = 5
(set-value! C 6 'user)  ; Error! Contradiction

(forget-value! B 'user) ; A = 2, B = ?, C = ?
(set-value! C 6 'user)  ; A = 2, B = 10, C = 6
