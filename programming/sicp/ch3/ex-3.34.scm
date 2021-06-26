#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.34

(#%require "constraint.scm")

(define (squarer a b)
  (multiplier a a b))

(define A (make-connector))
(define B (make-connector))

(squarer A B)
(probe "A" A)
(probe "B" B)

;; Setting a works correctly: two of the three inputs are known to the mulitplier
;; and thus it is able to compute the third (B).
(set-value! A 2 'user) ; A = 2, B = 4
(forget-value! A 'user) ; A = ?, B = ?

;; Setting B, however, does _not_ work. The multiplier has no special knowledge
;; that its other two inputs are the same, and thus the constraint could be
;; satisfied by setting them to the square root of B. Instead, it "sees" only
;; one of its inputs is set, and therefore does not set the value for A.
(set-value! B 25 'user) ; B = 25, A = ?
