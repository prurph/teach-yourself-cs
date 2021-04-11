#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.4

;; Given the alternate representation of pairs:
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

;; Verify that `(car (cons x y))` yields x
(car (cons x y))
((cons x y) (lambda p q p))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) (x y))
(lambda (x y) x)
x

;; Corresponding definition of `cdr`
(define (cdr z)
  (z (lambda (p q) q)))

;; Essentially, this definition of cons is a higher-order function that 
;; returns a procedure that takes a procedure and calls it with arguments x
;; and y. Thus `car` simply returns the first argument, x, and `cdr` the second,
;; y.
