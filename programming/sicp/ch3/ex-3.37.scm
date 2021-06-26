#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.36

(#%require "constraint.scm")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;; x - y = z => z + y = x
(define (c- x y)
  (let ((z make-connector))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

;; x / y = z => zy = x
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

;; x is a connector with Celsius temperatures, so:
;; 9x/5 + 32 = <temperature in Fahrenheit>
(define (celsius-fahrenheit-coverter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-coverter C))
(probe "Celsius" C)
(probe "Fahrenheit" F)

(set-value! F 32 'user)
(forget-value! F 'user)
(set-value! C 100 'user)
