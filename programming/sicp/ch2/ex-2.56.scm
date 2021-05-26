#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.56

(require "deriv.scm")

;; d(u^n)/dx = n*u^(n-1) * du/dx, with exponentiation encoded as **
(define (exponentiation? e) (and (pair? e) (eq? (car e) '**)))

(define (make-exponentiation b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        ((=number? b 1) 1)
        ((and (number? b) (number? p)) (expt b p))
        (else (list '** b p))))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

;; New deriv with support for exponentiation. Tricky part: use make-sum to
;; subtract one from the exponent
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product (make-product e
                                       (make-exponentiation b (make-sum e -1)))
                         (deriv b var))))
        (else (error "unkown expression type: DERIV" exp))))

(provide make-exponentiation)
(provide base)
(provide exponent)
