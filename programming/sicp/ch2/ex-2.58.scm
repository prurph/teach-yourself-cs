#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.58

(require "deriv.scm")

;; 2.58(a)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;; Sophisticated make-product that will reduce answers to their "simplest" form
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        ((=number? b 1) 1)
        ((and (number? b) (number? p)) (expt b p))
        (else (list b '** p))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (exponentiation? e) (and (pair? e) (eq? (cadr e) '**)))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (infix-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (infix-deriv (addend exp) var)
                              (infix-deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (infix-deriv (multiplicand exp) var))
                                  (make-product (infix-deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product (make-product e
                                       (make-exponentiation b (make-sum e -1)))
                         (deriv b var))))
        (else (error "unkown expression type: DERIV" exp))))

;; 2.58(b) Skipping since this one is brutal.
