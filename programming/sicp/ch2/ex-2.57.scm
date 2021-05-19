#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.57

(require "deriv.scm")

;; Extending program to handle sums and products of arbitrary numbers of terms.
;; I originally tried redefining make-sum and make-product to take varargs, but
;; as noted [by Drew Hess](http://wiki.drewhess.com/wiki/SICP_exercise_2.57)
;; it is cleaner to just redefne augend and multiplicand. I would not have
;; figured this out myself!

;; If we have exactly a binary expression, like (+ 2 3), the augend is the
;; last term (caddr). Otherwise it is the sum of everything but the addend.
(define (my-augend a)
  (if (eq? (length a) 3)
      (caddr a)
      (cons '+ (cddr a))))

(define (my-multiplicand m)
  (if (eq? (length m) 3)
      (caddr m)
      (cons '* (cddr m))))

;; This is an equivalent definition to the original deriv; doing it this way to
;; avoid annoying namespace clashes in the REPL that make it hard to know when
;; your code is bugged vs referencing stale procedure definitions.
(define (my-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (my-deriv (addend exp) var)
                              (my-deriv (my-augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (my-deriv (my-multiplicand exp) var))
                                  (make-product (my-deriv (multiplier exp) var)
                                                (my-multiplicand exp))))
        (else (error "unkown expression type: DERIV" exp))))

;; New form with multiple numbers
(my-deriv '(* x y (+ x 3)) 'x)
;; => '(+ (* x y) (* y (+ x 3)))

;; Equivalent old representation
(my-deriv '(* (* x y) (+ x 3)) 'x)
;; => '(+ (* x y) (* y (+ x 3)))
