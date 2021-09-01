#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.14

;; Primitive procedures are applied by:
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;; If map is installed as a primitive, then calling `(map f xs)` becomes `(map '(f xs))`, however Scheme's built-in map function does not take single argument like this, so it fails.
