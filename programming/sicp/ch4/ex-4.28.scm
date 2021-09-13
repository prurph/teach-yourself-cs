#lang sicp

(#%require "lazy-evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.28

;; An example of why the operator must be forced is the following:

(define (do-this x) x)
((do-this +) 1 2)

;; If the operator isn't forced then the apply sequence looks like:

(my-lazy-apply (eval (operator exp) env)
               (operands exp)
               env)

;; Now however the operator is the thunk produced from evaluating (do-this +),
;; and when apply attempts to apply it, this is not a primitive or compound
;; procedure, rather a thunk, and so apply will error since it does not know
;; what to do with this.
