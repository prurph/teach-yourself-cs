#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.7

(define (let*? exp) (tagged-list? exp 'let*))
;; Could also use reduce. Returning body directly for the base case from
;; https://github.com/felix021/sicp/blob/master/code/4-07.scm and is super
;; slick! I originally checked if the cdr was null and had a base case like
;; (list 'let (car bindings) body), but this is much more elegant.
(define (let*->nested-lets exp)
  (define (nest-bindings bindings)
    (if (null? bindings)
        (let-body exp)
        (list 'let (list (car bindings)) (nest-bindings (cdr bindings)))))
  (nest-bindings (let-bindings exp)))

;; Add to eval:
;; ((let*? exp) (eval (let*->nested-lets exp) env))
