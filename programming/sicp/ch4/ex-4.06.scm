#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.6

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
  (cons (make-lambda (map car (let-bindings exp))
                     (let-body exp))
        (map cadr (let-bindings exp))))

;; Add to eval:
;; ((let? exp) (eval (let->combination exp) env))
