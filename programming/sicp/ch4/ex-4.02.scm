#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.2

;; 4.2(a)
;; With this change, all lists will be treated as procedure applications, even
;; those special forms like define.

;; If application were evaluated before assignment for (define x 3):
(eval '(define x 3) env)
;; Relevant eval clause:
(apply (eval (operator exp) env)
       (list-of-values (operands exp) env))
;; exp is (define x 3), substituting operator and operands
(apply (eval 'define env)
       (list-of-values '(x 3) env))
;; Now we have (eval 'define env):
(variable? define)              ; => true
;; evaluate the proper consequent
(lookup-variable-value exp env) ; => unbound variable 'define

;; The above would not happen for the original eval because the special form
;; would be detected since (definition? exp) would be hit before
;; (application? exp), which is designed to be a catch all.

;; 4.2(b)
;; Now the syntax has been changed so that procedure application has an explicit
;; syntax (call operator operand). The required changes are:
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
