#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.3

;; Assume we have some table with put and get, the latter returns #f if an entry
;; doesn't exist
(define (put . ks) '())
(define (get . ks) '())

(define (eval exp env)
  (let ((proc (get 'eval (exp-type exp))))
    (if proc
        (proc exp env)
        (error "Unknown expression type: EVAL" exp))))

(define (exp-type exp)
  (cond ((self-evaluating? exp) 'self-evaluating)
        ((varible? exp) 'variable)
        ((quoted? exp) 'quote)
        ((assignment? exp) 'assignment)
        ((definition? exp) 'definition)
        ((if? exp) 'if)
        ((lambda? exp) 'lambda)
        ((begin? exp) 'begin)
        ((cond? exp) 'cond)
        ((application? exp) 'application)))

(put 'eval 'self-evaluating (lambda (exp env) exp))
(put 'eval 'variable lookup-variable-value)
(put 'eval 'quoted (lambda (exp env) (text-of-quotation exp)))
(put 'eval 'assignment eval-assignment)
(put 'eval 'definition eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda
     (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                       (lambda-body exp)
                                                       env)))
(put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))
(put 'eval 'application
     (lambda (exp env) (apply (eval (operator exp) env)
                              (list-of-values (operands exp) env))))
