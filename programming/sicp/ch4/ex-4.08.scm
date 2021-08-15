#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.8

;; "Named let" has syntax (let <var> <bindings> <body>).
(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))
(define (let->combination exp)
  ;; Super clever: for the named let create a begin statement (using 
  ;; sequence->exp) and then two procedures:
  ;; 1. Define the named proc with the lambda of parameter names and body
  ;; 2. Invoke the named proc with the parameter values
  (if (named-let? exp)
      (sequence->exp (cons (list 'define
                                 (named-let-name exp)
                                 (make-lambda (map car (named-let-bindings exp))
                                              (named-let-body exp)))
                           (cons (named-let-name exp)
                                 (map cadr (named-let-bindings exp)))))
      (cons (make-lambda (map car (let-bindings exp))
                         (let-body exp))
            (map cadr (let-bindings exp)))))
