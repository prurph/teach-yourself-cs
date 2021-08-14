#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.4

;; Adjust eval as follows:
;; (define (eval exp env)
;;   (cond ; everything else
;;         ((and? exp) (eval-and (and-exps exp) env))
;;         ((or? exp) (eval-or (or-exps exp) env))
;;         ; rest
;;         ))
 
;; and and or implemented as special forms
(define (and? exp) (tagged-list? exp 'and)) 
(define (and-exps exp) (cdr exp))
(define (eval-and exps env)
  (if (null? exps)
      'true
      (if (true? (eval (car exp) env))
          (eval-and (cdr exps) env)
          'false)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (eval-or exps env)
  (if (null? exps)
      'false
      (if (true? (eval (car exp) env))
          'true
          (eval-or (cdr exps) env))))

;; and and or implemented as derived expressions
(define (eval-and-derived exps env)
  (eval (and->if exps) env))
(define (and->if exps)
  (if (null? exps)
      'true
      (make->if (car exps) (and->if (cdr exps)) 'false)))

(define (eval-or-derived exps env)
  (eval (or->if exps) env))
(define (or->if exps)
  (if (null? exps)
      'false
      (make->if (car exps) 'true (or->if (cdr exps)))))

