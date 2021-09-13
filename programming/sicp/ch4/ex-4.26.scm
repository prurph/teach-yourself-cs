#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.26

;; Implementing unless as a derived expression:
(define (eval exp env)
  (cond
        ;; ...
        ((unless? exp) (eval (unless->if exp) env))
   ))

(define (unless? exp)
  (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (unless->if exp)
  (make-if (unless-condition exp) (unless-exceptional-value exp) (unless-usual-value exp)))

;; One advantage of having unless as a procedure instead of a derived
;; expression is you could use it in map or filter like:
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(define defaults (list 'peach 'watermelon 'cherry))
(define overrides (list 'PUPPY #f 'KITTEN))
(define options (map unless overrides defaults overrides))
;; => '(PUPPY watermelon KITTEN)
