#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.20

;; 4.20(a)
;; Implement letrec as a derived expression.
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))

(define (letrec->combination exp)
  (let* ((bindings (letrec-bindings exp))
         (vars (map car bindings))
         (vals (map cadr bindings)))
    (define (unassigned-binding var)
      (list var '*unassigned*))
    (define (assignment var val)
      (list 'set! var val))
    (append (list 'let (map unassigned-binding vars))
            (map assignment vars vals)
            (letrec-body exp))))

;; 4.20(b)
;; If using let instead of letrec:
(define (f x)
  (let ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
        (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
    ;; <body of f>
    'nothing-here))
;; even and odd cannot reference each other in this way because the call is converted to a lambda:
(define (fi x)
  (lambda (even? odd?)
    ;; <body of f>
    'nothing-here)((lambda (n) (if (= n 0) true (odd? (- n 1))))
                   (lambda (n) (if (= n 0) false (even? (- n 1))))))
;; Notice above that odd? and even? will not be defined when the argument
;; lambdas attempt to call them.
