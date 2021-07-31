#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.1

(define (no-operands? ops)
  (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; Original list-of-values: order of operand evaluation depends on order of evaluation of arguments to cons
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps env)))))

;; list-of-values that always evaluates operands left to right
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
    '()
    (let ((left (eval (first-operand exps) env)))
      (cons left (list-of-values (rest-operands exps env))))))

;; list-of-values that always evaluates operands right to left
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
    '()
    (let ((right (eval (list-of-values (rest-operands exps env)))))
      (cons (eval (first-operand exps) env)
            right))))
