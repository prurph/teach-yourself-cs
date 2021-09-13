#lang sicp

(#%require "lazy-evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.27

(driver-loop)
(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define w (id (id 10)))

;;; L-Eval input:
count

;;; L-Eval value:
1

;;; L-Eval input:
w

;;; L-Eval value:
10

;;; L-Eval input:
count

;;; L-Eval value:
2

;; Explanation
;; Defining w evaluates (id (id 10)). This is a procedure application which results in:

(my-lazy-apply (actual-value (operator exp) env)
               (operands exp)
               env)

;; where the operator is evaluated, but the operands are lazy. This means the
;; operator, `id`, gets evaluated and so count is incremented by 1 upon
;; definition of w.  When w is then evaluated, we must fetch its value, which
;; now requires evaluating the operand, increasing count again and resulting in
;; the observed final value of 2.
