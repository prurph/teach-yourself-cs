;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_thm_1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; Applicative-order evaluation (Lisp)
; - Evaluate operator and operands, then apply resulting procedure to resulting arguments (lazy)
; (test 0 (p)) ; to get the result of test, we need to evaluate its operands
; (test 0 (p)) ; (p) expands to itself, creating an infinite loop

; Normal-order evaluation
; - Substitute operand expressions for formal parameters ("fully expand then reduce")
; (test 0 (p))       ; first expand the body of test, not its operands
; (if (= 0 0) 0 (p)) ; do not evaluate operand until expression contains only primitive operators
; (#t 0 (p))         ; p is never evaluated
; 0
