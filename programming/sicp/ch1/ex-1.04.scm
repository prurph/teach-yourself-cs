; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_thm_1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; (a-plus-abs-b 1 2)
; ((if (> b 0) + -) a b))
; ((if (> 1 0) + -) 1 2))
; (+ 1 2)
; 3

; The if expression returns an operator, + or -, which is then applied to the
; operands, a and b. Sort of like a higher-order function.
