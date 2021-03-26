;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_thm_1.7
; Newton's Method: successive approximations for determining sqrt
(define (square x)
  (* x x))

; The previous static good-enough? didn't work well for:
; - Very large numbers: floating-point precision means we may never converge
; - Very small numbers: the minimum distance is too large relative to the input:
;     (square (sqrt 0.0005)) => 0.00101

; Improve guess (get closer to sqrt x) by averaging guess and x/guess
(define (improve guess x)
  (average guess (/ x guess)))

; Modified good-enough? that peeks at the next improvement to see if it is
; within 0.001 of the guess. An alternative implementation would thread the
; prior guess through sqrt-iter.
(define (good-enough? guess x)
  (<= (abs (- (improve guess x) guess))
      (* guess 0.001)))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
