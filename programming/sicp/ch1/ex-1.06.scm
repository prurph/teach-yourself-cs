;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_thm_1.6
; Newton's Method: successive approximations for determining sqrt
(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define good-enough 0.001)

(define (good-enough? guess x)
  (<= (abs (- (square guess) x)) good-enough))

; Improve guess (get closer to sqrt x) by averaging guess and x/guess
(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; custom `new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; infinite recursion
(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new-if (improve guess x) x)))

; The built-in special form if does not evaluate its arguments until it has
; evaluated the predicate This is effectively a special case where Scheme uses
; normal order. Defining a custom `new-if`, however, causes both the
; `then-clause` and `else-clause` to be evaluated regardless of the truthiness
; of the predicate; this is applicative order as per usual. Consequently
; `sqrt-iter-new-if` recurses infinitely.
