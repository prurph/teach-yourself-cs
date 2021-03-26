;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_thm_1.8
; Newton's Method for Cube Roots: improve approximation y as (x/y**2 + 2y) / 3
(define (square x)
  (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

; Same good-enough? function as ex-1.07
; SICP foreshadows that Newton's Method can be generalized. This amounts to
; defining a HOF that builds the iteration based on a good-enough? procedure
; and an improve procedure (we can keep improving until we're good enough, or
; at least Newton thinks so).
(define (good-enough? guess x)
  (<= (abs (- (improve guess x) guess))
      (* guess 0.001)))

(define (cubert-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubert-iter (improve guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))
