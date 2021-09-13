#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-27.html#%_thm_4.25

;; Assume we have unless defined as:
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

;; Then defining factorial in terms of this:
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; Attempting to evaluate factorial in regular applicative-order Scheme:
(factorial 5)
(unless (= 5 1)
        (* 5 (factorial 4))
        1)
;; Since unless is _not_ a special form, we evaluate both of its arguments:
(unless (= 5 1)
        (* 5 (unless (= 4 1)
                     (* 4 (factorial 3))
                     1)))
;; This continues infinitely:
(
 ;; ...
 unless (= 1 1)
        (* 1 (factorial 0)) ; just keep evaluating this argument, it never stops!
        1)

;; In a normal order language our definitions will work because the arguments
;; will be evaluated as needed, so when n = 1 the if will return 1, avoiding
;; the infinite evaluation of factorial for n = 0 and less, and we
;; go back up the stack to return a value.
