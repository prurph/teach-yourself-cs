;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.35

;; φ = (1 + sqrt 5) / 2
;; x is a fixed point of f if f(x) = x, so fixed points of x -> 1 + 1/x are:
;;   x = 1 + 1 / x, or x^2 = x + 1
;;   φ^2 = (1 + 2*sqrt 5 + 5)/4 = (6 + 2*sqrt 5)/4 = (1 + sqrt 5)/2 + 1
;; thus φ is a fixed point of f(x) = 1 + 1/x

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ;; Successively apply f(x), f(f(x)), ... until successive values are close
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;; => 1.618032786...

