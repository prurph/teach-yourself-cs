;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.17

(define (double x) (* x 2))
(define (halve x) (/ x 2))

;; a * b = 2a(b/2)
;; a * b = a + a(b - 1)
(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-* (double a) (halve b)))
        (else (+ a (fast-* a (- b 1))))))
