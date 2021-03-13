;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html#%_thm_1.44

(define (smooth f dx)
  (define dx 0.001)
  (lambda (x)
    (/ (+ (f x) (f (- x dx)) (f (+ x dx)))
       3)))

;; n-fold smoothed function using repeated from ex-1.43
(define (n-fold-smoothed f n)
  ((repeated smooth n) f))
