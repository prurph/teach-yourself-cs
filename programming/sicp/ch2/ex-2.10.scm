;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.10

(define (div-interval x y)
  (if (and (<= 0 (lower-bound y))
           (>= 0 (upper-bound y)))
      (error "Divisor interval spans zero")
      ((mul-interval x (make-interval (/ 1 (upper-bound y))
                                      (/ 1 (lower-bound y)))))))
