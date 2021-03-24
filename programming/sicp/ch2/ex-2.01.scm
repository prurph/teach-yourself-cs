; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.1

(load "rat.scm")

;; Better `make-rat` that handles negative arguments
;; Depends on gcd (from rat.scm)
(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (xor (negative? n) (negative? d)) (- 1) 1)))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))
