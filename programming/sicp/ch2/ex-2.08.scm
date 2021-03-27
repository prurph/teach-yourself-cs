;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.8

(load "interval.scm")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; or, using add-interval:
(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))
