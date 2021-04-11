#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.9

;; Width of an interval: half the difference between upper and lower bounds
;; For adding (or subtracting, since it can be implemented in terms of addition)
;; the width is a function only of the widths of the intervals:
;;
;; Given:
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;; The width of the resulting interval can be expressed as:
(let width (/ (- (+ (upper-bound x) (upper-bound y))
                 (+ (lower-bound x) (lower-bound y)))
              2.0))
;; Simplifying:
;; (ubx + uby - lbx - lby) / 2
;; (ubx - lbx) / 2 + (uby - lby) / 2
;; (+ (width x) (width y))

;; However a counter-example that shows this does not work for multiplication.
;; The width of the resultant interval does not depend on (lower-bound y), but
;; does depend on (upper-bound y), and therefore is not a function only of the
;; widths of the intervals.
(mul-interval (make-interval 0 1) (make-interval 5 6)) ; (0 6)
(mul-interval (make-interval 0 1) (make-interval 2 6)) ; (0 6)
(mul-interval (make-interval 0 1) (make-interval 2 7)) ; (0 7)
