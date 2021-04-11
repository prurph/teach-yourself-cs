#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#%_thm_2.13

;; Express tolerance of product of two intervals in terms of their individual
;; tolerances. Assume all numbers are positive, which allows the simplification:
(define (mul-interval x y)
  (make-interval (* (lower-bound x) (lower-bound y))
                 (* (upper-bound x) (upper-bound y))))
;; Expressing bounds in terms of tolerance and center the result becomes, since
;; tolerances are a percent:
;;   lower: (cx - cxtx)(cy - cyty)
;;   upper: (cx + cxtx)(cy + cyty)
;; Rearranging gives forms like this for the upper bound:
;;   cx(1 + tx)cy(1 + ty) = cxcy(1 + tx + ty + txty)
;; Since tolerances are small percentage-wise, txty is negligible:
;;   cxcy(1 + tx + ty) = cxcy + cxcy(tx + ty)
;; The absolute delta is thus cxcy(tx + ty), so the percentage tolerance is
;; tx + ty, or the sum of the individual percent tolerances.
