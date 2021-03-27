;; interval.scm
;; Abstractions for working with intervals that represent the range of possible
;; values of an inexact quantity.


(define (make-interval a b) (cons a b))

;; upper-bound and lower-bound are implemented as part of ex-2.07
(define (upper-bound i)
  (car i))
(define (lower-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;; sub-interval is defined as part of ex-2.08
(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
;; division as multiplying by reciprocal, switching upper and lower bounds
(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))


