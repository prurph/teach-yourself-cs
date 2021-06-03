#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.05

(require "monte-carlo.scm")

;; simply-scheme just has (random x) which returns an integer [0...x)
;; Also, it seems like (random (expt 2 32)) for 32-bit positive integer returns,
;; but never goes higher than 2^31, so you get a generator that never goes
;; higher than 0.5, ergo use 2^31.
(define (random-zero-to-one)
  (/ (random (expt 2 31)) (- (expt 2 31) 1.0)))

(define (random-range lo hi)
  (let ((range (- hi lo)))
    (+ lo (* (random-zero-to-one) range))))

(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (define (experiment)
    (predicate (random-range x1 x2)
               (random-range y1 y2)))
  ;; Multiply the area of the rectangle by the percentage of points inside the
  ;; region defined by the predicate.
  (* (- x2 x1) (- y2 y1) (monte-carlo trials experiment)))

;; Estimate pi by computing the integral of the unit circle, which has A = pi
(define (estimate-pi trials)
  (define (in-unit-circle? x y)
    (<= (+ (* x x) (* y y)) 1.0))
  (estimate-integral in-unit-circle? -1.0 -1.0 1.0 1.0 trials))


(estimate-pi 10000000)
;; A few iterations gave me: 3.1408316, 3.1412872, 3.1414088
