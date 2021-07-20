#lang sicp

;; NB: this must be run with dyoo/simply-scheme or you'll get "cannot use before
;; initialization".
(#%require (planet "dyoo/simply-scheme"))
(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.79

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt) int))))
  int)

;; Solving general second order differential equations d²y/dt² = f(dy/dt, y)
;; Parameters:
;; - a, b, dt: constants
;; - y0:       initial value for y
;; - dy0:      initial value for dy/dt
(define (solve-2nd f y0 dy0 dt)
  ;; Integrate dy to get y
  (define y (integral (delay dy) y0 dt))
  ;; Integrate ddy to get dy
  (define dy (integral (delay ddy) dy0 dt))
  ;; Now f is a function that takes dy and y
  (define ddy (stream-map f dy y))
  y)
