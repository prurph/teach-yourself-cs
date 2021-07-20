#lang sicp

;; NB: this must be run with dyoo/simply-scheme or you'll get "cannot use before
;; initialization".
(#%require (planet "dyoo/simply-scheme"))
(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.78

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt) int))))
  int)

;; Solving d²y/dt² - a dy/dt - by = 0, outputting the stream y
;; Parameters:
;; - a, b, dt: constants
;; - y0:       initial value for y
;; - dy0:      initial value for dy/dt
(define (solve-2nd a b dt y0 dy0)
  ;; Integrate dy to get y
  (define y (integral (delay dy) y0 dt))
  ;; Integrate ddy to get dy
  (define dy (integral (delay ddy) dy0 dt))
  ;; ddy is a dy/dt + by, from the initial equation
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)
