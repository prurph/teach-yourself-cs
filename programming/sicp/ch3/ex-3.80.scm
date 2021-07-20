#lang sicp

;; NB: this must be run with dyoo/simply-scheme or you'll get "cannot use before
;; initialization".
(#%require (planet "dyoo/simply-scheme"))
(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.80

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt) int))))
  int)

(define (RLC R L C dt)
  (define (circuit-state vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream iL (/ R L -1))
                             (scale-stream vC (/ 1 L))))
    (cons vC iL))
  circuit-state)

(define circuit (RLC 1 1 0.2 0.1))
(define states (circuit 10 0))

;; Capacitor voltages
(display-stream-next (car states) 5)
;; 10
;; 10
;; 9.5
;; 8.55
;; 7.220000000000001


;; Inductor currents
(display-stream-next (cdr states) 5)
;; 0
;; 1.0
;; 1.9
;; 2.66
;; 3.249
