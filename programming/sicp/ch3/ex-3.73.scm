#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.73

;; Model an RC circuit. Inputs:
;; - R:  resistance of the resistor
;; - C:  capacitance of the capacitor
;; - dt: time-step
;; Output: a procedure that takes a stream of the input current i, an initial
;; value for the capacitor voltage v0, and produces a stream of voltages v
;;
;; The formula is v = v0 + (1/C)∫ idt + Ri for the integral over 0 to t

;; Compute the integral by taking an input stream integrand, and accumulating
;; its sum over small increments dt, then adding to the initial value.
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (voltage i v0)
    (cons-stream v0
                 (add-streams (integral (scale-stream i (/ 1 C)) v0 dt)
                   (scale-stream i R))))
  voltage)

(define RC1 (RC 5 1 0.5))

(display-stream-next (RC1 (list->stream (list 0 1 2 3 2 1 0)) 0.25) 5)

