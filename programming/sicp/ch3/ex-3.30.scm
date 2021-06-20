#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.30

(#%require "circuit.scm")

;; Adds numbers represented by a-bits and b-bits, outputing them on the wires
;; sum-bits and carry-bit.  Assumes equal length a-bits b-bits sum-bits
;; (checking this would be trivial).
(define (ripple-carry-adder a-bits b-bits sum-bits carry-bit)
  (if (null? (cdr a-bits))
      (full-adder (car a-bits)
                  (car b-bits)
                  (car sum-bits)
                  carry-bit)
      (ripple-carry-adder (cdr a-bits)
                          (cdr b-bits)
                          (cdr sum-bits)
                          (full-adder (car a-bits)
                                      (car b-bits)
                                      (car sum-bits)
                                      (make-wire)))))

;; The delay to obtain the complete output from an n-bit ripple-carry adder is:
;;
;;   n(full-adder-delay) = n(2half-adder-delay + or-gate-delay)
;;
;; Expanding half-adder-delay:
;;
;;   half-adder-delay = max(2and-gate-delay + inverter-delay, and-gate-delay + or-gate-delay)
;;   
;; Substituting:   
;;
;;   n(full-adder-delay) = n(2max(2and-gate-delay + inverter-delay, and-gate-delay + or-gate-delay) + or-gate-delay)
;;                       = n(max(4and-gate-delay + 2inverter-delay + or-gate-delay,
;;                               2and-gate-delay + 3or-gate-delay))
