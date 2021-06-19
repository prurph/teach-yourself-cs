#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.29

(#%require "circuit.scm")

(define (logical-or s1 s2)
  (cond ((not (and (memq s1 (list 0 1))
                   (memq s2 (list 0 1))))
         (error "Invalid signals: LOGICAL-OR" s1 s2))
        ((or (= s1 1) (= s2 1)) 1)
        (else 0)))

(define or-gate-delay 1)

;; An or-gate constructed from and-gates and inverters.
;; a || b === !(!a && !b)
(define (or-gate o1 o2 output)
  (let ((not-o1 (make-wire)) (not-o2 (make-wire)) (and-out (make-wire)))
    (inverter o1 not-o1)
    (inverter o2 not-o2)
    (and-gate not-o1 not-o2 and-out)
    (inverter and-out output)
    'ok))

;; The delay of this version versus a "normal" or-gate is:
;;   inverter-delay + and-gate-delay + inverter-delay = (2 * inverter) + and
;; Inversion of o1 and o2 can happen in parallel, so whether one or both is
;; flipped, it takes an inverter-delay for that to percolate to the and-gate,
;; then the and-gate-delay, then the final inverter-delay to invert the result
;; of the and gate.
