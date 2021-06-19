#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.28

(#%require "circuit.scm")

(define (logical-or s1 s2)
  (cond ((not (and (memq s1 (list 0 1))
                   (memq s2 (list 0 1))))
         (error "Invalid signals: LOGICAL-OR" s1 s2))
        ((or (= s1 1) (= s2 1)) 1)
        (else 0)))

(define or-gate-delay 1)

(define (or-gate o1 o2 output)
  (define (or-gate-procedure)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! o1 or-gate-procedure)
  (add-action! o1 or-gate-procedure) 
  'ok)
