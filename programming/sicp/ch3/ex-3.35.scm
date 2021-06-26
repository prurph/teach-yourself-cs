#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.35

(#%require "constraint.scm")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a me)
  (connect b me)
  me)

;; Example
(define A (make-connector))
(define B (make-connector))
(probe "A" A)
(probe "B" B)
(squarer A B)

(set-value! A 5 'me)  ; A = 5, B = 25
(set-value! B 36 'me) ; Error! Contradiction
(forget-value! A 'me) ; A = ?, B = ?
(set-value! B 36 'me) ; A = 6, B = 36
