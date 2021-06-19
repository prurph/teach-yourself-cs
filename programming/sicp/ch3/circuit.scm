#lang racket

(define (make-wire) (error "Not implemented"))
(define (get-signal wire) (error "Not implemented"))
(define (set-signal! wire value) (error "Not implemented"))
;; proc is a procedure of no arguments that should be run whenever the signal
;; on the wire changes value
(define (add-action! wire proc) (error "Not implemented"))
(define (after-delay delay proc)
  (sleep delay)
  (proc))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal: LOGICAL-NOT" s))))

(define (logical-and s1 s2)
  (cond ((not (and (memq s1 (list 0 1))
                   (memq s2 (list 0 1))))
         (error "Invalid signals: LOGICAL-AND" s1 s2))
        ((and (= s1 1) (= s2 1)) 1)
        (else 0)))

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
  (add-action! o2 or-gate-procedure) 
  'ok)

(define and-gate-delay 1)
(define (and-gate a1 a2 output) 
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define inverter-delay 1)
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

;; A half adder of input wires a and b, and output wires s and c.
;; - s: 1 whenever exactly 1 of a and b is 1
;; - c: 1 when both a and b are 1
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(provide (all-defined-out))
