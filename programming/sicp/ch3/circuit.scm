#lang racket

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

;; *** Wire
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire value) ((wire 'set-signal!) value))
;; proc is a procedure of no arguments that should be run whenever the signal
;; on the wire changes value
(define (add-action! wire proc) ((wire 'add-action!) proc))

;; *** Agenda
;; A schedule of things to do. Allows capturing of delays for wires and signals.
(define (make-agenda) (error "Not implemented"))
(define (empty-agenda? agenda) (error "Not implemented"))
(define (first-agenda-item agenda) (error "Not implemented"))
(define (remove-first-agenda-item! agenda) (error "Not implemented"))
(define (add-to-agenda! time action agenda) (error "Not implemented"))
(define (current-time agenda) (error "Not implemented"))
;; The book defines these directly on a global agenda, "the-agenda"
(define (the-agenda) (make-agenda))
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda) action the-agenda)))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

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
