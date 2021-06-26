#lang sicp

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
;; An agenda is made up of time segments. Time segments are pairs consisting of
;; a number (the time) and a queue of procedures scheduled to be run during the
;; time segment.
(#%require "queue.scm")

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s)
  (car s))
(define (segment-queue s)
  (cdr s))

;; The agenda is a table of time segments, sorted in order of increasing time.
;; The current time is stored at the head of the agenda, therefore a new agenda
;; has no segments and a current time of 0.
(define (make-agenda)
  (list 0))
(define (current-time agenda)
  (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        cons ((make-new-time-segment time action) rest))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action)
                                    segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        ;; Whenever accessing the first item, update the current time, such that
        ;; the current time is always that of the most recently processed action.
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

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

(#%provide add-action!)
(#%provide after-delay)
(#%provide and-gate)
(#%provide get-signal)
(#%provide inverter)
(#%provide full-adder)
(#%provide make-wire)
(#%provide set-signal!)
