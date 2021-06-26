#lang sicp

;; A constraint system built of "connectors". When a connector's value is changed
;; it determines any possible values--that is, those for which it has enough
;; information to compute--and propagates them throughout the system.
;;
;; The book's example is temperature conversion: 9C = 5(F - 32) represented as
;; a network of primitive adder, multiplier, and constant constraints. When a
;; value is set for C, for example, 9 * C is propagated, ultimately causing F
;; to be set.

;; An adder constraint: summand connectors a1 and a2 must add to connector sum.
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2)) me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1)) me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;; A multiplier constraint: connectors m1 and m2 must multipliy to connector
;; product.
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2 (/ (get-value product) (get-value m1)) me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1 (/ (get-value product) (get-value m2)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;; A constant constructor: no messages may be sent to it.
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;; Probes print messages about a connector being set or unset. Note this
;; responds to the same messages as any other connector. Cool!
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name) (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

;; Connectors communicate by informing about new and lost (unset) values.
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


;; A connector is a procedural object with local state variables:
;; - value: the current value of the connector
;; - informant: the object that set the connector's value
;; - constrants: a list in which the connector participates
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor inform-about-no-value constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? exception (car items)) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(#%provide adder)
(#%provide multiplier)
(#%provide constant)
(#%provide probe)
(#%provide inform-about-value)
(#%provide inform-about-no-value)
(#%provide make-connector)
(#%provide for-each-except)
(#%provide has-value?)
(#%provide get-value)
(#%provide set-value!)
(#%provide forget-value!)
(#%provide connect)

;; Example
(define C (make-connector))
(define F (make-connector))
(define (celsius-fahrenheit-coverter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))
(celsius-fahrenheit-coverter C F)
(probe "Celsius" C)
(probe "Fahrenheit" F)