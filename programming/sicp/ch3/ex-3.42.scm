#lang sicp

;; Mega-annoyingly we need SICP to be the last import or sicp-concurrency pulls
;; in the Racket cond that doesn't allow expressions in the else clause.
(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))
(#%require (planet neil/sicp))

;; The change is from creating a serialized procedure with each dispatch call
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))


;; To createing the serialized procedures beforehand and always using that same
;; procedure for each message type.
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else
                (error "Unknown request: MAKE-ACCOUNT"
                       m))))
      dispatch)))

;; This is a safe change and doesn't alter the behavior at all because the same
;; serializer is always used for any call to withdraw or deposit. Any procedures
;; created by the same serializer will never be interleaved, regardless of how
;; they are created: for each dispatch, or ahead of time.
