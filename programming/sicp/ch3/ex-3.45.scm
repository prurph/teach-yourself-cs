#lang sicp

;; Mega-annoyingly we need SICP to be the last import or sicp-concurrency pulls
;; in the Racket cond that doesn't allow expressions in the else clause.
(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))
(#%require (planet neil/sicp))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.45

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; In this version of make-account-and-serializer withdraw and deposit are
;; already serialized, but this creates a problem with serialized-exchange
;; above. It serializes exchange based on the accounts serializers, but exchange
;; then calls withdraw and deposit, which themselves use those same serializers.
;; As a result the outer calls from serialized-exchange are waiting on results
;; from inner calls to withdraw and deposit that cannot complete because they
;; are waiting on the serializers. Deadlock ensues. Specifically the withdraw
;; from account1 cannot complete.
