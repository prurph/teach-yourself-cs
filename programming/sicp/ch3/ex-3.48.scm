#lang sicp

;; Mega-annoyingly we need SICP to be the last import or sicp-concurrency pulls
;; in the Racket cond that doesn't allow expressions in the else clause.
(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))
(#%require (planet neil/sicp))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.48

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; ORIGINAL serialized-exchange. It can deadlock if concurrent calls to exchange
;; a1 ⭤ a2 and a1 ⭤ a2 since each call will be waiting on the other's serializer.
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange)) account1 account2)))

;; NEW serialized-exchange. Avoids deadlock because for a given call to
;; serialized-exchange serializer1 and serializer2 will be consistent regardless
;; of the order of account parameters. Thus there is no circular dependency;
;; e.g. a1's serializer will always require the result of a2's, and never vice-
;; versa.
(define (serialized-exchange-safe account1 account2)
  (if (< (account1 'account-number) (account2 'account-number))
      (serialized-exchange account1 account2)
      (serialized-exchange account2 account1)))

;; It's better yet to remove the unsafe serialized-exchange and just provide the
;; following. The order of the account parameters in the final invocation
;; doesn't matter, only that the serializers are consistent: in this case the
;; outer serializer always belongs to the lower numbered account.
(define (serialized-exchange-safe2 account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'account-number) (account2 'account-number))
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account2 account1))))

;; The above requires the following modification to make-account to create
;; accounts with numbers.
(define (make-account-and-serializer balance account-number)
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
            ((eq? m 'account-number) account-number)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))
