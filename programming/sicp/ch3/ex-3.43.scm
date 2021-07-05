#lang sicp

(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.43

;; If the exchange processes are sequential, any exchange atomically swaps the
;; balances of two accounts, therefore there is no way for any of the three
;; accounts to end up with a value other than one of the starting balances
;; following a sequence of concurrent exchanges.

;; Considering the non-threadsafe first version of exchange, the above could be
;; violated as shown:
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; While an exchange between acct2 and acct3 is underway, a concurrent exchange
;; between acct1 and acct3 executes, reading the value of acct3 that has yet
;; to be updated from the first exchange.
;;
;; ┌ ─ ─ ─ ─                         ┌ ─ ─ ─ ─                         ┌ ─ ─ ─ ─ 
;;  acct1 30│ (exchange acct1 acct3)  acct2 20│ (exchange acct2 acct3)  acct3 10│
;; │                                 │                                 │         
;;          │                                 │                                 │
;; │                                 │                                 │         
;;          │                               10│   (acc2 'withdraw 10)           │
;; │                                 │                                 │         
;;        10│   (acc1 'withdraw 20)           │                                 │
;; │            (acc3 'deposit 20)   │                                 │      30 
;;          │                                 │   (acc3 'deposit 10)          40│
;; └ ─ ─ ─ ─                         └ ─ ─ ─ ─                         └ ─ ─ ─ ─ 
;; ┌────────┐                        ┌────────┐                        ┌────────┐
;; │end:  10│                        │end:  10│                        │end:  40│
;; └────────┘                        └────────┘                        └────────┘

;; The sum of the balances will always be preserved, because the individual
;; withdraw and deposit procedures are serialized, meaning all updates to a
;; balance will be in an amount represented by a `difference` between the two
;; values, preserving the total.
;;
;; If transactions on individual accounts were not serialized, it would be
;; possible for the sum to change. For example, the following could happen in
;; the above diagram for P1 (acc3 'deposit 20) and P2 (acc3 'deposit 10)
;;   acct3's balance is 10
;;   P1 reads 10 and will next update the balance to 30
;;   P2 reads 10 and sets to 20
;;   P1 updates to 20
;;   acct3's balance is now 30 when it should be 40, losing 10 from the total
