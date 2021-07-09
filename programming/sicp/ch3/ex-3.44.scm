#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.44

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;; Defining transfer in this way is acceptable provided from-account and
;; to-account have serialized withdraw and deposit procedures. The reason is
;; that each account is only accessed once, whereas with the exchange procedure
;; accounts were accessed first to calculate the difference between them, and
;; then a second time to withdraw and deposit. We saw this could create issues
;; with concurrent exchanges where the reads were interleaved with pending
;; updates and didn't see the updated balance, however the total was always
;; preserved. Here, however, that is impossible since amount is passed in and
;; does not require any calculation by accessing the values, so as long as
;; withdraw and deposit happen transactionally--that is it is impossible for
;; there to be a crash between the two procedure calls in transfer--this is
;; sufficient.
