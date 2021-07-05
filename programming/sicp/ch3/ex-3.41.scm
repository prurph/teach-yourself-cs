#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.41

;; On the one hand, it's true that there is possible user confusion with the
;; account procedures. For example, a call to display the balance could be
;; interleaved with a deposit or withdrawal between its read and update state,
;; and so two successive calls to retrieve the balance might return different
;; amounts, even though no transaction has been dispatched between them. This
;; amounts to the user typing:
;;
;; (define acct (make-account 100))
;; (parallel-execute (lambda () (acct 'balance)
;;                   (lambda () (acct 'deposit 100))
;;
;; and sometimes seeing 100 and sometimes 200 for the balance. However, the
;; serializer does nothing to prevent this: its only guarantee is that procedures
;; will produce results as if they had run sequentially. It does not specify what
;; that sequence will be, and therefore there can be multiple correct results;
;; both 100 and 200 above would be correct, so serializing 'balance access does
;; not have an effect. In other words, regardless of when the account balance is
;; read, it represents a correct state of the account.
