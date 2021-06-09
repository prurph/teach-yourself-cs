#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-21.html#%_thm_3.10

;; global environment:
;;   - make-withdraw
;;       - parameters: initial-amount
;;       - body:
           ((lambda (balance)
              (lambda (amount)
                (if (>= balance amount)
                    (begin (set! balance (- balance amount))
                           balance)
                    "Insufficient funds")))
            initial-amount)

;; (define W1 (make-withdraw 100)) 
;;   - creates a frame E1
;;       - initial-amount: 100
;;   - evaluate body of make-withdraw, which invokes the lambda above
;;       - creates a new frame E2
;;           - balance: 100 (from initial-amount)
;;           - parent environment is E1
;;           - body
               (lambda (amount)
                 (if (>= balance amount)
                     (begin (set! balance (- balance amount))
                            balance)
                     "Insufficient funds")) 
;;           - W1 exists in global-env with the above body and its environment as E2

;; (W1 50)
;;   - creates a new frame E3
;;       - amount: 50
;;       - body: as above from W1
;;       - parent environment is E2
;;           - balance is found in E2 
;;           - initial-amount still exists in E1, but is not used
;;           - E1 will not be GC'd because E2 still references it

;; (define W2 (make-withdraw 100))
;;   - creates a new frame E4
;;       - initial-amount: 100
;;   - evaluate body of make-withdraw, as above
;;       - creates a new frame E5
;;           - balance: 100
;;           - parent environment is E4
;;   - E1/E2 for W1 are totally segregated from E4/E5 for W2, even though both
;;     pairs contain bindings for balance and initial-amount, respectively

;; Using `let` here to create a local binding between balance and initial-amount
;; vs simply referencing the parameter directly gives the same behavior but
;; creates an extra frame to capture that binding. 
