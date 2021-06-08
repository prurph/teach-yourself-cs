#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.11

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; Not going to draw out the entire environment structure. Descriptions below.

(define acc (make-account 50))
;; - make-account exists in the global env
;; - when it is called with 50, E1 is created with
;;   - balance: 50
;;   - procedures withdraw, deposit, and dispatch
;; - acc is then defined in the global environment with the return of make-account
;;   - note this is the procedure body of dispatch, which has env E1
;;   - that env contains bindings for balance, withdraw, and deposit

((acc 'deposit) 40)
;; - acc is evaluated in an environment E2 (parent E1, from dispatch) with:
;;   - m: 'deposit
;; - this results in the procedure deposit, to which 40 is applied
;; - for this application, an environment E3 (parent E1) is created with
;;   - amount: 40
;;   - the procedure body references balance, and changes its value in E1 to 90

((acc 'withdraw) 60)
;; - acc is evaluated in an environment E4 (parent E1, from dispatch) with:
;;   - m: 'withdraw
;; - this results in the procedure withdraw, to which 60 is applied
;; - for this application, an environment E5 (parent E1) is created with
;;   - amount: 60
;;   - the procedure body references balance, from E1, and changes its value in E1 to 30

;; As demonstrated above, the local state for acc is kept in E1, the environment
;; created from the application of 50 to make-account.

;; If a new account is defined, a new environment will be created, with balance
;; bound within it. This balance is distinct from the first account's balance,
;; stored in E1.

;; The shared environment structure between two accounts is dependent on the
;; interpreter, but in theory the body of `make-account` and its internal
;; procedures withdraw, deposit, and dispatch could all be shared between
;; accounts.

