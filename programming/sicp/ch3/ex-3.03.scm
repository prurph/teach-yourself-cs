#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.03

(define (make-account balance password)
  (define (verify-password-and pw f) 
    (lambda (arg)
      (if (eq? pw password)
          (f arg)
          (error "Invalid password"))))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;; Could also verify password first for any request, however doing it this
  ;; way decouples verification from dispatch so that we could have methods that
  ;; don't require verification.
  (define (dispatch pw m)
    (cond ((eq? m 'withdraw) (verify-password-and pw withdraw))
          ((eq? m 'deposit) (verify-password-and pw deposit))
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100 'WATERMELON))
((acc 'WATERMELON 'withdraw) 10)
((acc 'CHERRY 'withdraw) 10)
((acc 'WATERMELON 'withdraw) 100)
((acc 'CHERRY 'deposit) 1000000000)
((acc 'WATERMELON 'deposit) 1000000000)
