#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.07

(define (make-account balance password)
  (define (verify-password pw)
    (if (eq? pw password)
        #t
        (error "Incorrect password")))
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
  ;; Note I cleaned this up a bit from 3.03 which had a pretty silly
  ;; `verify-password-and`. If password verification is going to throw instead
  ;; of returning a type, we might as well just make it a separate method and
  ;; call it inline. Better still would be an either we could just foreach on,
  ;; or map over if we wanted to do some fancy monadic "description of
  ;; computation" stuff we'd only later interpret.
  (define (dispatch pw m)
    (cond ((eq? m 'verify-password)
           (verify-password pw))
          ((eq? m 'withdraw)
           (verify-password pw)
           withdraw)
          ((eq? m 'deposit)
           (verify-password pw)
           deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

;; Note this implementation breaks all joint accounts when the master password
;; is changed, but since make-account doesn't offer that functionality it's no
;; problem until product asks for it. 😂
(define (make-joint acc password joint-password)
  (acc password 'verify-password)
  (lambda (pw m)
    (if (eq? pw joint-password)
        (acc password m)
        (error "Incorrect password"))))

(define acc (make-account 100 'WATERMELON))
((acc 'WATERMELON 'withdraw) 10)
(define acc-joint (make-joint acc 'WATERMELON 'CHERRY))
((acc-joint 'CHERRY 'withdraw) 10)
