#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.46

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true) false)))

;; If the two operations--test (car cell) and set (set-car!)--in the above
;; procedure are not atomic, then two calls to test-and-set! can return false
;; since neither has yet set the cell successfully. This means both callers
;; will think they have acquired the lock.

;;       .─────────.            .─────────.           .─────────.       
;;      (    P1     )          (   cell    )         (    P2     )      
;;       `─────────'            `─────────'           `─────────'       
;;                                                                      
;;                               ┌───────┐                              
;;             ┌─────────────────│ false │──────────────────┐           
;;             │                 └───────┘                  │           
;;             ▼                                            │           
;; ┌──────────────────────┐                                 │           
;; │ (car cell): #f       │                                 ▼           
;; └──────────────────────┘                     ┌──────────────────────┐
;;             │                                │ (car cell): #f       │
;;             ▼                                └──────────────────────┘
;; ┌──────────────────────┐      ┌───────┐                  │           
;; │ (set-car! cell true) │─────▶│ true  │                  │           
;; └──────────────────────┘      └───────┘                  │           
;;                                                          │           
;;                                                          ▼           
;;                               ┌───────┐      ┌──────────────────────┐
;;                               │ true  │◀─────│ (set-car! cell true) │
;;                               └───────┘      └──────────────────────┘
