#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.13

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; In the repl z is shown as the following. Cool way to represent the cycle!
;;   #0=(mcons 'a (mcons 'b (mcons 'c #0#)))
;;
;;           ┌─────────────────────────────────┐
;;           │                                 │
;;         ┌─▼─┬───┐    ┌───┬───┐    ┌───┬───┐ │
;;  z ─────▶ ● │   ├────▶ ● │   ├────▶ ● │   ├─┘
;;         └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘  
;;           │            │            │        
;;         ┌─▼─┐        ┌─▼─┐        ┌─▼─┐      
;;         │ a │        │ b │        │ c │      
;;         └───┘        └───┘        └───┘      
;;
;; Calling last-pair b creates an infinite--wait for it--cycle.
