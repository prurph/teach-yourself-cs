#lang sicp


;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.15

;; Consider the proposed example, where `halts?` exists, and corectly
;; determines if p halts on a.
(define (halts? p a) #t)

;; This would allow us to write:
(define (run-forever) (run-forever))
(define (try p)
  (if halts? p p) (run-forever 'halted))

;; Now evaluating (try try)
(try try)
((if halts? try try) (run-forever) 'halted)
;; One outcome is that halts? returns true, and we run forever. halts? is
;; therefore wrong: (try try) does not in fact halt.
;;
;; The other outcome is that halts? returns false, and so we return the value
;; 'halted. halts? is again wrong: we did halt.
;;
;; Consequently it is impossible to write the generalized procedure halts?
