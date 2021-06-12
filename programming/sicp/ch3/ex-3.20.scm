#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.20

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation: CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value) ((z 'set-car!) new-value))
(define (set-cdr! z new-value) ((z 'set-cdr!) new-value))

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
;; => 17

;; Barry Allison has published [excellent environment diagrams for this question](https://wizardbook.wordpress.com/2010/12/16/exercise-3-20/)
;; x is created in the global environment, as:
;; - pointer to the procedure body cons, parameters x, y
;; - an environment frame E1 with x: 1, y: 2, and the internal procedures
;;   set-x!, set-y!, and dispatch
;;
;; z is created in the global environment, identically to above
;; - it has its own environment frame E2, with x: (global) x, y: (global) x
;;
;; When calling (set-car! (cdr z) 17), first (dispatch 'cdr) from E2 is called
;; by creating a new frame E3, referencing dispatch from E2 and returning y from
;; E2, which is *also x from global*. Through a series of new environment frame
;; creations, (set-car! x 17) becomes ((dispatch 'set-car!) 17), with dispatch
;; from E1 above, which becomes (set-x! 17) from E1, updating x in E1, which is
;; a pointer to (car x) for the global x, so it becomes 17.
