#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.12

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z
;; => (a b c d)
(cdr x)
;; (b)

;; The append method does not mutate x, so its cdr doesn't change; it is still
;; b. Remember it is implemented as successive consing the elements of x onto
;; y. This means it maintains a pointer to y, but not to any of x. If we were
;; to mutate x, z wouldn't change, but y would.

;;         ┌───┬───┐    ┌───┬───┐          ┌───┬───┐    ┌───┬───┐
;;  x ─────▶ ● │   ├────▶ ● │ / │   y ─────▶ ● │   ├────▶ ● │ / │
;;         └─┬─┴───┘    └─┬─┴───┘    ┌─────▶─┬─┴───┘    └─┬─┴───┘
;;           │            │          │       │            │      
;;         ┌─▼─┐        ┌─▼─┐        │     ┌─▼─┐        ┌─▼─┐    
;;         │ a │        │ b │        │     │ c │        │ d │    
;;         └───┘        └───┘        │     └───┘        └───┘    
;;                                   │                           
;;         ┌───┬───┐    ┌───┬───┐    │                           
;;  z ─────▶ ● │   ├────▶ ● │   ├────┘                           
;;         └─┬─┴───┘    └─┬─┴───┘                                
;;           │            │                                      
;;         ┌─▼─┐        ┌─▼─┐                                    
;;         │ a │        │ b │                                    
;;         └───┘        └───┘

(define w (append! x y))
w
;; => (a b c d)
(cdr x)
;; => (b c d)

;; The append! method mutates x. Its last pair, (cons 'b '()), has its cdr
;; set to (c d). Therefore x becomes (cons 'a (cons 'b (cons 'c (cons 'd '())).
;; Its cdr is thus (b c d).

;;       x ──┐                     y ──┐                   
;;           │                         │                   
;;         ┌─▼─┬───┐    ┌───┬───┐    ┌─▼─┬───┐    ┌───┬───┐
;;  w ─────▶ ● │   ├────▶ ● │   ├────▶ ● │   ├────▶ ● │ / │
;;         └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘
;;           │            │            │            │      
;;         ┌─▼─┐        ┌─▼─┐        ┌─▼─┐        ┌─▼─┐    
;;         │ a │        │ b │        │ c │        │ d │    
;;         └───┘        └───┘        └───┘        └───┘
