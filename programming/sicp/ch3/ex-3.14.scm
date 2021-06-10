#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; Mystery reverses a list in place and returns a reference to the new head.
;; Since this is a mutation, the input effectively becomes a pointer to the
;; tail "node".
(define v (list 'a 'b 'c 'd))

;;        ┌───┬───┐    ┌───┬───┐    ┌───┬───┐    ┌───┬───┐
;; v ─────▶ ● │   ├────▶ ● │   ├────▶ ● │   ├────▶ ● │ / │
;;        └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘
;;          │            │            │            │      
;;        ┌─▼─┐        ┌─▼─┐        ┌─▼─┐        ┌─▼─┐    
;;        │ a │        │ b │        │ c │        │ d │    
;;        └───┘        └───┘        └───┘        └───┘

(define w (mystery v))
w ; => ('d 'c 'b 'a) reversed in place, pointer to new head
v ; => ('a)          old head is now the tail (its cdr is now nil)

;;                                                 w      
;;                       ┌────────────────┐        │      
;;          ┌────────────┼───┐        ┌───┼────────┼───┐  
;;        ┌─▼─┬───┐    ┌─▼─┬─┴─┐    ┌─▼─┬─┴─┐    ┌─▼─┬─┴─┐
;; v ─────▶ ● │ / │    │ ● │   │    │ ● │   │    │ ● │   │
;;        └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘
;;          │            │            │            │      
;;        ┌─▼─┐        ┌─▼─┐        ┌─▼─┐        ┌─▼─┐    
;;        │ a │        │ b │        │ c │        │ d │    
;;        └───┘        └───┘        └───┘        └───┘
