#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.16

;; Incorrect count-pairs
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define count-3 (list 1 2 3))

;; This is only three pairs, but the l1 reference is double-counted.
;; When drawing these, I find it easiest just to just remember a cons is a box
;; of car and cdr. Sometimes the cdr is nil for a regular list. Sounds obvious,
;; but otherwise it's easy to get confused here; (cons l1 l1) is one box, where
;; the car and cdr both point to l1.
(define count-4
  (let ((l1 (list 4)))
    (cons 1 (cons l1 l1))))
;; 
;;              ┌───┬───┐    ┌───┬───┐
;; count-4 ─────▶ ● │   ├────▶ ● │   │
;;              └─┬─┴───┘    └─┬─┴─┬─┘
;;                │            │   │  
;;              ┌─▼─┐        ┌─▼─┬─▼─┐
;;              │ 1 │        │ ● │ / │
;;              └───┘        └─┬─┴───┘
;;                             │      
;;                           ┌─▼─┐    
;;                           │ 4 │    
;;                           └───┘

;; This is only three pairs, but the l1 reference is double-counted, twice.
(define count-7
  (let* ((l1 (list 7))
         (l2 (cons l1 l1)))
    (cons l2 l2)))
;;
;;              ┌───┬───┐                               
;; count-7 ─────▶ ● │   │  the car and cdr of count-inf are both l2 
;;              └─┬─┴─┬─┘                               
;;                │   │                                 
;;              ┌─▼─┬─▼─┐                               
;;      l2 ─────▶ ● │   │  the car and cdr of l2 are both l1 
;;              └─┬─┴─┬─┘                               
;;                │   │                                 
;;              ┌─▼─┬─▼─┐                               
;;      l1 ─────▶ ● │ / │                               
;;              └─┬─┴───┘                               
;;                │                                     
;;              ┌─▼─┐                                   
;;              │ 7 │                                   
;;              └───┘

;; Cycle
(define count-inf
  (let ((l1 (list 'c 'a 't)))
    (set-cdr! (cdddr l1) l1)
    l1))
;;
;;                  ┌─────────────────────────────┐  
;;                  │                             │  
;;                ┌─▼─┬───┐    ┌───┬───┐    ┌───┬─┴─┐
;; count-inf ─────▶ ● │   ├────▶ ● │   ├────▶ ● │   │
;;                └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘
;;                  │            │            │      
;;                ┌─▼─┐        ┌─▼─┐        ┌─▼─┐    
;;                │ c │        │ a │        │ t │    
;;                └───┘        └───┘        └───┘
