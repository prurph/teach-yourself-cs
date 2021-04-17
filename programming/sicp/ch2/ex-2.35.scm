#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.35

(load "accumulate.scm")

;; Original count-leaves:
(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

;; Implementation using accumulate.
;; This is tricky: the accumlation is just summing the count of leaves from each
;; sub-tree. Trees are represented by nested lists, so if an elemnt is a pair,
;; it's a sub-tree and we recurse, if it is not, it is a leaf.
(define (count-leaves t)
  (accumulate + 0 (map (lambda (t) 
                         (if (pair? t)
                             (count-leaves t)
                             1))
                       t)))

;; Remember the way to "read" these as trees is (pairs as internal nodes),
;; primitives as leaves:
;;         ((1 2) 3 4)   
;;       ┌──────●──────┐ 
;;       │      │      │ 
;;       │      │      │ 
;; (1 2) │      │      │ 
;;     ┌─●─┐    │      │ 
;;     │   │    3      4 
;;     │   │             
;;     1   2
(define t1
  (cons (list 1 2) (list 3 4))) ; (cons a (list b c)) is (list a b c)
(count-leaves t1)
;; => 4

;;  (1 (2 (3 4)))              
;; ┌──────●─────┐              
;; │            │              
;; │            │              
;; │            │(2 (3 4))     
;; │        ┌───●───┐          
;; 1        │       │(3 4)     
;;          │     ┌─●─┐        
;;          2     │   │        
;;                │   │        
;;                3   4
(define t2
  (list 1 (list 2 (list 3 4))))
(count-leaves t2)
;; => 4

