#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.65

(require "ex-2.63.scm")
(require "ex-2.64.scm")

;; General strategy: convert tree rep to list, then union/intersect, then
;; convert back. These conversions are O(n), so overall complexity is O(n).
(define tree->list tree->list-2)

(define (union-set set1 set2)
  ;; As in ex-2.62
  (define (union-set-list l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((= (car l1) (car l2)) (cons (car l1)
                                       (union-set-list (cdr l1) (cdr l2))))
          ((> (car l1) (car l2)) (cons (car l2)
                                       (union-set-list l1 (cdr l2))))
          ((< (car l1) (car l2)) (cons (car l1)
                                       (union-set-list (cdr l1) l2)))))
  (list->tree (union-set-list (tree->list set1)
                              (tree->list set2))))

(define (intersection-set set1 set2)
  (define (intersection-set-list l1 l2)
    (cond ((or (null? l1) (null? l2)) '())
          ((= (car l1) (car l2)) (cons (car l1)
                                      (intersection-set-list (cdr l1) (cdr l2))))
          ((> (car l1) (car l2)) (intersection-set-list l1 (cdr l2)))
          ((< (car l1) (car l2)) (intersection-set-list (cdr l1) l2))))
  (list->tree (intersection-set-list (tree->list set1)
                                     (tree->list set2))))

(define t1 (list->tree (list 1 3 5 7 9 11 100)))
(define t2 (list->tree (list 2 4 5 6 8 10 11 12)))

(tree->list (union-set t1 t2))
(tree->list (intersection-set t1 t2))
