#lang scheme

;; Implementations based on lists with duplicates

;; element-of-set
;; - Without duplicates: O(n)
;; - With duplicates:    O(n), but n can in theory be infinitely larger for a
;;                             set with the same elements
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

;; adjoin-set
;; - Without duplicates: O(n), must check if x is an element of the set already
;; - With duplicates:    O(1)
;; Other interesting points from Drew Hess: implementing this with `append` is
;; likely slower since it would need to find the end of the list, as well as
;; create a copy of the list, whereas this cons version can avoid it by sharing
;; the tail structure.
(define (adjoin-set x set)
  (cons x set))

;; union-set
;; - Without duplicates: O(n*m), must check if each element in s1 is in s2
;; - With duplicates:    O(n), must copy/cons down set1
(define (union-set set1 set2)
  (append set1 set2))

;; intersection-set
;; - Without duplicates: O(n*m), must check if each element in s1 is in s2
;; - With duplicates:    O(n*m), still must check if each s1 in s2
;; This removes duplicates from set2, but keeps any that appear in set1
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; In general this representation would be preferred for cases where adjoin-set
;; is the most frequent operation, and/or where union-set is common and sets
;; are not overly long, otherwise O(n) on append could end up being slower than
;; O(n*m) if there are many duplicates.

