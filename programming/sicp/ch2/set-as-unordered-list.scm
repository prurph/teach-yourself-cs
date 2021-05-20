#lang scheme

;; Operations on sets represented as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; Note this is a streamlined implemntation that only calls `element-of-set?`
;; once per element in set1 by using `cons` to construct the intersection
;; directly if it is. An alternate implementation could use adjoin-set instead.
(define (intersection set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))

;; Implemnted in ex-2.59.scm
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(provide element-of-set?)
(provide adjoin-set)
(provide intersection)
(provide union-set)
