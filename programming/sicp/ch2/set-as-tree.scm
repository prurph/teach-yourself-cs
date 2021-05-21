#lang scheme

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))
        (else #t)))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))
        (else set)))

(provide entry)
(provide left-branch)
(provide right-branch)
(provide make-tree)
(provide element-of-set?)
(provide adjoin-set)
