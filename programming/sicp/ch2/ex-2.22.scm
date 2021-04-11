#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.22

;; This iterative definition reverses the list because the list is iterated
;; through from head to tail, but each successive square is prepended to the
;; accumulator, so the first processed element (the original head) ends up as
;; the last element in the accumulator, and vice versa.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items nil))

;; This iterative definition is all kinds of weird because the answer is created
;; with a flipped argument order from the usual: in this case cons is called
;; with a pair as its first value, and the second is a primitive value:
;;   (square-list (list 1 2 3))
;;   => (cons (cons (cons '() 1) 4) 9)
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))
