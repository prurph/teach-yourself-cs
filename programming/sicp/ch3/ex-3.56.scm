#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.56

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car
                                (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car
                                (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1) (stream-cdr s2)))))))))

;; The ascending stream of positive integers with no prime factors other than
;; 2, 3, or 5. As per the book:
;;
;; - S begins with 1
;; - Elements of `(scale-stream S 2)` are also elements of S
;; - Elements of `(scale-stream S 3)` are also elements of S
;; - Elements of `(scale-stream S 5)` are also elements of S
;; - These are all the elements of S
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
