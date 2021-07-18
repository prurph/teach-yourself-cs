#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.70

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (cons-stream s1car
                                (merge-weighted (stream-cdr s1)
                                                s2
                                                weight)))
                  ((> (weight s1car) (weight s2car))
                   (cons-stream s2car
                                (merge-weighted s1
                                                (stream-cdr s2)
                                                weight)))
                  (else
                    (cons-stream s1car
                                 (merge-weighted (stream-cdr s1)
                                                 (stream-cdr s2)
                                                 weight))))))))

(define (weighted-pairs s t weight)
  (let ((p (pairs s t)))
    (merge-weighted p p weight)))

;; 3.70(a) Stream of all pairs of positive integers i <= j ordered according to
;; i + j.
(display-stream-next
  (weighted-pairs integers
                  integers
                  (lambda (x) (apply + x)))
  10)

;; (1 1)
;; (1 2)
;; (2 2)
;; (1 3)
;; (2 3)
;; (1 4)
;; (3 3)
;; (1 5)
;; (2 4)
;; (1 6)

;; 3.70(b) Stream of all pairs of positive integers i <= j where neither i nor
;; j is divisible by 2, 3 or 5, and the pairs are ordered according to
;; 2i + 3j + 5ij
(define not-divisible-by-2-3-5
  (stream-filter (lambda (x) (not (or (zero? (modulo x 2))
                                      (zero? (modulo x 3))
                                      (zero? (modulo x 5)))))
                 integers))

(display-stream-next
  (weighted-pairs not-divisible-by-2-3-5
                  not-divisible-by-2-3-5
                  (lambda (x) (+ (* 2 (car x))
                                 (* 3 (cadr x))
                                 (* 5 (car x) (cadr x)))))
  10)

;; (1 1)
;; (1 7)
;; (7 7)
;; (1 11)
;; (7 11)
;; (1 13)
;; (11 11)
;; (1 17)
;; (7 13)
;; (1 19)
