#lang sicp

(#%require "stream.scm")
(#%require "ex-3.70.scm")    ; weighted-pairs

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.72

;; Construct a stream of all numbers that can be written as the sum of two
;; squares in three different ways by weighting a stream of integer pairs by
;; the sum of their squares, then looking for three entries in a row with the
;; same sum of squares.
(define (sum-of-two-squares-three-ways)
  (define (sum-of-squares pair)
    (let ((i (car pair))
          (j (cadr pair)))
      (+ (* i i) (* j j))))
  (define (go s)
    (let ((i (stream-car s))
          (j (stream-car (stream-cdr s)))
          (k (stream-car (stream-cdr (stream-cdr s)))))
      (if (= (sum-of-squares i) (sum-of-squares j) (sum-of-squares k))
          (cons-stream (list (sum-of-squares i) i j k)
                       (go (stream-cdr s)))
          (go (stream-cdr s)))))
  (go (weighted-pairs integers integers sum-of-squares)))

(display-stream-next (sum-of-two-squares-three-ways) 5)
;; (325 (1 18) (6 17) (10 15))
;; (425 (5 20) (8 19) (13 16))
;; (650 (5 25) (11 23) (17 19))
;; (725 (7 26) (10 25) (14 23))
;; (845 (2 29) (13 26) (19 22))


