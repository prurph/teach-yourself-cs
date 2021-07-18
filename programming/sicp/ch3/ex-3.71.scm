#lang sicp

(#%require "stream.scm")
(#%require "ex-3.70.scm")    ; weighted-pairs

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.71

;; Ramanujan numbers are those that can be expressed as the sum of two cubes in
;; more than one way. Construct them by ordering pairs based on a weighting of
;; i³ + j³, then searching those pairs for two consecutive ones with the same
;; weight.
(define (ramanujan-numbers)
  (define (sum-of-cubes pair)
    (let ((i (car pair))
          (j (cadr pair)))
      (+ (* i i i) (* j j j))))
  (define (go s)
    (let ((i (stream-car s))
          (j (stream-car (stream-cdr s))))
      (if (= (sum-of-cubes i) (sum-of-cubes j))
          (cons-stream (list (sum-of-cubes i) i j)
                       (go (stream-cdr s)))
          (go (stream-cdr s)))))
  (go (weighted-pairs integers integers sum-of-cubes)))

(display-stream-next (ramanujan-numbers) 5)
;; (1729 (1 12) (9 10))
;; (1729 (9 10) (1 12))
;; (4104 (2 16) (9 15))
;; (4104 (9 15) (2 16))
;; (13832 (2 24) (18 20))

