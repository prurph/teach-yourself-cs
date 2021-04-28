#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.37

(require "accumulate.scm")

;; Represent a matrix as a list of lists.

;; The dot product is Σ v_i * w_i for vectors v and w (multiply terms
;; at corresponding positions and add them).
;; Note the definition below uses map that takes multiple lists and applies the
;; procedure to all the first elements then all the second, etc, returning a
;; list of the results.
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Returns vector t, where t_i = Σ m_ij * v_j for all j. For each row in m,
;; multiply its terms by the corresponding terms in v, then add them.
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

;; accumulate-n takes a list of lists, and accumulates matching indexes across
;; each list, returning a list of the results; wow this allows an elegant
;; implementation of transpose.
(define (transpose m)
  (accumulate-n cons '() m))

;; Returns matrix p, where p_ij = Σ m_ik * n_kj
;; Transpose the second matrix to get a list of its columns, then for each
;; row in the first matrix, multiply it by each column to get the row in the
;; output matrix.
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;; Examples
(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((1) (2) (3) (4)))
(define v '(1 2 3 4))

(matrix-*-vector m v)
;; => '(30 56 80)

(transpose m)
;; => '((1 2 4 (2 5 6) (3 6 8) (4 6 9)))
(transpose n)
;; => '((1 2 3 4)) 

(matrix-*-matrix m n)
;; => '((30 (56) (80)))
(matrix-*-matrix n (transpose n))
;; => '((1 2 3 4) (2 4 6 8) (3 6 9 12) (4 8 12 16))
