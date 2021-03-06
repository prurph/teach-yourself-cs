#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.54
(define ones
  (cons-stream 1 ones))

;; Omits 0
(define integers
  (cons-stream 1 (add-streams ones integers)))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))
