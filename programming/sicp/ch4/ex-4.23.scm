#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.23

;; The difference is that the text's analyze-sequence returns a single lambda
;; at analysis time, whereas Alyssa's version returns a lambda that--when
;; called at execution time--calls her internal `execute-sequence` to execute
;; each analyzed procedure. If the resulting analyzed sequence were executed
;; multiple times, Alyssa's would do the additional work of generating the
;; sequence again.
