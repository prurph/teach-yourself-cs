#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.22

;; let is just sugar so only the following clause needs to be added to analyze.
;; This is analogous to how `cond`s are analyzed as (analyze (cond->if exp))
(define (analyze exp)
  (cond ;; ...
        ((let? exp) (analyze (let->combination exp)))
        ;; ...
        ))
