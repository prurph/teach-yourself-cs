#lang sicp

(#%require "evaluator.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.10

;; The point here is that we can simply modify the predicates that match a given
;; syntax element in eval, and the selectors that then parse its various
;; components that allow it to be evaluated. A very simple example is using
;; different symbols for and and or. This only requires:
(define (and? exp)
  (tagged-list? exp '&&))
(define (or? exp)
  (tagged-list? exp '||))

;; These are trivial, but we've been doing a lot of exercises and I'm tired!
