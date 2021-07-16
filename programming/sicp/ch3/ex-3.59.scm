#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.59

;; 3.59(a)
(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

;; Given a0, a1, a2 retrn a0, a1/2, a2/3, ...
(define (integrate-series s)
  (stream-map / s integers))

(integrate-series ones)
;; 1, 1/2, 1/3, 1/4, ...

;; 3.59(b)
;; x ↦ e˟is its own derivative, therefore ͯe˟ and ∫e˟ are the same series except
;; for the constant term e⁰ = 1
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; Derivative of sin is cos, derivative of cos is -sin, therefore using the
;; approach of consing on 1 like above:
(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; The terms match with power series expressions of cos and sin:
;;   cos x = 1 - x^2/2 + x^4/4*3*2 - ... (cosine-series = 1, 0, -1/2, 0, ...)
;;   sin x = x - x^3/3*2 + x^5/5*4*3*2 - ... (sine-series = 0, 1, 0, -1/6, ...)
