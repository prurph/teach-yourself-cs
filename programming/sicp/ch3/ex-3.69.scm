#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.69

;; Stream of triples (s_i t_j u_k) such that i <= j <= k. The pairs imported
;; from stream is pairs (s_i t_j) such that i <= j.
(define (triples s t u)
  (cons-stream (map stream-car (list s t u))
               (interleave (stream-map (lambda (x) (cons (stream-car s) x))
                                       (stream-cdr (pairs t u)))
                           (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

;; My initial attempt used (pairs (stream-cdr t) (stream-cdr u)) but this is
;; incorrect because it will not include pairs like (1 1 2) because s_i is never
;; mixed with pairs (t_j u_k) where i = j. Instead we take
;; (stream-cdr (pairs t u)) where the cdr is important so as not to duplicate
;; (1 1 1) (2 2 2) etc.

(define (square x) (* x x))

(define pythagorean-triples
  (stream-filter 
    (lambda (x) (= (+ (square (car x))
                      (square (cadr x)))
                   (square (caddr x))))
    (triples integers integers integers)))

;; Don't go higher than 5 or it takes a looooooong time and hangs the machine.
(display-stream-next pythagorean-triples 5)
;; (3 4 5)
;; (6 8 10)
;; (5 12 13)
;; (9 12 15)
;; (8 15 17)


