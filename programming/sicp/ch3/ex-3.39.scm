#lang sicp

(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.39

(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))

;; Possible outcomes, with nomenclature
;;   - P1: multiplication procedure (lambda () (set! x (s (lambda () (* x x)))))
;;   - P2: atomic increment procedure (s (lambda () (set! x (+ x 1))))
;;
;; 101: P1 sets x to 100, then P2 sets x to 101
;; 121: P2 sets x to 11, then P1 sets x to 121
;; 100: P1 reads x twice (both reads must be the same), P2 sets x to 11, P1 sets x to 100

