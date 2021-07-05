#lang sicp

(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.40

(define x 10)
(parallel-execute
  (lambda () (set! x (* x x)))    ; P1
  (lambda () (set! x (* x x x)))) ; P2

;; Possible outcomes:
;;
;; 10^2: P1 reads x twice, P2 reads and sets, P1 sets x*x
;; 10^3: P2 reads x three times, P1 reads and sets, P2 sets x^3
;; 10^4: P1 reads x, P2 reads and sets, P1 reads x^3, P1 sets x(x^3)
;;       or P2 reads x twice, P1 sets x^2, P2 reads x^2 and sets x*x(x^2)
;; 10^5: P2 reads x once, P1 sets x to x^2, P2 reads x^2 twice and sets x(x^2)(x^2)
;; 10^6: P1 sets x to x^2, P2 sets x to (x^2)^3
;;       or P2 sets x to x^3, P1 sets x to (x^3)^2

(define y 10)
(define s (make-serializer))
(parallel-execute
  (s (lambda () (set! y (* y y))))    ; P1
  (s (lambda () (set! y (* y y y))))) ; P2

;; Possible outcomes:
;;
;; 10^6: P1 sets y to y^2, P2 sets y to (y^2)^3
;;       or P2 sets y to y^3, P1 sets y to (y^3)^2
