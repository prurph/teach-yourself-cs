#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.06

;; Random number generator from http://community.schemewiki.org/?sicp-ex-3.6
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

;; Version that takes an initial seed
(define (make-random-seeded seed)
  (define (dispatch m)
    (cond ((eq? m 'generate)
           (set! seed (rand-update seed))
           seed)
          ((eq? m 'reset)
           (lambda (s) (set! seed s)))
          (else (error "Invalid operation - MAKE-RANDOM:" m))))
  dispatch)

;; Version that does not
(define (make-random)
  (make-random-seeded (random (expt 2 31))))

(define r1 (make-random-seeded 5))
(define r2 (make-random))

(r1 'generate)
(r1 'generate)

(r2 'generate)
(r2 'generate)

((r2 'reset) 5)
(r2 'generate)
(r2 'generate)
