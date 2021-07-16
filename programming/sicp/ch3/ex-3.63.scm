#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.63
(define (average . xs)
  (/ (apply + xs)
     (length xs)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

;; More efficient, original, version
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

;; Suggested improvment without local `guesses`
(define (sqrt-stream-no-local x)
  (cons-stream 1.0
               (stream-map (lambda (guess) (sqrt-improve guess x))
                           (sqrt-stream-no-local x))))

;; The problem with the second version is that without a local a new stream is
;; constructed with each recursive call, and that stream must be evaluated to
;; the required term to determine the next. These computations are then all
;; performed again (plus one additional) to find the term after that, etc, so
;; the memoization of delay is not leveraged. If delay didn't memoize results,
;; there would be no optimization in using the local.

;; [Barry Allison's explanation](https://wizardbook.wordpress.com/2010/12/20/exercise-3-63/)
;; is a good one:
;;
;; sqrt-stream's first call gives (1.0 (delay (stream-map λ (guesses))))
;; wherein each call to stream-map uses the same stream, and so subsequent calls
;; refer to already memoized results.
;;
;; sqrt-stream-no-local gives (1.0 (delay (stream-map λ (sqrt-stream-no-local x))))
;; wherein everytime a new element is forced from the stream we call sqrt-stream-no-local
;; again, getting a new stream allocated with all of the calculations we
;; previously did *not memoized*.

(#%provide sqrt-stream)
