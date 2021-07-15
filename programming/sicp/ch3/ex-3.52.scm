#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.52

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; Sum is now 1 because only the car of the stream is evaluated

(define y (stream-filter even? seq))
;; Sum is now 6 because stream-filter evaluates the stream until the predicate
;; returns true, so it goes 1 -> 1 + 2 = 3 -> 3 + 3 -> 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;; By the logic above, the stream is evaluated once more: 6 + 4 -> 10, so now
;; sum is 10. There is no re-evaluation of prior terms, so they don't get added
;; in again.

(stream-ref y 7)
;; Now seq is evaluated until we have 7 even terms, so the full evaluation is:
;;   6   (1 + 2 + 3)
;;   10  (6 + 4)
;;   28  (10 + 5 + 6 + 7)
;;   36  (28 + 8)
;;   66  (36 + 9 + 10 + 11)
;;   78  (66 + 12)
;;   120 (78 + 13 + 14 + 15)
;;   136 (120 + 16)
;; Consequently sum is 136, which is also what is returned.

(display-stream z)
;; Display-stream exhaustively evaluates z, displaying terms divisible by 5.
;; This requires full evaluation of seq.
;;   10
;;   15
;;   45
;;   55
;;   105
;;   120
;;   190
;;   210
;; It returns 'done. Now sum is 210, which happens to be the final value in seq.

;; If delay were simply (lambda () ...) then sum would immediately 210 after
;; the definition of seq, and each successive procedure call that evaluated seq
;; would exhausitvely re-evaluate all terms, adding 210 to sum for each of the
;; definitions of y and z. Then the terms up to 136 would be added for the
;; stream-ref call, and again another 210 for display-stream z.
