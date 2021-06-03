#lang scheme

;; Run a Monte Carlo simulation for:
;; - `trials`: number of trials
;; - `experiment`: a no-arg procedure that returns a boolean each time it is run
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((zero? trials-remaining) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(provide monte-carlo)
