#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.82

;; simply-scheme just has (random x) which returns an integer [0...x)
;; Also, it seems like (random (expt 2 32)) for 32-bit positive integer returns,
;; but never goes higher than 2^31, so you get a generator that never goes
;; higher than 0.5, ergo use 2^31.
(define (random-zero-to-one)
  (/ (random (expt 2 31)) (- (expt 2 31) 1.0)))

(define (random-range lo hi)
  (let ((range (- hi lo)))
    (+ lo (* (random-zero-to-one) range))))

(define (monte-carlo experiment)
  (define (next passed failed)
    (let* ((result (experiment))
          (passed (+ passed (if result 1 0)))
          (failed (+ failed (if result 0 1))))
      (cons-stream (/ passed (+ passed failed))
                   (next passed failed))))
  (next 0 0))

;; Could also define monte-carlo to take an experiment-stream and passed/failed
;; like in the text. Then construct the experiment stream like:
;; (define (experiment-stream)
;;   (cons-stream (experiment)
;;                (experiment-stream)))
(define (estimate-integral predicate x1 y1 x2 y2)
  (define (experiment)
    (predicate (random-range x1 x2)
               (random-range y1 y2)))
  (stream-map (lambda (x) (* x (- x2 x1) (- y2 y1)))
              (monte-carlo experiment)))

(define (estimate-pi)
  (define (in-unit-circle? x y)
    (<= (+ (* x x) (* y y)) 1.0))
  (estimate-integral in-unit-circle? -1.0 -1.0 1.0 1.0))

(stream-ref (estimate-pi) 10000000)
;; 3.1415688858431112 (example; will vary every time)
