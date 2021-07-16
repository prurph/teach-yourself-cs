#lang sicp

(#%require "stream.scm")
(#%require "ex-3.55.scm") ; partial-sums

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.65

(define (square x) (* x x))

;; Accelerated sequence for approximating a series:
;;   S_n+1 - (S_n+1 - S_n)² / (S_n-1 - 2S_n + S_n+1)
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; S_n-1
        (s1 (stream-ref s 1))  ; S_n
        (s2 (stream-ref s 2))) ; S_n+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; Now approximating ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
(define (ln2-terms n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (ln2-terms (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-terms 1)))

(define ln2-stream-accel
  (euler-transform ln2-stream))

;; SICP calls a stream of streams a "tableau", so this is used to accelerate
;; the approximation by recursively transforming it, then taking the first
;; term in each row.
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define ln2-stream-accel-more
  (accelerated-sequence euler-transform ln2-stream))

(define (display-stream-next s n)
  (if (zero? n)
      'done
      (begin (display-line (stream-car s))
             (display-stream-next (stream-cdr s) (- n 1)))))

;; Gauging how fast they converge to ln 2 ≈ 0.69314718056
(display-stream-next ln2-stream 5)
;; 1.0
;; 0.5
;; 0.8333333333333333
;; 0.5833333333333333
;; 0.7833333333333332


(display-stream-next ln2-stream-accel 5)
;; 0.7
;; 0.6904761904761905
;; 0.6944444444444444
;; 0.6924242424242424
;; 0.6935897435897436

(display-stream-next ln2-stream-accel-more 5)
;; 1.0
;; 0.7
;; 0.6932773109243697
;; 0.6931488693329254
;; 0.6931471960735491
