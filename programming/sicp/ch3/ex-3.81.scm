#lang sicp

(#%require "stream.scm")

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_thm_3.81

;; Random number generator from http://community.schemewiki.org/?sicp-ex-3.6
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define (random-stream seed)
  (define s
    (cons-stream seed
                 (stream-map rand-update s)))
  s)

(define global-seed 0)

(define (rand requests)
  ;; msgs: pairs of either ('generate '()) or ('reset \d+)
  ;; numbers: the stream of random numbers
  (define (dispatch msgs numbers)
    (if (stream-null? msgs)
        numbers
        (let ((msg (stream-car msgs)))
          (cond ((eq? (car msg) 'generate)
                 (cons-stream (stream-car numbers)
                              (dispatch (stream-cdr msgs) (stream-cdr numbers))))
                ((eq? (car msg) 'reset)
                 (dispatch (stream-cdr msgs) (random-stream (cadr msg))))))))
  (dispatch requests (random-stream global-seed)))

(define randoms
  (rand (list->stream (list (list 'generate)
                            (list 'generate)
                            (list 'generate)
                            (list 'reset 12345)
                            (list 'generate)
                            (list 'generate)
                            (list 'generate)
                            (list 'reset 12345)
                            (list 'generate)
                            (list 'generate)
                            (list 'generate)))))

;; 0
;; 26
;; 93
;; 12345
;; 93
;; 124
;; 12345
;; 93
;; 124
