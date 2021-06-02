#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html#%_thm_3.02

(define (make-monitored f)
  (define how-many-calls? 0)
  (define (reset-count)
    (set! how-many-calls? 0))
  (define (dispatch a)
    (cond ((eq? a 'how-many-calls?) how-many-calls?)
          ((eq? a 'reset-count) (reset-count))
          (else (set! how-many-calls? (+ how-many-calls? 1))
                (f a))))
  dispatch)

(define s (make-monitored sqrt))
(s 100)
;; => 10
(s 'how-many-calls?)
;; => 1
(s 'reset-count)
(s 'how-many-calls?)
;; => 0
