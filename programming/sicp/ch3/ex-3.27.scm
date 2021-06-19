#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.27

(#%require "ex-3.26.scm")

;; This results in exponental runtime because (fib (- n 1)) and (fib (- n 2))
;; result in "branches" of recursive calls that end up computing fib for the
;; same n many times.
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))))))

(define (memoize f)
  (let* ((table (make-table cmp))
         (lookup (table 'lookup-proc))
         (insert! (table 'insert-proc!)))
    (lambda (x)
      (let ((previously-computed-result (lookup x)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result)
              result))))))

;; When memo-fib is defined using memoize, an environment is created in which
;; table is defined, and f is the lambda in memo-fib. Consequently, when
;; (memo-fib 3) is called, the value of (memo-fib 1), which would otherwise be
;; computed twice by calls to f from
;; (memo-fib 3) ==
;;    (+ (memo-fib 2) (memo-fib 1)) ==
;;    (+ (+ (memo-fib 1) (memo-fib 0)) (+ (memo-fib 1) (memo-fib 0)))
;; is memoized for the second call. In general, (memo-fib n) is only computed
;; once for a given n, therefore provided the lookup in the table is constant
;; time--note that the implementation from ex-3.26.scm *is not*, but in general
;; we'd simply use a built-in hash--the runtime of memo-fib n will be O(n).

;; If instead we try to do (define memo-fib (memoize fib)), only the results of
;; memo-fib itself are memoized, because in the memoize fib environment, f is
;; simply fib, so when it calls itself, there is no memoization and it does not
;; store or lookup values in the table.
