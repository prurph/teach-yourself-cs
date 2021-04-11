#lang scheme
;; [Questions](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/hw.pdf)
;; [Solutions](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week3)
(require (planet "dyoo/simply-scheme"))

;; 1. SICP 1.16, 1.35, 1.37, 1.38

;; 2.
;; Find the next perfect number after n, where a perfect number is a number
;; equal to the sum of all its factors less than itself (eg 6 = 1 + 2 + 3)

;; Find the factors less than n
(define (factors n)
  (define (iter i acc)
    ;; If i is a factor, add both i and n/i. This implies we're done when i
    ;; exceeds sqrt n.
    (cond ((> i (sqrt n)) acc)
          ((= i (sqrt n)) (se acc i))
          ((zero? (modulo n i)) (iter (+ i 1) (se acc i (/ n i))))
          (else (iter (+ i 1) acc))))
  (iter 2 '(1)))

(define (is-perfect? n)
  (= n (reduce + (factors n))))

(define (next-perf n)
  (if (is-perfect? (+ n 1))
      (+ n 1)
      (next-perf (+ n 1))))

(next-perf 29)
;; => 496

;; 3.
;; Original order of conditionals:
(cond ((= amount 0) 1)
      ((or (< amount 0) (= kinds-of-coins 0)) 0)
      ...)

;; Modified order of conditionals:
(cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
      ((= amount 0) 1)
      ...)

;; These will differ in result only when both clauses are true, meaning the
;; order of early return will matter. Thus:
(and ((= amount 0))
     ((or (< amount 0) (= kinds-of-coins 0))))
;; Simplifying--amount must be zero for the first clause, therefore cannot be
;; < 0 in the second:
(and ((= amount 0))
     ((= kinds-of-coins 0)))

;; Examining the recursive call in the else clause to determine how this could
;; occur:
(else (+ (cc amount (- kinds-of-coins 1))
         (cc (- amount (...) kinds-of-coins))))
;; 1. For the first part: kinds-of-coins - 1 = 0 and amount _was_ zero. This is
;;    impossible because we would have short-circuited before on the amount = 0
;;    condition.
;; 2. For the second: amount - (...) = 0 and kinds-of-coins _was_ zero. Again,
;;    this is impossible because i twould short-circuit on the other condition.
;;
;; Thus, the only case in which the order can matter is the direct call:
;;   (cc 0 0)
;; This is tantamount to asking: how many ways are there to make zero cents
;; with no coins? There is one way: no coins at all, and this is why the order
;; of the conditional in the book starts with (= amount 0) 1 to return 1 any
;; time we reach the exact amount.

;; 4.
;; Algebraic formula relating b, n, counter, and product in:
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))
;; counter product
;; n       1
;; n - 1   b
;; n - 2   b^2
;; n - 3   b^3
;; 0       b^n
;;
;; product = b^(n - counter)
;; product * b^counter = b^n

;; Extra 1.
(define (number-of-partitions i)
  ;; Pull out a "chunk" of the number. If the chunk equals the number, there
  ;; is one partition. Otherwise, it's the sum of the number of partitions
  ;; with that chunk removed and the number with a smaller chunk size, down
  ;; to zero.
  (define (rec i chunk)
    (cond ((zero? i) 1)
          ((or (< i 0) (zero? chunk)) 0)
          (else (+ (rec (- i chunk) chunk)
                   (rec i (- chunk 1))))))
  (rec i i))

;; Extra 2.
;; This is identical to the count-change procedure where the denominations of
;; the coins are all positive integers up to the argument integer.

;; Extra 3.
;; Would not have come up with this myself! Continuation passing.
(define (number-of-partitions i)
  (define (rec i chunk next)
    (cond ((zero? i) (next 1))
          ((or (< i 0) (zero? chunk)) (next 0))
          (else (rec (-i chunk) chunk (lambda (x)
                                        (rec i (- chunk 1) lambda (y) (+ x y)))))))
  (rec i i (lambda (x) x)))
