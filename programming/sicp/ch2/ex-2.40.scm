#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.40

(require "accumulate.scm")

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval x y)
  (define (iter acc i)
    (if (< i x)
        acc
        (iter (cons i acc) (- i 1))))
  (iter '() y))

;; Given an integer n, generate the sequence of pairs (i, j) with 1 <= i < j <= n
;; Note these are lists, not cons.
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval (+ 1 i) n)))
           (enumerate-interval 1 n)))

(define (prime? n)
  (define (iter i)
    (cond ((equal? 1 i) #t)
          ((zero? (modulo n i)) #f)
          (else (iter (- i 1)))))
  (and (> n 1) (iter (- n 1))))

(define (make-pair-sum p)
  (list (car p)
        (cadr p)
        (+ (car p) (cadr p))))

;; Given an integer n, find all ordered pairs of distinct positive integers
;; i and j where 1 <= j < i <= n and i + j is prime, and return (i, j, i + j)
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter (lambda (p) (prime? (caddr (make-pair-sum p))))
               (unique-pairs n))))
