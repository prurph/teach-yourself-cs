#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_thm_4.21

;; 4.21(a)
;; Verifying this computes the factorial of 10 using the Y operator style of
;; passing the function to be called recursively as an argument.
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)

((lambda (fact) (fact fact 10))
 (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))

;; Call this lambda with itself as the first argument and k = 10.
;; It recursively calls itself, with itself as the first argument and (- k 1)
;; as the second: (ft ft (- k 1))
((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
 ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
  10))

;; Analogous expression for fibonacci:
(define (fibonacci n)
  ((lambda (fib) (fib fib n))
   (lambda (f k) (cond ((= k 1) 0)
                       ((= k 2) 1)
                       (else (+ (f f (- k 1)) (f f (- k 2))))))))

;; 4.21(b)
;; Y operator style for mutual recursion of even? and odd? internal definitions
;; to compute `even?`. lmao this is awesome
(define (f x)
  ; evaluate (even? x), with refs to the even? and odd? procs so we can mutually recurse
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n) ; the actual even? logic
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n) ; the actual odd? logic
     (if (= n 0) false (ev? ev? od? (- n 1))))))
