#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-21.html#%_thm_3.09

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


;; global env
;;   factorial →
;;     parameter: n
;;     body: (if (= n 1) ...
;; (factorial 6)
;;   E1 →
;;     n: 6
;;     (if (= n 1) ...
;;   E2 →
;;     n: 5
;;     (if (= n 1)
;;   ...
;;   E6 →
;;     n: 1
;;     (if (= n 1) ...

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* product count)
                 (+ counter 1)
                 max-count)))

;; global env
;;   factorial →
;;     parameter: n
;;     body: (fact-iter 1 1 n)
;;   fact-iter →
;;     parameter: product counter max-count
;;     body: (if (> counter max-count) ...
;; (factorial 6)
;;   E1 →
;;     n: 6
;;     (fact-iter 1 1 n)
;;   E2 →
;;     product: 1
;;     counter: 1
;;     max-count: 6
;;     (if (> counter max-conunt) ...
;;   E3 →
;;     product: 1
;;     counter: 2
;;     max-count: 6
;;     (if (> counter max-conunt) ...
;;   ...
;;   E8 →
;;     product: 720
;;     counter: 7
;;     max-count: 6
;;     (if (> counter max-conunt) ...
