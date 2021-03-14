;; [Questions](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/hw.pdf)
;; [Solutions](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week2)
(require (planet "dyoo/simply-scheme"))

;; 1. SICP 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46 (see SICP solutions)

;; 2.
;; Unconventional naming: they suggest you write map and call it every
(define (every f xs)
  (if (empty? xs)
      '()
      (se (f (first xs)) (every f (bf xs)))))

;; 3.
;; simply-scheme in fact defines map (as every) and filter (as keep)
(every (lambda (number)
         (if (even? number)
             (word number number)
             number))
       '(781 5 76 909 24))
;; '(781 5 7676 909 2424)
