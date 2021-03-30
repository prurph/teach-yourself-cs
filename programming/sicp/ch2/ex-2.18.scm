;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.18

;; Recursive
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

;; Iterative
(define (reverse l)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (cons (car l) acc))))
  (iter l '()))
