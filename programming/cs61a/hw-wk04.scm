;; [Questions](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/hw.pdf)
;; [Solutions](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week4)
(require (planet "dyoo/simply-scheme"))

;; 1. SICP 2.7, 2.8, 2.10, 2.12, 2.17, 2.20, 2.22, 2.23

;; 2.
;; One solution: check if the argument is a pair (`list?` also works) and
;; react accordingly.
(define (substitute l old new)
  (cond ((null? l) '())
        ((pair? (car l)) (cons (substitute (car l) old new)
                               (substitute (cdr l) old new)))
        ((equal? (car l) old) (cons new (substitute (cdr l) old new)))
        (else (cons (car l) (substitute (cdr l) old new)))))

;; Another solution: tree-recursion style whereby for a pair argument, we
;; recurse, otherwise the argument l can be thought of as a "leaf", one of:
;;   1. nil: return it
;;   2. A word matching old: return new to substitute
;;   3. A word not matching old: return the word (no substitution required)
(define (substitute l old new)
  (cond ((null? l) '())
        ((equal? l old) new)
        ((pair? l) (cons (substitute (car l) old new)
                         (substitute (cdr l) old new)))
        (else l)))

;; 3.
(define (substitute2 l olds news)
  (define (substitute-element l olds news)
    (cond ((null? olds) l)
          ((equal? l (car olds)) (car news))
          ((substitute-element l (cdr olds) (cdr news)))))
  (cond ((null? l) '())
        ((pair? l) (cons (substitute2 (car l) olds news)
                         (substitute2 (cdr l) olds news)))
        (else (substitute-element l olds news))))

;; Extra
;; Note that (caddr x) means (car (cdr (cdr x))) so the application is a
;; composition starting from the right, not `andThen` semantics.
(define (cxr-function word)
  (lambda (x)
    (let ((char (first word))
          (rest (bf word)))
      (cond ((equal? char 'c) ((cxr-function rest) x))
            ((equal? char 'a) (car ((cxr-function rest) x)))
            ((equal? char 'd) (cdr ((cxr-function rest) x)))
            ((equal? char 'r) x)
            (else (error (format "Character ~s is not one of c,a,d,r" char)))))))

;; Or a slick version using compose from the solutions and just assumes the
;; input is c[ad]+r, therefore stripping the first and last characters.
(define (cxr-function2 word)
  (define (f x)
    (if (empty? x)
        (lambda (x) x)
        (compose (if (equal? (first x) 'a) car cdr)
                 (f (bf x)))))
  (f (bf (bl word))))

