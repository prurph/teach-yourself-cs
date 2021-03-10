;; [Questions](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/hw.pdf)
;; [Solutions](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week1)
(require (planet "dyoo/simply-scheme"))

;; 1. SICP exercise 1.6 (see SICP solutions)

;; 2. `squares` procedure, using procedures from dyoo/simply-scheme
;;   se: create a list from arguments ("sentence")
;;   bf: all elements of a list _b_ut the _f_irst
(define (squares xs)
  (define (square x)
    (* x x))
  (if (empty? xs)
      '()
      (se (square (first xs))
          (squares (bf xs)))))

; 3.
(define (switch sent)
  (se (switch-sent-start (first sent))
      (switch-rest (bf sent))))

(define (switch-sent-start w)
  (cond ((equal? w 'you) 'I)
        ((or (equal? w 'I)
             (equal? w 'me)) 'you)
        (else w)))

(define (switch-later-word w)
  (cond ((equal? w 'you) 'me)
        ((or (equal? w 'I)
             (equal? w 'me)) 'you)
        (else w)))

(define (switch-rest sent)
  (if (empty? sent)
      '()
      (se (switch-later-word (first sent))
          (switch-rest (bf sent)))))

;; 4.
;; Recursive helper
(define (ordered? xs)
  (define (ordered-rec? x xs)
    (if (empty? xs)
        #t
        (and (<= x (first xs))
             (ordered-rec? (first xs) (bf xs)))))
  (if (empty? xs)
      #t
      (ordered-rec? (first xs) (bf xs))))

;; Recursive helper without if
(define (ordered? xs)
  (define (ordered-rec? x xs)
    (or (empty? xs)
        (and (<= x (first xs))
             (ordered-rec? (first xs) (bf xs)))))
  (or (empty? xs)
      (ordered-rec? (first xs) (bf xs))))

;; Based on provided solution, but accounting for empty list. I find this the
;; easiest to parse, particularly checking the disordered case and returning
;; false instead of "else (x[0] <= x[1] && (ordered? x[1:]))", which is harder
;; to read when translated to Scheme.
(define (ordered? xs)
  (cond ((empty? xs) #t)
        ((empty? (bf xs)) #t)
        ((> (first xs) (first (bf xs))) #f)
        (else (ordered? (bf xs)))))

;; Or, if we assume xs is non-empty
(define (ordered? xs)
  (or (empty? (bf xs))
      (and (<= (first xs) (first (bf xs)))
           (ordered? (bf xs)))))

;; 5.
(define (word-ends-e? w)
  (equal? (last w) 'e))

(define (ends-e sent)
  (cond ((empty? sent) '())
        ((word-ends-e? (first sent)) (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent)))))

;; As a generic list filter function with a predicate
(define (filter l p)
  (cond ((empty? l) '())
        ((p (first l)) (se (first l) (filter (bf l) p)))
        (else (filter (bf l) p))))

(define (ends-e sent)
  (filter sent word-ends-e?))

;; 6.
;; If `and` and `or` are special forms, they will not evaluate all argument
;; expressions (they can short-circuit). If they are ordinary functions, they
;; will, since Scheme uses applicative-form evaluation.
;;
;; To test this, create an invalid argument expression in a position that would
;; not be evaluated in the special form case. If the invocation does not cause
;; an error, then it is a special form. Note that if it _does_ cause an error,
;; you cannot conclude it is an ordinary procedure, because it could be special
;; and evaluating its arguments in a different order. (I didn't think of this
;; but the course solutions pointed it out.)
(define (test-and-special-form)
  (and #f (an-undefined-identifier)))

(define (test-or-special-form)
  (or #t (an-undefined-identifier)))

;; Special forms of `and` and `or` can be advantageous because they may reduce
;; computational time and effort by short-circuiting:
;;   (or (something-false) (expensive-comp)) ; expensive-comp never evaluated

;; Ordinary procedures could be advantageous in a runtime where the arguments
;; could be evaluated concurrently or in parallel, since the interpreter could
;; then proceed as soon as one returns in some cases:
;;   (or (return-false-in-60s) (return-true-in-60s))
;; in the above example special form evaluation would require 2 minutes to
;; evaluate the procedure, whereas an ordinary function could theoretically
;; take only one given concurrent evaluation.

;; Other reasons for preferring an ordinary function noted in the solutions
;; include that functions (procedures) can be manipulated as data, but special
;; forms cannot in be manipulated in the same ways.
