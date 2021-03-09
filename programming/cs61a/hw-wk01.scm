; [Questions](https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/hw.pdf)
; [Solutions](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week1)
(require (planet "dyoo/simply-scheme"))

; 1. SICP exercise 1.6 (see SICP solutions)

; 2. `squares` procedure, using procedures from dyoo/simply-scheme
;   se: create a list from arguments ("sentence")
;   bf: all elements of a list _b_ut the _f_irst
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

