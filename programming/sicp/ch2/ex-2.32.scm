;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.32

;; Subsets are formed by taking the car of a list and adding it to every subset
;; of the cdr. This is the (map (lambda (x (cons (car s) x) rest))) below. To
;; collect all of them we append successive subsets as we recurse back up.
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

;; The trace is useful:
(trace subsets)
(subsets '(1 2 3))

>(subsets '(1 2 3))                          ; subsets of (1 2 3) are 1 cons to each of...
> (subsets '(2 3))                           ; the subsets of (2 3), which are 2 cons to each of...
> >(subsets '(3))                            ; the subsets of (3), which are 3 cons to each of...
> > (subsets '())                            ; the subsets of '(), which are...
< < '(())                                    ; just '()
< <'(() (3))                                 ; consing 3 to get (subsets '(3)), appending them
< '(() (3) (2) (2 3))                        ; consing 2 to get (subsets '(2 3)), appending them
<'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) ; consing 1 to get (subsets '(1 2 3)), appending them

'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; http://wiki.drewhess.com/wiki/SICP_exercise_2.32 also explains it nicely. To
;; paraphrase: start with two sets identical except one contains an extra
;; element:
;;   t : {a1, a2,..., an}
;;   u : {a1, a2,..., an, an+1}
;; The subsets of u then are the subsets of t, plus for each subset of t, an
;; additional set which is that subset plus the element an+1. This is
;; implemented above by breaking the input set into (car s), which is an+1,
;; and then consing it onto each element of (cdr s), which is t.
