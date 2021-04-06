;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.28

;; Return all leaves in left-to-right (simply DFS, returning leaves). A leaf
;; is any value that isn't null or a pair.
(define (fringe t)
  (cond ((null? t) '())
        ((pair? t) (append (fringe (car t)) (fringe (cdr t))))
        (else (list t))))
  
;; Clever iterative solution from http://community.schemewiki.org/?sicp-ex-2.28
;; Traverses right-to-left so when consing leaves onto the accumulator we end
;; up with them listed left-to-right.
(define (fringe t)
  (define (iter t acc)
    (cond ((null? t) acc)
          ((not (pair? t)) (cons t acc))
          (else (iter (car t) (iter (cdr t) acc)))))
  (iter t '()))
