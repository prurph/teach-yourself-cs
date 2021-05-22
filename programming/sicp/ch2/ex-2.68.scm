#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.68

(require "huffman.scm")

;; Define encode-symbol to be used in the provided encode below. Note that
;; my solution does traverse the left side first (should be optimal since this
;; is frequency order), however instead of calling `member?`, as I do here,
;; [Drew Hess demonstrates](https://github.com/dhess/sicp-solutions/blob/master/chap2/ex2.68.scm#L79)
;; you can recurse on the encoding all the way left, then successively down
;; right sub-branches. In this scheme you compare the symbol you're encoding
;; when a leaf is reached, returning null on a match, or something else like #f
;; on a non-match. Then if you reach the far right and haven't seen that null,
;; error because the symbol isn't in the tree. This gives a better runtime than
;; my solution because mine calls member for each sub branch, which is an O(n)
;; operation each time, whereas he has only the constant time lookup at the
;; leaves. Clever!
(define (encode-symbol symbol tree)
  (define (encode-1 current-branch)
    (cond ((leaf? current-branch) '())
          ((member? symbol (symbols (left-branch current-branch)))
           (cons 0 (encode-1 (left-branch current-branch))))
          ((member? symbol (symbols (right-branch current-branch)))
           (cons 1 (encode-1 (right-branch current-branch))))
          (else (error "unknown symbol:" symbol))))
  (encode-1 tree))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(provide encode)
