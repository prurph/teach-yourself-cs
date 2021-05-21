#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.63

(require "set-as-tree.scm")

;; Two procedures to convert a BST to a list.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Trees from Figure 2.16, representing the set {1, 3, 5, 7, 9, 11}
(define t1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define t2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9 '() (make-tree 11 '() '())))))

(define t3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

;; 2.63(a)
;; Both give the same result.
;;
;; tree->list-1 performs an in-order traversal, recursing down the left side,
;; then appending it to the entry, and the recursive call to the right side.
;; This yields the set in ascending order.
;;
;; tree->list-2 performs a reverse-order traversal, because it must evaluate
;; all the way down the right side. It then prepends each successive entry to
;; this result, therefore also generating the set in ascending order, although
;; constructed in reverse.

;; 2.63(b)
;; Both procedures visit each node exactly once, the difference is that
;; tree->list-2 performs a `cons` for each entry. This is a constant "step"
;; operation--it requires the same amount of calculations regardless of input
;; sizes--so we expect tree->list-2 to grow O(n) with respect to steps.
;;
;; tree->list-1 instead performs an append, which is linear in terms of the
;; list to which it appends. The question specifies _balanced_ BSTs, so the
;; length of this list (which is one branch) will grow at O(log n), for an
;; overall growth of O(n log n) with respect to steps. Note that for unbalanced
;; trees, the complexity will be O(n) best case--totally unbalanced with no
;; left branches, so the first argument to append is always '()--to O(n^2) worst
;; case--totally unbalanced with no right branches. The latter gives:
;;   (n - 1) + (n - 2) + ... + 1
;; for the first call appending the root to the rest, etc., which grows at
;; O(n^2).

