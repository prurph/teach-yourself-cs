#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.64

(require "set-as-tree.scm")

;; Given
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree) remaining-elts))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; 2.64(a) Explain how partial-tree works.
;; Divides elts into left-tree, this-entry, and right-tree. The trees are built
;; by recursively dividing the remaining elements in half, with the first half
;; in the left tree, and the second (except for `this-entry`; this is why
;; right-size is n - left-size - 1) in the right. Since the input elements are
;; ordered, the smaller half is guaranteed on the left, the larger on the
;; right, and `this-entry` between them; this in turn means the tree produced
;; will satisfy the binary search property. Furthermore, since the elements are
;; divided in half, the tree will be balanced.

(list->tree (list 1 3 5 7 9 11))
'(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;;    5     
;;   ╱ ╲    
;;  ╱   ╲   
;; 1     9  
;;  ╲   ╱ ╲ 
;;   3 7  11

;; 2.65(b) What is the order of growth in number of steps for n elements?
;; Each element is visited once, and ultimately is `cons`ed onto the result
;; once, so O(n).
