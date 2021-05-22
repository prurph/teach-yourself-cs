#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.72

;; For relative frequencies of the form 1, 2, 4, ..., 2^(n-1):
;;
;;   most-frequent: O(n), since the most frequent symbol is always the first
;;                  branch it will be taken immediately after searching the symbol
;;                  list, which requires O(n)
;;   least-frequent: O(n^2), now we must search the symbol list n - 1 times
;;
;; More generally, the minimum complexity for encoding is when the tree is a
;; balanced one--unlike in Exercise 2.71--and therefore there are O(log n)
;; searches each at O(n) for an overall complexity of O(n log n).
