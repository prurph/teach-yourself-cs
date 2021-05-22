#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.71

;; For an alphabet of n symbols with relative frequency 1, 2, 4, ..., 2^(n-1):

;; Since successive more frequent symbol has double the frequency of the
;; previous, when combining least frequent symbols during tree construction,
;; the sum of the two least frequent remaining is always less than the third.
;; This results in each successive combination producing a new sibling of the
;; less frequent symbols, and so every node in the tree has a right leaf.
;;
;; Consequently the most frequent symbol is encoded with 1 bit (from the topmost
;; right child), and the least frequent symbols require n - 1 bits since the
;; tree has depth n - 1.

;; Tree from the [Scheme Community Wiki](http://community.schemewiki.org/?sicp-ex-2.71)
;;                    {a b c d e} 31
;;                     /           \
;;                {a b c d} 15      e 16
;;                 /     \
;;           {a b c} 7    d 8
;;             /    \
;;        {a b} 3    c 4
;;         /   \
;;      a 1    b 2
