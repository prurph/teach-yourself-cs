#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.69

(require "huffman.scm")

;; Write successive-merge for an ordered set of leaves for use in the provided
;; `generate-huffman-tree` below.
(define (successive-merge leaf-set)
  (let ((l (car leaf-set))
        (r (cdr leaf-set)))
    (if (null? r)
        l
        (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                      (cadr leaf-set))
                                      (cddr leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(provide generate-huffman-tree)
