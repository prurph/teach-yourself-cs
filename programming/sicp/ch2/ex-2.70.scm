#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_thm_2.70

(require "ex-2.68.scm") ; encode
(require "ex-2.69.scm") ; generate-huffman-tree

;; Trivial exercise (given the considerable work we've already done)!
(define ht (generate-huffman-tree
             '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define msg '(GET A JOB
              SHA NA NA NA NA NA NA NA NA
              GET A JOB
              SHA NA NA NA NA NA NA NA NA
              WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
              SHA BOOM))

(encode msg ht)
;; => '(0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 1 0 0 0 1 0 0 0
;;      0 0 1 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
;;      0 0 1 0 0 0 0 0 0 0)

;; How many bits are required for encoding?
(length (encode msg ht))
;; => 87

;; What is the smallest number of bits that would be needed to encode this song
;; if we used a fixed-length code for the eight-symbol alphabet?
;;
;; To encode 8 symbols, we require 3 bits, and there are 36 words `(length msg)`
;; ergo 3 * 36 = 108 bits
