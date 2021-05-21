#lang scheme

;; Leaves are the tree are represented by a list of symbol 'leaf, the symbol at
;; the leaf, and the weight.
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

;; General tree will be a list of a left branch, a right branch, a set of
;; symbols (as a ordered, ascending list), and a weight. 
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; Move down the tree, as dictated by each successive bit (left for 0, right
;; for 1), until reaching a leaf. Return the full decode by consing leaf values
;; onto the result of decoding the remaining bits against the full tree, because
;; after each leaf we start anew from the root of the tree to get the next
;; encoded symbol.
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= 0 bit) (left-branch branch))
        ((= 1 bit) (right-branch branch))
        (else (error "bad bit: choose-branch" bit))))

;; Set methods based on ordered, ascending lists, comparing items by weight to
;; determine the order, and whereby symbols are never already in the set.
;; Remeber this set will contain leaves or trees as we encode.
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; Convert a list of symbol-frequency pairs like ((A 4) (B 2) (C 1) (C 1)) to
;; an ordered set of leaves for Huffman encoding.
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (adjoin-set (make-leaf (car (car pairs)) (cadr (car pairs)))
                  (make-leaf-set (cadr pairs)))))

(provide leaf?)
(provide symbol-leaf)
(provide weight-leaf)
(provide make-leaf)
(provide left-branch)
(provide right-branch)
(provide symbols)
(provide decode)
(provide make-leaf)
(provide make-code-tree)
(provide adjoin-set)
(provide make-leaf-set)
