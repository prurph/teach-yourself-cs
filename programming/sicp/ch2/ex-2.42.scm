#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.42

;; flatmap and enumerate-interval
(require "ex-2.40.scm")

;; Note that the book passes in `k`, the current column to functions like
;; `safe?` and `adjoin-position`, however this isn't necessary since the board
;; is always constructed in a fashion such that the incoming column is always
;; the head of the list, and all previous columns are guaranteed to be
;; consistent (as the book even mentions). Therefore, there's no reason to
;; specify the column index anywhere.

(define empty-board '())

;; Return a list representation with `rest-of-queens`, plus a new element
;; representing the queen's row in column k. Columns are added to the head of
;; the list.
(define (adjoin-position new-row rest-of-queens)
  (cons new-row rest-of-queens))

;; Given a row value for the queen in column `column`, determine if it is safe.
;; We must check there is no conflict in:
;; - Columns: trivial--by definition positions is a list of row values for a
;;   queen in a given column, so there can be no conflict
;; - Rows: check if the value of `column` already exists in `positions`
;; - Diagonal: for each column in `positions`, check if the queen at that
;;   position's row value differs by +- the offset.
(define (safe? positions)
  (define (next-column-safe? new-row positions row-offset)
    (if (null? positions)
        #t
        (let* ((this-row (car positions))
               (forbidden-rows (list this-row (- this-row row-offset) (+ this-row row-offset))))
          (if (member? new-row forbidden-rows)
              #f
              (next-column-safe? new-row (cdr positions) (+ row-offset 1))))))
  (next-column-safe? (car positions) (cdr positions) 1))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))
