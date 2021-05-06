#lang scheme
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#%_thm_2.43

;; NB: http://wiki.drewhess.com/wiki/SICP_exercise_2.43 has a really good
;; discussion of this, and is the source of the observations below.

;; The flatmap in the original version:
(flatmap (lambda (rest-of-queens)
           (map (lambda (new-row)
                  (adjoin-position new-row rest-of-queens))
                (enumerate-interval 1 board-size)))
         ((queen-cols (- k 1))))

;; This computes (queen-cols (-k 1)) once, then for each element therein,
;; maps over the interval of 1..board-size to find viable rows for a queen
;; in the "next" column. This is effectively linear recursion; each call to
;; queen-cols calls queen-cols once.

;; The flatmap in the slow version:
(flatmap (lambda (new-row)
           (map (lambda (rest-of-queens)
                  (adjoin-position new-row rest-of-queens))
                (queen-cols (- k 1))))
         (enumerate-interval 1 board-size))

;; This instead evaluates the interval 1..board-size once and then for each
;; of these solves (queen-cols (- k 1)). This is a tree-recursive process because
;; each call to queen-cols will generate board-size more calls to queen-cols.
;; Since there are board-size total calls, we can envision this as a tree of
;; depth board-size and degree (# of child nodes per parent) of board-size.

;; About how much slower is the slow version if the fast version takes time T?

;; The number of nodes in a tree of degree n and depth m is:
;;   1 + n + n^2 + n^3 + .. + n^(m - 1)
;; (There are m levels, we start with one node, so n^0, and each successive
;; level multiplies the number of parent nodes above by n.) Thus the slower
;; version makes on the order of board-size^board-size calls to queen-cols,
;; whereas the faster one makes on the order of board-size calls.
;;
;; If the faster version has runtime T, we expect the slower one to be on the
;; order of T^T. As shrewdly noted in the above link, this "doesn't tell the 
;; whole story" since the number of elements in (queen-cols x) for different
;; values of x is different and complicated, the cost of filtering them isn't
;; easily expressed--at least by me--in terms of T.
