;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_thm_1.14

;; Evaluation of (count-change 11)
(cc 11 5)                                        ; 5 for use 5 denominations of coins

(+ (cc 11 4)
   (cc (- 11 (first-denomination 5)) 5))
;; ... the second part keeps decrementing and
;;     adding zeros until we get to a dime

(+ (cc 11 2)
   (cc (- 11 (first-denomination 3)) 3))

(+ (cc 11 2)
   (cc 10 3))

(+ (cc 11 1)
   (+ (cc 10 2)
      (cc 0 3)))

(+ (cc 11 1)
   (+ ((+ (cc 10 1)
          (cc 6 2))
       1)))

;; Ultimately:
;; 1 way to make 11 with 3 coins (cc 11 3):
;;   1 dime 1 penny
;; 2 ways to make 11 with 2 coins (cc 11 2):
;;   1 nickel  6 pennies
;;   2 nickels 1 penny
;; 1 way to make 11 with 1 coins:
;;   11 pennies
;; 4 total ways to make 11 cents

;; Space complexity: O(n)
;;   max depth is for pennies, and this increases with n
;; Time complexity: O(n^k) for k denominations
;;   each additional denomination adds on the order of n steps
