#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.38

;; Peter: (set! balance (+ balance 10))
;; Paul:  (set! balance (- balance 20))
;; Mary:  (set! balance (- balance (/balance 2)))

;; 3.38(a)
;; Peter → Paul  → Mary:  110 → 90 → 45
;; Peter → Mary  → Paul:  110 → 55 → 35
;; Paul  → Peter → Mary:  80  → 90 → 45
;; Paul  → Mary  → Peter: 80  → 40 → 50
;; Mary  → Peter → Paul:  50  → 60 → 40
;; Mary  → Paul  → Peter: 50  → 30 → 40

;; 3.38(b)
;; If the system allows interleaved processes, concurrent access could produce
;; any of the intermediate values in the first and second columns above:
;; 110, 90, 80, 55, 60, 30, in addition to the sequential possibilities.
