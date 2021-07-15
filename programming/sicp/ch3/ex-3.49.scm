#lang sicp

;; Consider database updates based on a parameter, for example:
;;   UPDATE employees SET salary = 10000000 WHERE ...
;; Two such concurrent updates cannot know which rows will require locking ahead
;; of time, and therefore in the naive case may deadlock in a scenario where
;; each has a lock the other requires and neither can acquire a lock on all rows
;; to be updated.
