#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.31

;; The actions added to a wire are designed to add signals to the agenda when
;; they are called, through `after-delay`. If `add-action!` were rewritten to
;; not call added actions immediately, they would not be added to the agenda,
;; and so calls to `propagate` would simply return `done` because the agenda
;; would be empty. In other words, calling them immediately initializes the
;; agenda. When it is subsequently run actions can then trigger other actions,
;; which in turn add more actions to the agenda, and stuff happens.
