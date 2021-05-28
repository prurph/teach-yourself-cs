#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html#%_thm_2.81

;; 2.81(a)
;; In this case `apply-generic` will recurse infinitely:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  ;; Now both coercion methods exist for type1 == type2, so
                  ;; the first condition is evaluated, and:
                  ;; (apply-generic op (t1->t2 a1) a2)
                  ;; => (apply-generic op a1 a2)
                  ;; => ... infinite recursion
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))

;; 2.81(b)
;; Coercion was not needed for arguments of the same type. If no method exists
;; for op with type1 == type2 then coercion will be attempted, but get-coercion
;; will return false for both t1->t2 and t2->t1, and thus the inner error
;; statement will be reached. TL;DR: `apply-generic` works properly for
;; arguments of the same type if no same-type coercion is defined, however if
;; one is introduced, it breaks as described in 2.81(a).

;; 2.82(c)
(define (apply-generic op . args)
  (define (err)
    (error "No method for these types" (list op type-tags)))
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
              (if (equal? type1 type2)
                  (err)
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                          (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                          (else (err))))))
            (err)))))
