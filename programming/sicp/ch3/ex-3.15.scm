#lang sicp

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_thm_3.15

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow) x)

(set-to-wow! z1)
;; => ((wow b) wow b)

;;
;;         ┌───┬───┐                                                             
;; z1 ─────▶ ● │ ● │
;;         └─┬─┴─┬─┘                                                             
;;           │   │                                                               
;;         ┌─▼─┬─▼─┐    ┌───┬───┐                                                
;;  x ─────▶ ● │   ├────▶ ● │ / │                                                
;;         └─┬─┴───┘    └─┬─┴───┘                                                
;;           │            │                                                      
;;         ┌─▼─┐        ┌─▼─┐                                                    
;;         │wow│        │ b │                                                    
;;         └───┘        └───┘                                                    

(set-to-wow! z2)
;; => ((wow b) a b)
                                                                              
;;         ┌───┬───┐    ┌───┬───┐    ┌───┬───┐                                   
;; z2 ─────▶ ● │ ● ├────▶ ● │   ├────▶ ● │ / │
;;         └─┬─┴───┘    └─┬─┴───┘    └─┬─┴───┘                                   
;;           │            │            │                                         
;;           │          ┌─▼─┐        ┌─▼─┐                                       
;;           │          │ a │        │ b │                                       
;;           │          └───┘        └─▲─┘                                       
;;           │                         │                                         
;;           │          ┌───┬───┐    ┌─┴─┬───┐                                   
;;           └──────────▶ ● │   ├────▶ ● │ / │                                   
;;                      └─┬─┴───┘    └───┴───┘                                   
;;                        │                                                      
;;                      ┌─▼─┐                                                    
;;                      │wow│                                                    
;;                      └───┘
