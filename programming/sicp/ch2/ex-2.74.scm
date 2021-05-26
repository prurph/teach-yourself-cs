#lang scheme

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-17.html#%_thm_2.74

;; 2.74(a)
(define get '())
(define put '())

;; Construct a standardized file from the division name and its original file.
(define (make-file division original-file)
  (cons division original-file))
(define (file-division file)
  (car file))
(define (file-content file)
  (cdr file))

;; The main get-record method will look up separate get-record methods based on
;; the division, and apply it to the employee name and the file content. It will
;; return a record of the division and the record, or (division #f) if the
;; record doesn't exist.
(define (get-record employee file)
  (make-record (file-division file)
               ((get 'get-record (file-division file)) employee
                (file-content file))))

(define (make-record division original-record)
  (cons division original-record))
(define (record-division record)
  (car record))
(define (record-content record)
  (cdr record))

;; 2.74(b)
;; Each division's module should provide a get-salary method that takes a given
;; record and returns its salary.
(define (get-salary record)
  ((get 'get-salary (record-division record)) (record-content record)))

;; Here is a sample division package:
(define (install-engineering-division-package)
  (define (get-record employee file-content) '())
  (define (get-salary employee-record) '())
  (put 'get-record '(engineering) get-record)
  (put 'get-salary '(engineering) get-salary))

;; 2.74(c)
(define (find-employee-record employee files)
  (if (null? files)
      (error "employee not found:" employee)
      (let ((record (get-record employee (car files))))
        (if (record)
            record
            (find-employee-record employee (cdr files))))))

;; 2.74(d)
;; A new package must be defined and installed, with a globally unique division
;; identifier:
(define (install-newco-package)
  (define (get-record employee file-content) '())
  (define (get-salary employee-record) '())
  (put 'get-record '(newco) get-record)
  (put 'get-salary '(newco) get-salary))

;; and the standardized file must be constructed
(make-file '(newco) original-newco-file)
