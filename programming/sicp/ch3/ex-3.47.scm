#lang sicp

;; Mega-annoyingly we need SICP to be the last import or sicp-concurrency pulls
;; in the Racket cond that doesn't allow expressions in the else clause.
(#%require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))
(#%require (planet neil/sicp))

;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html#%_thm_3.47

;; A semaphore is a generalization of a mutex which supports acquire and release
;; like a mutex, but allows up to a given number of processes to acquire it
;; concurrently.

;; 3.47(a) Implementation of semaphore in terms of mutex
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

;; The mutex above allows any thread to release the mutex, even if it was not
;; the acquiring one (technically the behavior in this case is "undefined" and
;; the expectation is only the acquiring thread should release). In a semaphore,
;; however, any thread can release it by design, and if an unacquired semaphore
;; is released, the releasing thread should block; this allows signalling in
;; cases like a work pool where a worker thread may call 'release and wait
;; to be signalled that work is ready. Most of the solutions I saw online did
;; not account for this behavior, and when calling 'release simply always
;; increased the number of available resources and returned. That is, they did
;; not call (the-semaphore 'release) recursively if the number available was
;; equal to the size of the semaphore.
(define (make-semaphore-mut n)
  (let ((the-mutex (make-mutex))
        (available n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (the-mutex 'acquire)
             (cond ((zero? available)
                    (the-mutex 'release)
                    (the-semaphore 'acquire))
                   (else                      
                    (set! available (- available 1))
                    (the-mutex 'release))))
            ((eq? m 'release)
             (cond ((= available n)
                    (the-mutex 'release)
                    (the-semaphore 'release))
                   (else
                     (set! available (+ available 1))
                     (the-mutex 'release))))))
    the-semaphore))

;; 3.47(b) Implementation of semaphore in terms of test-and-set! Recall that the
;; mutex is acquired when test-and-set! returns false; if it returns true the
;; mutex is held by another thread (or this thread, if we messed up and have now
;; deadlocked ourselves 🧟.)
(define (make-semaphore-ts n)
  (let ((available n)
        (cell (list false)))
    (define (the-semaphore m)
      ;; Little lazy: always try to get the mutex immediately before checking
      ;; the message. If we were validating messages it would be better to do
      ;; that up front, but for this exercise I've omitted that.
      (if (test-and-set! cell)
          (the-semaphore m)
          ;; At this point we have the mutex
          (cond ((eq? m 'acquire)
                 (cond ((zero? available)
                        (clear! cell)
                        (the-semaphore 'acquire))
                       (else
                         (set! available (- available 1))
                         (clear! cell))))
                ((eq? m 'release)
                 (cond ((= available n)
                        (clear! cell)
                        (the-semaphore 'release))
                       (else
                         (set! available (+ available 1))
                         (clear! cell)))))))
    the-semaphore))
