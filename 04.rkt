#lang racket

(define foo
  (λ (s)
  (define (f1) (displayln 'present-flower))
  (define (f2) (displayln 'provide-money))
  (define d (lambda ()
  (cond
    [(equal? s 'beautilful) (f1)]
    [(equal? s 'poor) (f2)])))
  d))

(define (f2) (displayln 'provide-money))

;(f2)

((foo 'poor))

((λ (x y)
   (* x y))
 100
 200)

(let ((x 100)
      (y 200))
      (* x y))

(let ((x 100)
      (y 200))
  (let ((z 300))
    (* x y z)))

(let* ((x 1)
       (y 2)
       (z (* x y)))
  (* x y z))
