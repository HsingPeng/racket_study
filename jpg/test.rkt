#lang racket

(define book% (class object% (field [size 10]) (super-new)))
(define hhh (new book%))

(set-field! size hhh (+ 1 (get-field size hhh)))
(get-field size hhh)

(define add (set-field! size hhh (+ 1 (get-field size hhh))))
(define (add1)
  (set-field! size hhh (+ 1 (get-field size hhh)))
  (get-field size hhh))
(define add2 (set-field! size hhh (+ 1 (get-field size hhh))))

(printf "add ~a\n" add)
(printf "add ~a\n" add)
(printf "add ~a\n" add)
(printf "add1 ~a\n" (add1))
(printf "add1 ~a\n" (add1))
(printf "add1 ~a\n" (add1))

(printf (and (add1) #t "333\n"))
(get-field size hhh)
