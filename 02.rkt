#lang racket

(define f2 (λ (x . y) (displayln x)))
; 1
(f2 1 2 3 4 5)
; (1 2 3)
(f2 '(1 2 3))
; 1
(f2 1 2 3)

(define f3 (λ (x . y) (displayln (car y))))
; 2
(f3 1 2 3)

(define f4 (λ (x . y) (displayln (cdr y))))
; (3)
(f4 1 2 3)

(define f5 (λ (x . y) (displayln (cadr y))))
; 3
(f5 1 2 3 4 5)

(define f6 (λ (x . y) (displayln (cddr y))))
; (4 5)
(f6 1 2 3 4 5)

(define f7 (λ (x . y) (displayln (car x))))
; 1
(f7 '(1 2) 2 3)
