#lang racket

(λ ()
  (define c (λ (x)
              (* x x)))
  c)

(define foo (lambda () (define f (λ (x) (* x x))) f))

((foo) 4)
