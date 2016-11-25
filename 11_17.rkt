#lang racket

(define foo
  (λ (lst)
    (call/cc
     (λ (k)
       (cond
         [(null? lst) 'no]
         [(eq? (car lst) #f) (k #t)]
         [else (foo (cdr lst))])))))
(foo '(1 2 3 4))
(foo '(1 2 3 4 #f 5))
