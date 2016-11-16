;#lang racket

(define search
  (lambda (l x)
    (cond
      [(null? l) 'not_found]
      [(= x (car l)) 'found]
      [else (search (cdr l) x)])))
;(search '(1 2 3 4 5))
;(provide search)
