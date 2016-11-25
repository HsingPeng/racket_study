#lang racket

; 9
(define reverse
  (λ (lst)
    (define __reverse
      (λ (lst rlst)
        (cond
          [(eqv? '() lst)
           rlst]
          [else
           (__reverse (cdr lst) (cons (car lst) rlst))])))
    (__reverse lst '())))
;(reverse '(a 3 x))

; 8
(define append1
  (λ (lst11 lst22)
    (define __append
      (λ (lst1 lst2)
        (cond
          [(eqv? '() lst1)
           ;(print 'bbb)
           lst2]
          [else
           ;(print 'aaa)
           (__append (cdr lst1) (cons (car lst1) lst2))])))
    (__append (reverse lst11) lst22)))

;(append '(a b c) '(1 2 3))

; 2
(define insertR
  (λ (a b ilst)
    (define _f
      (λ (x y lst rlst)
        (cond
                  [(eqv? '() lst)
                    ;(print 'aaa)
                    rlst]
                  [(eqv? x (car lst))
                    ;(print 'bbb)
                   (_f x y (cdr lst) (append rlst (list 'x) (list y)))]
                  [else
                   ;(print 'ccc)
                   (_f x y (cdr lst) (append rlst (list (car lst))))])))
    (_f a b ilst '())))

;(insertR 'x 'y '(x z z x y x))

; 1
(define (countdown x)
  (define f (λ (lst i)
              (cond
                [(eq? 0 i)
                 (append lst '(0))
                 ]
                [(< 0 i)
                 (f (append lst (list i)) (- i 1))]
                [else
                 (displayln "Input error.")])))
    (f '() x))

;(countdown 5)
;(countdown 0)
;(countdown -1)




; 3
(define (remv-lst a lst)
  (define f
    (λ (x lst rlst)
                (cond
                  [(eqv? x (car lst))
                   (append rlst (cdr lst))]
                  [(eqv? '() lst)
                   rlst]
                  [else
                   (f x (cdr lst) (append rlst (list (car lst))))])))
    (f a lst '()))
;(remv-lst 'x '(x y z x))
;(remv-lst 'y '(x y z y x))

; 4
(define (list-index-ofv? a lst)
  (letrec ([f (λ (x lst i)
                (cond
                  [(eqv? '() lst)
                   'no]
                  [(eqv? x (car lst))
                   i]
                  [else
                   (f x (cdr lst) (+ 1 i))]))])
    (f a lst 0)))

;(list-index-ofv? 'x '(x y z x x))
;(list-index-ofv? 'x '(y z x x))

; 5
(define (filter foo lst)
  (define f (λ (foo lst rlst)
                (cond
                  [(eqv? '() lst)
                   rlst]
                  [(foo (car lst))
                   (f foo (cdr lst) (append rlst (list (car lst))))]
                  [else
                   (f foo (cdr lst) rlst)])))
    (f foo lst '()))
;(filter even? '(1 2 3 4 5 6))

; 6
(define (zip lst11 lst22)
  (define f (λ (lst1 lst2 rlst)
                (cond
                  [(or (eqv? '() lst1) (eqv? '() lst2))
                   ;(print 'bbb)
                   rlst]
                  [else
                   ;(print 'aaa)
                   (f (cdr lst1) (cdr lst2) (append rlst (list (cons (car lst1) (car lst2)))))])))
    (f lst11 lst22 '()))

;(zip '(1 2 3) '(a b c))
;(zip '(1 2 3 4 5 6) '(a b c))
;(zip '(1 2 3) '(a b c d e f))

; 7
(define (map foo lst)
  (define f (λ (foo lst rlst)
                (cond
                  [(eqv? '() lst)
                   rlst]
                  [else
                   (f foo (cdr lst) (append rlst (list (foo (car lst)))))])))
          (f foo lst '()))

;(map add1 '(1 2 3 4))

; 10
(define fact
  (λ (n)
    (define __fact
      (λ (i ret)
        (cond
          [(eqv? 0 i)
           ret]
          [else
           (__fact (- i 1) (* ret i))])))
    (__fact n 1)))
;(fact 0)
;(fact 5)

; 11
(define memv
  (λ (x lst)
    (define __memv
      (λ (x lst)
        (cond
          [(eqv? '() lst)
           #f]
          [(eqv? x (car lst))
           lst]
          [else
           (__memv x (cdr lst))])))
    (__memv x lst)))

;(memv 'a '(a b c))
;(memv 'b '(a ? c))
;(memv 'b '(a b c b))

; 12
(define fib
  (λ (x)
    (define __fib
      (λ (a b i)
        (cond
          [(eqv? 0 i)
           b]
          [else
           (__fib b (+ a b) (- i 1))])))
    (cond
      [(eqv? x 0)
       0]
      [(eqv? x 1)
       1]
      [else
       (__fib 0 1 (- x 1))])))
;(fib 0)
;(fib 1)
;(fib 7)

; 13
; rewrite ((w x) y (z))
;'((w . (x . ())) . (y . ((z . ()) . ())))

; 14
(define binary->natural
  (λ (lst)
    (define __bin
      (λ (lst p ret)
        (cond
          [(eqv? '() lst)
           ret]
          [else
           (__bin (cdr lst) (* 2 p) (+ ret (* (car lst) p)))])))
    (__bin lst 1 0)))

;(binary->natural '())
;(binary->natural '(0 0 1))
;(binary->natural '(0 0 1 1))
;(binary->natural '(1 1 1 1))
;(binary->natural '(1 0 1 0 1))
;(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))

; 15
(define minus
  (λ (a b)
    (define __minus
      (λ (x y i)
        (cond
          [(eqv? x y)
           i]
          [else
           (__minus x (+ y 1) (+ 1 i))])))
    (__minus a b 0)))

;(minus 5 3)
;(minus 100 50)

; 16
(define div
  (λ (a b)
    (define __div
      (λ (x y ret_y i)
        (define ___div
          (lambda (ret_yy yy n)
            (cond
              [(eqv? n yy)
               ret_yy]
              [else
               ;(println (format "aaa n=~a yy=~a ret_yy=~a" n y ret_y))
               (___div (+ ret_yy 1) yy (+ n 1))])))
        (cond
          [(eqv? x ret_y)
           i]
          [(< x ret_y)
           #f]
          [else
           ;(println (format "bbb x=~a y=~a ret_y=~a" x y ret_y))
           (__div x y (___div ret_y y 0) (+ i 1))])))
    (__div a b b 1)))

;(div 25 5)
;(div 100 50)

; 17
(define append-map
  (λ (foo lst)
    (define __append-map
      (λ (foo lst rlst)
        (cond
          [(eqv? '() lst)
           rlst]
          [else
           (__append-map foo (cdr lst) (append rlst (foo (car lst))))])))
    (__append-map foo lst '())))

;(append-map countdown (countdown 5))

; 18
(define set-difference
  (lambda (lst1 lst2)
    (define __set-difference
      (λ (lst set1 rlst)
        (cond
          [(eqv? '() lst)
           rlst]
          [(not (set-member? set1 (car lst)))
           (__set-difference (cdr lst) set1 (append rlst (list (car lst))))]
          [else
           (__set-difference (cdr lst) set1 rlst)])))
    (__set-difference lst1 (list->set lst2) '())))

;(set-difference '(1 2 3 4 5) '(2 4 6 8))

; 19
(define powerset
  (λ (lst)
    (define __powerset
      (λ (lst a)
        (cond
          [(eqv? '() lst)
           (list a)]
          [else
           (append (__powerset (cdr lst) (cons (car lst) a)) (__powerset (cdr lst) a))])))
    (__powerset lst '())))

;(powerset '(3 2 1))
;(powerset '())

(define (powerset1 lst)
  (if (null? lst)
      '(())
      (append-map (lambda (x)
                    (list x (cons (car lst) x)))
                  (powerset1 (cdr lst)))))

; 20
(define cartesian-product
  (λ (lst)
    (define __cartesian-product
      (λ (lst1 lst2 olst2 rlst)
        (cond
          [(eqv? '() lst1)
;           (println 'aaaa)
           rlst]
          [(eqv? '() lst2)
;           (println 'bbbb)
           (__cartesian-product (cdr lst1) olst2 olst2 rlst)]
          [else
;           (println 'cccc)
           (__cartesian-product lst1 (cdr lst2) olst2 (cons (list (car lst1) (car lst2)) rlst))])))
    (__cartesian-product (car lst) (car (cdr lst)) (car (cdr lst)) '())))

;(cartesian-product '((5 4) (3 2 1)))

; 21

; 22
(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
    ;... ;; this should be a single line, without lambda
))
