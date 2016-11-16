#lang racket

(define is-even?
  (λ (x)
    (if (= x 0)
        #t
        (is-odd? (- x 1)))))
(define is-odd?
  (λ (x)
    (if (= x 0)
        #f
        (is-even? (- x 1)))))

(is-even? 100)
(is-even? 101)
(is-odd? 9)

(letrec ((is-even?
          (λ (x)
            (if (= x 0)
                #t
                (is-odd? (- x 1)))))
         (is-odd?
          (λ (x)
            (if (= x 0)
                #f
                (is-even? (- x 1))))))
  (is-even? 100)
  (is-even? 101))

; 一种递归 空间效率差
(define
  factorial
  (λ (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

(factorial 5)

;尾递归
(define fac (λ (n)
              (let loop ([i n]
                         [k 1])
                (if (= i 0)
                    k
                    (loop (- i 1) (* i k))))))
(fac 4)
