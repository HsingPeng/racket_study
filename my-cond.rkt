#lang racket

(define-syntax my-cond
  (syntax-rules ()
    ((_ e1 e2 ...)
     (if e1
         e1
         (begin e2 ...)))))

(my-cond #f (print 'bbb) (print 'ccc))

(define my-cond1
  (λ (e1 e2 ...)
    (if e1
        e1
        (begin e2 ...))))

(my-cond1 #f (print 'bbb) (print 'ccc))

; write file
(define out (open-output-file "openputfile" #:exists 'truncate))
out
(display "hello word" out)
(close-output-port out)

(define out1 (open-output-file "openputfile" #:exists 'update))
out1
(display "211111112" out1)
(file-position out1 5)
(display "kkkkk" out1)
(file-size "openputfile")
(flush-output out1)
(file-size "openputfile")
(display "4321")
(close-output-port out1)

(define out2 (open-input-string "ABCDEFG HIJKLMN"))
(read out2)
out
(file-position out2 2)
(read out2)
(close-input-port out2)

(define out3 (file->string "openputfile" #:mode 'text))
out3
