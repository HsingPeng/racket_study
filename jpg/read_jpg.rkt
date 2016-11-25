#lang racket

(define file_name "test1.jpg")
;(define file_name "1")

(define read_file (file->bytes file_name #:mode 'binary))

read_file

(define (check_SOI data)
  (regexp-match #rx#"^\377\330.*" data))

(define (check_APP0 data)
  (regexp-match #rx#"^\377\340.*" data))

(define (check_APP1 data)
  (regexp-match #rx#"^\377\341.*" data))

 (define read_APP0
   (λ (data)
     data))

; beginning
((λ ([data (check_SOI read_file)])
  (cond
    [(eqv? data #f)
     (display (format "~a is not a jpeg file." file_name))]
    [else
     (if (check_APP0 (subbytes (car data) 2))
         (read_APP0 data))])))

;(define (read_SOI data)
;  (cond
;    [(eqv? data #f)
;     (display (format "~a is not a jpeg file." file_name))]
;    [else
;     (check_APP0 (subbytes (car data) 2))]))
;
;(define (read_APP0 data)
;  data)
;
;(define (read-other data)
;  data)
;
; beginning
;(define (read-file file-name)
;  (define (check-section data value)
;    (eqv? (subbytes data 0 2) value))
;  (define (read-func data)
;    (cond
;      [(check-section data #"\377\330")
;       (read-func (read_SOI))]
;      [(check-section data #"\377\340")
;       (read-func (read_APP0))]
;      [(eqv? data "")
;       (display "OK.")]
;      [else
;       (read-func (read-other data))]))
;  (read-func data))
;
;(read-file "test1.jpg")
