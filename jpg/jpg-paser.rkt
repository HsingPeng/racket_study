#lang racket

(define markers (hash
                 #"\xff\xc0" "SOF0"
                 #"\xff\xc1" "SOF1"
                 #"\xff\xc2" "SOF2"
                 #"\xff\xc3" "SOF3"
                 #"\xff\xc4" "DHT"
                 #"\xff\xc5"  "SOF5"
                 #"\xff\xc6"  "SOF6"
                 #"\xff\xc7" "SOF7"
                 #"\xff\xc8" "JPG"
                 #"\xff\xc9" "SOF9"
                 #"\xff\xca" "SOF10"
                 #"\xff\xcb" "SOF11"
                 #"\xff\xcc" "DAC"
                 #"\xff\xcd" "SOF13"
                 #"\xff\xce" "SOF14"
                 #"\xff\xcf" "SOF15"
                 #"\xff\xd0" "RST0"
                 #"\xff\xd1" "RST1"
                 #"\xff\xd2" "RST2"
                 #"\xff\xd3" "RST3"
                 #"\xff\xd4" "RST4"
                 #"\xff\xd5" "RST5"
                 #"\xff\xd6" "RST6"
                 #"\xff\xd7" "RST7"
                 #"\xff\xd8" "SOI"
                 #"\xff\xd9" "EOI"
                 #"\xff\xda" "SOS"
                 #"\xff\xdb" "DQT"
                 #"\xff\xdc" "DNL"
                 #"\xff\xdd" "DRI"
                 #"\xff\xde" "DHP"
                 #"\xff\xdf" "EXP"
                 #"\xff\xe0" "APP0"
                 #"\xff\xe1" "APP1"
                 #"\xff\xe2" "APP2"
                 #"\xff\xe3" "APP3"
                 #"\xff\xe4" "APP4"
                 #"\xff\xe5" "APP5"
                 #"\xff\xe6" "APP6"
                 #"\xff\xe7" "APP7"
                 #"\xff\xe8" "APP8"
                 #"\xff\xe9" "APP9"
                 #"\xff\xea" "APP10"
                 #"\xff\xeb" "APP11"
                 #"\xff\xec" "APP12"
                 #"\xff\xed" "APP13"
                 #"\xff\xee"  "APP14"
                 #"\xff\xef" "APP15"
                 #"\xff\xf0" "JPG0"
                 #"\xff\xf1" "JPG1"
                 #"\xff\xf2" "JPG2"
                 #"\xff\xf3" "JPG3"
                 #"\xff\xf4" "JPG4"
                 #"\xff\xf5" "JPG5"
                 #"\xff\xf6" "JPG6"
                 #"\xff\xf7" "JPG7"
                 #"\xff\xf8" "JPG8"
                 #"\xff\xf9" "JPG9"
                 #"\xff\xfa" "JPG10"
                 #"\xff\xfb" "JPG11"
                 #"\xff\xfc" "JPG12"
                 #"\xff\xfd" "JPG13"
                 #"\xff\xfe" "COM"
                 #"\xff\x01" "TEM"
                 #"\xff\x02" "RES1"
                 #"\xff\xbf" "RES2"))


(define-syntax markers-get
  (syntax-rules ()
    ((_ key)
     (hash-ref markers key key))))

(define-syntax markers-handler-get
  (syntax-rules ()
    ((_ marker)
     (hash-ref markers-handler marker parse-none))))

(define-syntax read-2b
  (syntax-rules ()
    ((_ input)
     (read-bytes 2 input))))

; unuse
(define-syntax hash-set-cons
  (syntax-rules ()
    ((_ hash cons)
     (hash-set hash (car cons) (cdr cons)))))

; unfinsh
(define (parse-DQT input frame-marker frame-hash-table)
  (void))

; unfinsh
(define (parse-DHT input frame-marker frame-hash-table)
  (void))

(define (parse-none input frame-marker frame-hash-table)
  (λ ([length (integer-bytes->integer (read-2b input) #f #t)])
    (displayln (format "[~a] lengeh: ~a"  frame-marker length))
    (hash-set frame-hash-table frame-marker (read-bytes (- length 2) input))))

; unfinsh
(define markers-handler (hash
                 "DQT"  parse-none
                 "DHT" parse-none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (input-file file-name)
  (cond
    [(file-exists? file-name)
     (open-input-file file-name)]
    [else
     (displayln (format "~a is not exist." file-name))
     #f]))

(define (read-SOI input)
  (if (bytes=? (read-2b input) #"\xff\xd8")
      #t
      (begin
        (displayln "The file is not a jpeg.")
        #f)))

; finished
(define (read-frames input)
  (define (read-one-frame input frames-hash-table)
    (define frame-value (read-2b input))
    (define frame-marker (markers-get frame-value))
    (displayln (format "frames ~a" frame-marker))
    (displayln "read-frames...")
    (cond
      [(eqv? frame-marker #f)
       frames-hash-table]
      [(string=? frame-marker "EOI")
       frames-hash-table]
      [(eof-object? frame-marker)
       frames-hash-table]
      [else
        ((parse-none input frame-marker frames-hash-table))]))
;       ((λ ([table (parse-none
;                     input frame-marker frames-hash-table)])
  ;          (read-one-frame (markers-get (read-2b input)) input table)))]))
  (read-one-frame input (make-hash)))

; unfinish
(define (read-image input frame-set)
      (displayln "OK: finish"))

(define (jpeg-paser file-name)
  ; return input port of the jpeg file
  (define input
    (input-file file-name))
  ; return a hash-set which contains all the frames
  (define read-header
    (and
     input               (displayln (format "OK: input file -> ~a" file-name))
     (read-SOI input)    (displayln "OK: read SOI")
     (read-frames input) (displayln "OK: read frames")))
  ; execute and read the image
  ((λ ([input input] [read-header read-header])
     (if read-header
         (read-image input read-header)
         (void)))))

(jpeg-paser "test1.jpg")
