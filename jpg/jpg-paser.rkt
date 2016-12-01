#lang racket

(define LOG-I-FLAG #t)
(define LOG-D-FLAG #t)
(define LOG-DD-FLAG #t)
(define LOG-E-FLAG #t)

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
     (hash-ref markers-handler marker (λ () parse-none)))))

(define-syntax read-2b
  (syntax-rules ()
    ((_ input)
     (read-bytes 2 input))))

; unuse
(define-syntax hash-set-cons
  (syntax-rules ()
    ((_ hash cons)
     (hash-set hash (car cons) (cdr cons)))))

(define-syntax log-i
  (syntax-rules ()
    ((_ content)
     (and LOG-I-FLAG (printf "INFO: ~a\n" content)))))

(define-syntax log-d
  (syntax-rules ()
    ((_ content)
     (and LOG-D-FLAG (printf "DEBUG: ~a\n" content)))))

(define-syntax log-dd
  (syntax-rules ()
    ((_ content)
     (and LOG-D-FLAG
          LOG-DD-FLAG
          (printf "DEBUG: ")
          (println content)))))

(define-syntax log-e
  (syntax-rules ()
    ((_ content)
     (and LOG-E-FLAG (eprintf "ERROR: ~a\n" content)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; j-DQT . '((DQT-id DQT-precision DQT-tabe) ...)
(define (parse-DQT input frame-marker frame-hash-table)
  (define length (integer-bytes->integer (read-2b input) #f #t))
  (log-d (format "[~a] length: ~a"  frame-marker length))
  (define (__read-DQT number input DQT-list)
    (cond
      [(eqv? number 0)
       DQT-list]
      [else
       (__read-DQT (- number 1)
                   input
                   (append DQT-list
                           (list
                            (integer-bytes->integer
                             (bytes 0 (read-byte input)) #f #t))))]))
  (define DQT-info (read-byte input))
  (define DQT-precision (arithmetic-shift DQT-info -4))
  (define DQT-id (bitwise-and DQT-info 15))
  (define result (list (list DQT-id
                            DQT-precision
                            (bytes->list (read-bytes
                                          (- length 3)
                                          input)))))
  (if (hash-has-key? frame-hash-table "j-DQT")
      (hash-update frame-hash-table
                   "j-DQT"
                   (λ (value)
                     (append value
                             (list
                              result))))
      (hash-set frame-hash-table
                "j-DQT"
                result)))

; j-DHT . '((DHT-id DHT-type #hash((2.0) ...)) ...)
(define (parse-DHT input frame-marker frame-hash-table)
  (define length (integer-bytes->integer (read-2b input) #f #t))
  (log-d (format "[~a] length: ~a"  frame-marker length))
  (define info (read-byte input))
  (define DHT-type (arithmetic-shift info -4))
  (define DHT-id (remainder info 16))
  (define table-num-data (bytes->list (read-bytes 16 input)))
  (define code-data (bytes->list (read-bytes (- length 19) input)))
  (define (__table base-value group-num-lst code-lst r-set)
    (cond
      [(eqv? code-lst '())
       r-set]
      [else
       (define (__group __base-value __group-num __code-lst __r-set)
         (cond
           [(or (eqv? __code-lst '()) (eqv? __group-num 0))
            (values __base-value __code-lst __r-set)]
           [else
            (__group (+ __base-value 1)
                     (- __group-num 1)
                     (cdr __code-lst)
                     (hash-set __r-set
                               __base-value
                               (car __code-lst)))]))
       (let-values ([(__base-value __code-lst __r-set)
                     (__group (arithmetic-shift base-value 1)
                              (car group-num-lst)
                              code-lst
                              r-set)])
         (__table (+ __base-value 1)
                  (cdr group-num-lst)
                  __code-lst
                  __r-set))]))
  (define result (list DHT-id
                       DHT-type
                       (__table 0 table-num-data code-data #hash())))
  (if (hash-has-key? frame-hash-table "j-DHT")
      (hash-update frame-hash-table
                   "j-DHT"
                   (λ (value)
                     (append value (list result))))
      (hash-set frame-hash-table
                "j-DHT"
                (list result))))

; j-components . '((component-id Vmax Hmax DQT-id) ...)
; j-height
; j-width
; j-comp-number
(define (parse-SOF0 input frame-marker frame-hash-table)
  (define length (integer-bytes->integer (read-2b input) #f #t))
  (log-d (format "[~a] length: ~a"  frame-marker length))
  (define sample-precision
    (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
  (define height (integer-bytes->integer (read-2b input) #f #t))
  (define width (integer-bytes->integer (read-2b input) #f #t))
  (define components-number
    (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
  (define (__read-component number input component-list)
    (cond
      [(eqv? number 0)
       component-list]
      [else
       (define __component-id
         (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
       (define __component-sample-factors
         (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
       (define __component-DQT-id
         (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
       (define __component-list
         (append component-list
                 (list (list
                        __component-id
                        (quotient __component-sample-factors 16)
                        (remainder __component-sample-factors 16)
                        __component-DQT-id))))
       (__read-component (- number 1) input __component-list)]))
  (cond
    [(eqv? components-number 1)
     (log-e "unsupport grey image")
     #f]
    [(eqv? components-number 3)
     (hash-set* frame-hash-table
                "j-height"      height
                "j-width"       width
                "j-comp-number" 3
                "j-components"  (__read-component 3 input '()))]
    [else
     (log-e "unsupport unknown component type")
     #f]))

(define (parse-none input frame-marker frame-hash-table)
  (define length (integer-bytes->integer (read-2b input) #f #t))
  (log-d (format "[~a] length: ~a"  frame-marker length))
  (hash-set frame-hash-table
            frame-marker
            (read-bytes (- length 2) input)))

(define markers-handler (hash
                 "DQT"  parse-DQT
                 "DHT"  parse-DHT
                 "SOF0" parse-SOF0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (input-file file-name)
  (cond
    [(file-exists? file-name)
     (open-input-file file-name)]
    [else
     (log-i (format "~a is not exist." file-name))
     #f]))

(define (read-SOI input)
  (if (bytes=? (read-2b input) #"\xff\xd8")
      #t
      (begin
        (log-i "The file is not a jpeg.")
        #f)))

(define (read-frames input)
  (define (__read-one-frame input frames-hash-table)
    (define frame-value (read-2b input))
    (define frame-marker (markers-get frame-value))
    (log-d "read-frames...")
    (cond
      [(string=? frame-marker "SOS")
       (log-d "[SOS] frames end")
       (log-dd frames-hash-table)]
      [else
       (define table ((markers-handler-get frame-marker)
                       input
                       frame-marker
                      frames-hash-table))
       (if table
           (__read-one-frame input table)
           (void))]))
  (__read-one-frame input #hash()))

; unfinish
(define (read-image input frame-set)
  (define length (integer-btyes->integer (read-2b input) #f #t))
  (define comp-number
    (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
  (define ))

(define (jpeg-paser file-name)
  ; return input port of the jpeg file
  (define input
    (input-file file-name))
  ; return a hash-set which contains all the frames
  (define read-header
    (and
     input               (log-i (format "OK: input file -> ~a"
                                        file-name))
     (read-SOI input)    (log-i "OK: read SOI")
     (read-frames input) (log-i "OK: read frames")))
  ; execute and read the image
  ((λ ([input input] [read-header read-header])
     (if read-header
         (read-image input read-header)
         (void)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(jpeg-paser "OnePlus_IMG_20161121_221417-1.jpg")
;(jpeg-paser "Meizu_P61124-110200.jpg")
;(jpeg-paser "Iphone6_3.jpg")
(jpeg-paser "test3.jpg")
;(jpeg-paser "1")
