#lang racket

(define LOG-I-FLAG #t)
(define LOG-D-FLAG #t)
(define LOG-DD-FLAG #t)
(define LOG-E-FLAG #t)

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

(define markers (hash
                 #"\xff\xc0" "SOF0"
                 #"\xff\xc4" "DHT"
                 #"\xff\xd8" "SOI"
                 #"\xff\xd9" "EOI"
                 #"\xff\xda" "SOS"
                 #"\xff\xdb" "DQT"
                 #"\xff\xe0" "APP0"
                 #"\xff\xe1" "APP1"))

(define markers-handler (hash
                 "DQT"  parse-DQT
                 "DHT"  parse-DHT
                 "SOF0" parse-SOF0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define read-bit
;  (generator(input)
;            (void)))

(define (parse-one-unit) (void))

(define (parse-one-mcu) (void))

(define (parse-mcu input comp-info-list frame-set mcu-list)
  (define (__parse-mcu input comp-info-list frame-set mcu-list)
    (cond
      [(eqv? comp-info-list '())
       mcu-list]
      [else
       (define comp-info (car comp-info-list))
       (define comp-id (car comp-info))
       (define comp-huffman-id (car (cdr comp-info)))
;       (define get-comp-frame-info
;        (begin
;           (define j-comp (hash-ref frame-set "j-components"))
;           (for ([i j-comp])
;             (define component-id (car i))
;             (define Vmax (cadr i))
;             (define Hmax (caddr i))
;             )))
       (void)]))
  (void))

(define (read-mcus input comp-info-list frame-set)
  (define (__read-mcus input comp-info-list frame-set mcu-list)
    (define new-mcu
      (parse-mcu input comp-info-list frame-set))
    (cond
      [(null? new-mcu)
       mcu-list]
      [else
       (__read-mcus input
                    comp-info-list
                    frame-set
                    (append mcu-list new-mcu))])
    (__read-mcus input comp-info-list frame-set '()))
  (void))

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
      [(and (string? frame-marker) (string=? frame-marker "SOS"))
       (log-d "[SOS] frames end")
       ;;;;;;;
       (log-i (format "height: ~a"
                      (hash-ref frames-hash-table "j-height")))
       (log-i (format "width: ~a"
                      (hash-ref frames-hash-table "j-width")))
       ;;;;;;;
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
  (define length (integer-bytes->integer (read-2b input) #f #t))
  (define comp-number
    (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
  (define (__read-comp-info input number comp-info-list)
    (cond
      [(eqv? number 0)
       comp-info-list]
      [else
       (define comp-id
         (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
       (define comp-huffman-id
         (integer-bytes->integer (bytes 0 (read-byte input)) #f #t))
       (__read-comp-info input
                         (- number 1)
                         (append comp-info-list
                                 (cons comp-id comp-huffman-id)))]))
  ; '((comp-id . comp-huffman-id) ...)
  (define comp-info-list (__read-comp-info input 3 '()))
  (read-bytes 3 input)
  ;(define mcu-list (read-mcus input comp-info-list frame-set))
  )

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
         ;(read-image input read-header)
         (void)
         (void)
         )))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(jpeg-paser "Meizu_P61124-110200.jpg")
;(jpeg-paser "Iphone6_3.jpg")
(jpeg-paser "test3.jpg")
;(jpeg-paser "1")
;(jpeg-paser "OnePlus_IMG_20161121_221417-1.jpg")
