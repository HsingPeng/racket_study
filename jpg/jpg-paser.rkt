#lang racket

(define markers (hash
                 "SOF0"  #"\xff\xc0"
                 "SOF1"  #"\xff\xc1"
                  "SOF2" #"\xff\xc2"
                 "SOF3"  #"\xff\xc3"
                 "DHT"   #"\xff\xc4"
                 "SOF5"  #"\xff\xc5"
                 "SOF6"  #"\xff\xc6"
                 "SOF7"  #"\xff\xc7"
                 "JPG"   #"\xff\xc8"
                 "SOF9"  #"\xff\xc9"
                 "SOF10" #"\xff\xca"
                 "SOF11" #"\xff\xcb"
                 "DAC"   #"\xff\xcc"
                 "SOF13" #"\xff\xcd"
                 "SOF14" #"\xff\xce"
                 "SOF15" #"\xff\xcf"
                 "RST0"  #"\xff\xd0"
                 "RST1"  #"\xff\xd1"
                 "RST2"  #"\xff\xd2"
                 "RST3"  #"\xff\xd3"
                 "RST4"  #"\xff\xd4"
                 "RST5"  #"\xff\xd5"
                 "RST6"  #"\xff\xd6"
                 "RST7"  #"\xff\xd7"
                 "SOI"   #"\xff\xd8"
                 "EOI"   #"\xff\xd9"
                 "SOS"   #"\xff\xda"
                 "DQT"   #"\xff\xdb"
                 "DNL"   #"\xff\xdc"
                 "DRI"   #"\xff\xdd"
                 "DHP"   #"\xff\xde"
                 "EXP"   #"\xff\xdf"
                 "APP0"  #"\xff\xe0"
                 "APP1"  #"\xff\xe1"
                 "APP2"  #"\xff\xe2"
                 "APP3"  #"\xff\xe3"
                 "APP4"  #"\xff\xe4"
                 "APP5"  #"\xff\xe5"
                 "APP6"  #"\xff\xe6"
                 "APP7"  #"\xff\xe7"
                 "APP8"  #"\xff\xe8"
                 "APP9"  #"\xff\xe9"
                 "APP10" #"\xff\xea"
                 "APP11" #"\xff\xeb"
                 "APP12" #"\xff\xec"
                 "APP13" #"\xff\xed"
                 "APP14" #"\xff\xee"
                 "APP15" #"\xff\xef"
                 "JPG0"  #"\xff\xf0"
                 "JPG1"  #"\xff\xf1"
                 "JPG2"  #"\xff\xf2"
                 "JPG3"  #"\xff\xf3"
                 "JPG4"  #"\xff\xf4"
                 "JPG5"  #"\xff\xf5"
                 "JPG6"  #"\xff\xf6"
                 "JPG7"  #"\xff\xf7"
                 "JPG8"  #"\xff\xf8"
                 "JPG9"  #"\xff\xf9"
                 "JPG10" #"\xff\xfa"
                 "JPG11" #"\xff\xfb"
                 "JPG12" #"\xff\xfc"
                 "JPG13" #"\xff\xfd"
                 "COM"   #"\xff\xfe"
                 "TEM"   #"\xff\x01"
                 "RES1"  #"\xff\x02"
                 "RES2"  #"\xff\xbf"))

(define-syntax markers-get
  (syntax-rules ()
    ((_ key)
     (hash-ref markers key #f))))

(define-syntax read-2b
  (syntax-rules ()
    ((_ input)
     (read-bytes 2 input))))

(define (input-file file-name)
  (cond
    [(file-exists? file-name)
     (open-input-file file-name)]
    [else
     (display (format "~a is not exist." file-name))
     #f]))

(define (read-SOI input)
  (if (bytes=? (read-2b input) (markers-get "SOI"))
      #t
      (begin
        (displayln "The file is not a jpeg.")
        #f)))

; unfinish
(define (read-frames input)
  which)

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
