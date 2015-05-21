#lang typed/racket

(require "listex.rkt")
(provide (all-defined-out))


(: lminus (Positive-Integer Integer -> Positive-Integer))
(define (lminus x y)
    (let ([r (- x y)])
        (cond [(> r 1) r]
              [else 1]
        )
    )
)

; convert between a natural number and a binary string

(: nat->binary (Natural -> String))
(define (nat->binary x) (number->string x 2))

(: binary->nat (String -> Natural))
(define (binary->nat str) 
    (: rec ((Listof Char) Natural -> Natural))
    (define (rec ls e)
        (cond 
            [(empty? ls) 0]
            [else (+ (* (expt 2 e) (match (first ls) [#\0 0] [#\1 1])) (rec (rest ls) (+ 1 e))) ]
        )
    )
    (rec (reverse (string->list str)) 0)
)

(: nat->hex (Natural -> String))
(define (nat->hex x) (number->string x 16))

(: fix-binary-length (Positive-Integer String -> String))
(define (fix-binary-length len str)
    (let [(x (string-length str))]
        (cond 
            [(< x len) (string-append (make-string (- len x) #\0) str)]
            [else (substring str (- x len) x)]
        )
    )
)

; convert between a natural number and a word of length 32

(: int->word32 (Integer -> String))
(define (int->word32 x) (int->word 32 x))

(: int->word16 (Integer -> String))
(define (int->word16 x) (int->word 16 x))

(: int->word (Positive-Integer Integer -> String))
(define (int->word size x)
    (let ([fix (lminus size 1)])
        (cond 
            [(< x 0) (string-append "1" (fix-binary-length fix (nat->binary (abs (- (expt 2 fix) (abs x))))))]
            [else (string-append "0" (fix-binary-length fix (nat->binary x)))]
        )
    )
)

; convert between word of 32 and hex notation

(: word32->hex32 (String -> String))
(define (word32->hex32 str) 
    (when (not (eq? (string-length str) 32)) (error "input is not 32 bits" str))
    (foldr string-append "" (map (compose nat->hex binary->nat) (string-group 4 str)))
)

(: int->hex32 (Integer -> String))
(define int->hex32 (compose word32->hex32 int->word32))

(: hex32->int (String -> Integer))
(define (hex32->int str)
    (let* ([n (string-trim str #px"^0x")] [x (string->number n 16)])
        (when (not (eq? (string-length n) 8)) (error "input is not 8 bytes" str))
        (cond 
            [(exact-integer? x) (if (> x 268435456) (bitwise-not (- x 1)) x)]
            ; [(and (nteger? x) (inexact? x)) (inexact->exact x)]
            [else (error "hex->int" str n x)]
        )
    )
)
