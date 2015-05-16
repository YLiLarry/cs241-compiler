#lang typed/racket

(require "listex.rkt")
(provide (all-defined-out))

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
            [else str]
        )
    )
)

; convert between a natural number and a word of length 32

(: int->word32 (Integer -> String))
(define (int->word32 x) 
    (cond 
        [(< x 0) (string-append "1" (fix-binary-length 31 (nat->binary (abs (- (expt 2 31) (abs x))))))]
        [else (string-append "0" (fix-binary-length 31 (nat->binary x)))]
    )
)

; convert between word of 32 and hex notation

(: word32->hex32 (String -> String))
(define (word32->hex32 str) (foldr string-append "" (map (compose nat->hex binary->nat) (string-group 4 str))))

(: int->hex32 (Integer -> String))
(define int->hex32 (compose word32->hex32 int->word32))

(int->hex32 0)
(int->hex32 1)
(int->hex32 -1)
(int->hex32 -2)
(int->hex32 -100)
