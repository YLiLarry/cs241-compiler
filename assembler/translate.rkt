#lang typed/racket

(require "../word.rkt")
(require "type.rkt")
(provide (all-defined-out))

(: translate-one (Inst -> String))
(define (translate-one inst)
    (word32->hex32 (match inst
        ; [(list 'jr  s te tes)       "test"]
        [(Inst-r 'slt rd rs rt) (replace-r "0000 00ss ssst tttt dddd d000 0010 1010" rd rs rt)]
        [(Inst-i 'beq rs rt im) (replace-i "0001 00ss ssst tttt iiii iiii iiii iiii" rs rt im)]
        [(Inst-j 'jr  rs)       (replace-j "0000 00ss sss0 0000 0000 0000 0000 1000" rs)]
    ))
)

(: remove-space (String -> String))
(define (remove-space str) (string-replace str " " ""))

(: bin-5 (Natural -> String))
(define (bin-5 x) 
    (when (> x 31) (error "bin-5: the given number is greater than 31"))
    (fix-binary-length 5 (nat->binary x))
)

(: bin-16 (Integer -> String))
(define (bin-16 x) (int->word16 x))

(: replace-r (String Reg Reg Reg -> String))
(define (replace-r x rd rs rt) 
    (string-replace
        (string-replace
            (string-replace (remove-space x) "ddddd" (bin-5 rt))
            "sssss" 
            (bin-5 rs))
        "ttttt"
    (bin-5 rt))
)

(: replace-i (String Reg Reg Val -> String))
(define (replace-i x rs rt im) 
    (string-replace
        (string-replace
            (string-replace (remove-space x) "iiiiiiiiiiiiiiii" (bin-16 im))
            "sssss" 
            (bin-5 rs))
        "ttttt"
    (bin-5 rt))
)

(: replace-j (String Reg -> String))
(define (replace-j x rs) (string-replace (remove-space x) "sssss" (bin-5 rs)))

; (translate-one (list 'jr 31))
