#lang typed/racket

; The code in this file assembles an Inst structure to the MIPS binary

(require "internal.rkt")
(require "parse.rkt")
(require "translate.rkt")

(provide inst->mips mips)

(: inst->mips (Inst -> Bytes))
(define (inst->mips inst)   
    (int->mips (word32->int (inst->bin inst)))
)

(: int->mips (Integer -> Bytes))
(define (int->mips n) 
    (integer->integer-bytes n 4 #t #t)
)

(: mips ((Listof String) -> (Values (Listof Bytes) Label-Table)))
(define (mips ls)
    (let* ([file (tokenize-file ls)]
           [tb (fst-pass file 0)]
           [body (snd-pass file tb 0)]
           [result (map (lambda ([x : Inst]) (inst->mips x)) body)])
        ; (displayln body)
        (values result tb)
    )
)
