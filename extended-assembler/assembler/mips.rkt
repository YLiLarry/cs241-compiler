#lang typed/racket

; The code in this file assembles an Inst structure to the MIPS binary

(require "internal.rkt")
(require "parse.rkt")
(provide inst->mips)

(: inst->mips (Inst -> Bytes))
(define (inst->mips inst)
    (: int->mips (Integer -> Bytes))
    (define (int->mips n) (integer->integer-bytes n 4 (< n 2147483648) #t))
    (match inst
        [(Word imm) (int->mips imm)]
        [else (error "ERROR: NO PARSE" inst)]
    )
)


