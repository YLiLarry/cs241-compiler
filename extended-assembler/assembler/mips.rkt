#lang typed/racket

; The code in this file assembles an Inst structure to the MIPS binary

(require "internal.rkt")
(require "parse.rkt")
(require "translate.rkt")

(provide inst->mips)

(: inst->mips (Inst -> Bytes))
(define (inst->mips inst)   
    (int->mips (word32->int (inst->bin inst)))
)

(: int->mips (Integer -> Bytes))
(define (int->mips n) 
    (integer->integer-bytes n 4 #t #t)
)

; (: print-bytes ((Listof Bytes) -> Void))
; (define (print-bytes ls)
;     (for-each (lambda (x) (display x)) ls)
; )

; (display (int->mips (hex32->int "0xfffffff1")))

; (: find-code (Symbol (Listof (Pair Symbol Natural)) -> Natural))
; (define (find-code op table)
;     (let ([x (assoc op table)])
;         (if x x (error 'ERROR "NO OP CODE ~a IN ~a" op table))
;     )
; )

; (: inst-std->mips (inst-std -> Bytes))
; (define (inst-std->mips inst)
;     (match inst [(Inst-std op s t d)
;         (let* ([opCode (find-code op std-list)]
;                [rs (Inst-std-rs inst)]
;                [rt (Inst-std-rt inst)]
;                [rd (Inst-std-rd inst)])
;             ()
;         )]
;     )
; )

; (: inst-s->mips (inst-std -> Bytes))
; (define (inst-s->mips inst)
;     (match inst [(Inst-std op s t d)
;         (let* ([opCode (find-code op s-list)]
;                [rs (Inst-std-rs inst)])
;             ()
;         )]
;     )
; )
