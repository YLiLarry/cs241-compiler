#lang typed/racket

(require "../strnum.rkt")
(require "type.rkt")
(require "translate.rkt")
(provide (all-defined-out))

(define std-list (list
    'add
    'sub
    'slt
    'sltu
))

(define st-list (list
    'mult
    'multu
    'div
    'divu
))

(define s-list (list
    'jr
    'jalr
))

(define sti-list (list
    'lw
    'sw
    'beq
    'bne
))

(define d-list (list
    'mfhi
    'mflo
    'lis
))

(: parse-inst ((Listof String) -> Inst))
(define (parse-inst ls) 
    (let [(op (parse-op (first ls))) (x (rest ls))]
        (cond
            [(member op std-list) (match x [(list s t d) (Inst-std op (parse-rg d) (parse-rg s) (parse-rg t))])]
            [(member op st-list) (match x [(list s t) (Inst-st op (parse-rg s) (parse-rg t))])]
            [(member op s-list) (match x [(list s) (Inst-s op (parse-rg s))])]
            [(member op sti-list) (match x [(list s t i) (Inst-sti op (parse-rg s) (parse-rg t) (parse-vl i))])]
            [(member op d-list) (match x [(list d) (Inst-d op (parse-rg d))])]
            [else (error "parse error" ls)]
        )
    )
)

(: parse-op (String -> Op))
(define parse-op string->symbol)

(: parse-rg (String -> Reg))
(define (parse-rg str)
    (match str
        [(pregexp "\\$\\d+" (list x)) (parse-nat (substring x 1))]
    )
)

(: parse-vl (String -> Val))
(define (parse-vl str)
    (match str
        [(pregexp "^\\d+" (list x)) (parse-int x)]
    )
)

(: parse-nat (String -> Natural))
(define (parse-nat s)
    (define x (string->number s))
    (cond
        [(exact-nonnegative-integer? x) x]
        [else (error "no parse")]
    )
)

(: parse-int (String -> Integer))
(define (parse-int s)
    (define x (string->number s))
    (cond
        [(exact-integer? x) x]
        [else (error "no parse")]
    )
)
    


(: split-line (String -> (Listof String)))
(define (split-line str) (string-split (string-replace str "," " ") " "))
