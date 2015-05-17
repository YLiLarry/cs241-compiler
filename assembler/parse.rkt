#lang typed/racket

(require "../strnum.rkt")
(require "type.rkt")
(require "translate.rkt")


(: parse-inst ((Listof String) -> Inst))
(define (parse-inst ls) 
    (match ls
        ; normal
        [(list op r1 r2 r3) (Inst-r (parse-op op) (parse-rg r1) (parse-rg r2) (parse-rg r3))]
        [(list op r1 r2) (Inst-m (parse-op op) (parse-rg r1) (parse-rg r2))]
        [(list op r1) (Inst-j (parse-op op) (parse-rg r1))]
    )
)

(: parse-op (String -> Op))
(define (parse-op str)
    (match str
        ["slt" 'slt]
        ["jr" 'jr]
    )
)

(: parse-rg (String -> Reg))
(define (parse-rg str)
    (match str
        [(pregexp "\\$\\d+$" (list x)) (parse-nat (substring x 1))]
    )
)

; (: parse-vl (String -> Value))
; (define (parse-vl str)
;     (match str
;         [(pregexp "\\d+" (list x)) (parse-int (substring x 1))]
;     )
; )

(: parse-nat (String -> Reg))
(define parse-nat string->nat)

(translate-one (parse-inst '("jr" "$31")))
