#lang typed/racket

(require "type.rkt")
(require "compile.rkt")
(require "../type.rkt")
(require "../parse.rkt")

(: parse-inst-ext ((Listof String) -> Inst-ext))
(define (parse-inst-ext ls)
    (match ls
        [(list reg "=" r1 op r2) (Inst-assign (parse-rg reg) (Inst-assign-result (parse-op-ext op) (parse-rg r1) (parse-rg r2)))]
    )
)

(: parse-op-ext (String -> Op))
(define (parse-op-ext str)
    (match str
        ["+" '+]
        ["-" '-]
        ["*" '*]
        ["/" '/]
    )
)
    
(map inst->string (preprocess-one (parse-inst-ext (list "$1" "=" "$2" "+" "$3"))))
