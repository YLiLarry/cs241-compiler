#lang typed/racket

(require "../strnum.rkt")
(require "type.rkt")
(require "preprocessor/type.rkt")
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
(define (split-line str) 
    (match str
        [(regexp "if")
            (match (regexp-match* "([\\{\\(].+?[\\}\\)])" str)
                [(list if-cond if-true if-false)
                    (cond 
                        [(and (string? if-true) (string? if-false)) 
                            (list "if" 
                                (string-trim if-cond #rx"[()]") 
                                (string-trim if-true #rx"[{}]") 
                                (string-trim if-false #rx"[{}]")
                            )
                        ]
                        [else (error "can't parse")]
                    )
                ]
            )
        ]
        [else (string-split str " ")]
    )
)

(: until-complete-brackets (String -> (Values String String)))
(define (until-complete-brackets str)
    
    (: rec (Integer (Listof Char) -> (Listof Char)))
    (define (rec count ls)
        (cond
            [(equal? count 0) empty]
            [(empty? ls) (error "bracket mismatch" str)]
            [(equal? (first ls) #\{ ) (cons #\{ (rec (+ count 1) (rest ls)))]
            [(equal? (first ls) #\} ) (cons #\} (rec (- count 1) (rest ls)))]
            [else (cons (first ls) (rec count (rest ls)))]
        )
    )
    
    (match (regexp-match-positions #px"\\{" str)
        [(? list? pos) (let* (
                [p (first pos)]
                [i (cdr p)]
                [before (substring str 0 i)]
                [block (list->string (rec 1 (string->list (substring str i))))]
                [after (substring str (+ i (string-length block)))]
            )
            (newline)
            (print block)
            (newline)
            (values (string-append before block) after)
        )]
        [#f (values str "")]
    )
    
)


