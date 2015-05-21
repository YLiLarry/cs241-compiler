#lang typed/racket

;(require "../strnum.rkt")
(require "../word.rkt")
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
    (let ([op (parse-op (first ls))] [x (rest ls)])
        (cond
            [(member op std-list) (match x [(list s t d) (Inst-std op (parse-rg d) (parse-rg s) (parse-rg t))])]
            [(member op st-list) (match x [(list s t) (Inst-st op (parse-rg s) (parse-rg t))])]
            [(member op s-list) (match x [(list s) (Inst-s op (parse-rg s))])]
            [(member op sti-list) (match x [(list s t i) (Inst-sti op (parse-rg s) (parse-rg t) (parse-vl i))])]
            [(member op d-list) (match x [(list d) (Inst-d op (parse-rg d))])]
            [(equal? op '.word) (Word (hex32->int (first x)))]
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
        [x (error "parse-rg" x)]
    )
)

(: parse-vl (String -> Val))
(define (parse-vl str)
    (match str
        [(pregexp "^\\d+" (list x)) (parse-int x)]
        [x (error "parse-vl" x)]
    )
)

(: parse-nat (String -> Natural))
(define (parse-nat s)
    (define x (string->number s))
    (cond
        [(exact-nonnegative-integer? x) x]
        [else (error "parse-nat" x)]
    )
)

(: parse-int (String -> Integer))
(define (parse-int s)
    (define x (string->number s))
    (cond
        [(exact-integer? x) x]
        [else (error "parse-int" x)]
    )
)
    

    
(: find-complete-brackets (String Boolean -> (Values String String)))
(define (find-complete-brackets str include-before?)
    
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
            (values (string-append (if include-before? before "") block) after)
        )]
        [#f (values str "")]
    )
    
)

(: until-complete-brackets (String -> (Values String String)))
(define (until-complete-brackets str) (find-complete-brackets str #t));

(: complete-brackets (String -> (Values String String)))
(define (complete-brackets str) (find-complete-brackets str #f));


(: strict-regex-match (Regexp String -> String))
(define (strict-regex-match px str)
    (let ([x (regexp-match px str)])
        (cond 
            [(list? x) (first x)]
            [else (error "strict-regex-match" px str)]
        )
    )
)    


(: split-line (String -> (Listof String)))
(define (split-line str) 
    (: first-round-bracket (String -> String))
    (define (first-round-bracket str)
        (match (regexp-match "\\(.+?\\)" str)
            [#f (error "first-round-bracket")]
            [(? list? ls) (string-trim (first ls) #px"[()]")]
        )
    )
    
    (match (string-trim str)
        [(regexp #px"(^if)") (let*-values (
                [(after)  (strict-regex-match #px"\\{.*" str)]
                [(b1 b1r) (until-complete-brackets after)]
                [(b2 b2r) (until-complete-brackets b1r)] 
            )
            (list "if" 
                (first-round-bracket str) 
                (string-trim b1 #px"[{}]") 
                (string-trim (string-trim b2 #px"\\s*else.*\\{") #px"\\}")
            )
        )]
        [(regexp #px"(^while)") (let*-values (
                [(after)  (strict-regex-match #px"\\{.*" str)]
                [(b1 b1r) (until-complete-brackets after)]
            )
            (list "while" 
                (first-round-bracket str) 
                (string-trim b1 #px"[{}]") 
            )
        )]
        [else (string-split str " ")]
    )
)


