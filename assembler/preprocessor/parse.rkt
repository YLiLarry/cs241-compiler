#lang typed/racket

(require "type.rkt")
(require "compile.rkt")
(require "../type.rkt")
(require "../parse.rkt")
(require/typed racket [flatten (All (a) (Listof (Listof a)) -> (Listof a))])

(provide (all-defined-out))

(: parse-inst-ext ((Listof String) -> Stmt))
(define (parse-inst-ext ls)
    (match ls
        [(list reg "=" r1 op r2) (Inst-assign (parse-rg reg) (Inst-assign-result (parse-op-ext op) (parse-rg r1) (parse-rg r2)))]
        [(list reg "=" im) (Inst-assign (parse-rg reg) (Inst-assign-imm (parse-vl im)))]
        [(list "if" if-cond if-true if-false) (Inst-if (parse-rg if-cond) (split-parse if-true) (split-parse if-false))]
        [x (parse-inst x)]
    )
)

(: parse-op-ext (String -> Op))
(define (parse-op-ext str)
    (match str
        ["+" '+]
        ["-" '-]
        ["*" '*]
        ["/" '/]
        ["<" '<]
        [else (error "parse-op-ext")]
    )
)

(: split-lines (String -> (Listof String)))
(define (split-lines str)
    (: rec ((Listof String) (Listof Char) String -> (Listof String)))
    (define (rec done curr str)
        (cond 
            [(equal? str "") (append done (list (list->string curr)))]
            [else (match (get-word str) 
                [(cons w wr) (match w
                    ["while" (let*-values (
                            [(cstr) (list->string curr)] 
                            [(whilel whiler) (until-complete-brackets str)]
                        )
                        (rec 
                            (append done (list cstr whilel))
                            empty
                            whiler
                        )
                    )]
                    
                    ["if" (let*-values (
                            [(cstr) (list->string curr)] 
                            [(ifl ifr) (until-complete-brackets str)]
                        ) 
                        (match (car (get-word ifr))
                            ["else" (let*-values (
                                    [(elsel elser) (until-complete-brackets ifr)]
                                ) 
                                (rec 
                                    (append done (list cstr (string-append ifl elsel)))
                                    empty
                                    elser
                                )
                            )]
                            [else (rec 
                                (append done (list cstr ifl))
                                empty
                                ifr
                            )]
                        )
                    )]
                        
                    [else (rec done (append curr (string->list w)) wr)]
                )]
                [else (error "split-lines")]
            )]
        )
    )
    (rec '() '() str)
)


(: get-word (String -> (Pair String String)))
(define (get-word str)
    (let ([ls (regexp-match #px"\\s*\\S+" str)])
        (cond 
            [(list? ls) (let ([m (first ls)]) (cons (string-trim m) (string-trim (substring str (string-length m)))))]
            [else (error "get-word" str)]
        )
    )
)

(split-lines "asdf; if { while { if ($3) then {$2 = $3 + $1; $1+$3;} else {$4 = $3 + $2;} } asdfasdf; } asdf; ")

; (: filter-empty-string ((Listof String) -> (Listof String)))
; (define (filter-empty-string ls) (filter (lambda (elem) (not (equal? "" elem))) ls))

; (: string-split-at (String String -> (Values String String)))
; (define (string-split-at rx str)
;     (match (regexp-match-positions (regexp rx) str)
;         [#f (values str "")]
;         [(? list? ls)
;             (match (first ls)
;                 [(cons left right) (values (substring str 0 left) (substring str left))]
;                 [else (error "error" rx str)]
;             )
;         ]
;         [else (error "error" rx str)]
;     )
; )


; (: trim-space (String -> String))
; (define (trim-space str) (string-trim str))

(: rewrite-ext (String -> String))
(define (rewrite-ext str) 
    (string-join (flatten (map 
        (lambda ([line : String]) (map inst->string (preprocess-one (parse-inst-ext (split-line line)))))
        (split-lines str)
    )) "\n")
)

(: split-parse (String -> (Listof Stmt)))
(define (split-parse str)
    (map (lambda ([line : String]) (parse-inst-ext (split-line line))) 
        (split-lines str)
    )
)


; (split-line "if ($3) then {$2 = $3 + 1; $3 + $1} else {$4 = $3 + $2}")
; (splitf-at (string->list "$1+$3; if ($3) then {$2 = $3 + $1} else {$4 = $3 + $2}") (lambda (s) (print (equal? s #\2 )) (not (equal? s #\2 ))))
; (split-lines "$1+$3; if ($3) th   en {$2 = $3 + $1;} else {$4 = $3 + $2;}; asdfsdf; if ($3) then {$2 = $3 + $1;} else {$4 = $3 + $2;}; asdfsdf;")

; (displayln (rewrite-ext "$2 = $1 < $3; if ($3) then {$2 = $3 + $1; $2 = $3 + $1;} else {$4 = $3 + $2};"))

; (: split-line (String -> (Listof String)))
    
; (map inst->string (preprocess-one (parse-inst-ext (list "$1" "=" "$2" "+" "$3"))))


