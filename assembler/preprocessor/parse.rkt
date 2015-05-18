#lang typed/racket

(require "type.rkt")
(require "compile.rkt")
(require "../type.rkt")
(require "../parse.rkt")
(require/typed racket [flatten (All (a) (Listof (Listof a)) -> (Listof a))])

(: parse-inst-ext ((Listof String) -> Inst-ext))
(define (parse-inst-ext ls)
    (match ls
        [(list reg "=" r1 op r2) (Inst-assign (parse-rg reg) (Inst-assign-result (parse-op-ext op) (parse-rg r1) (parse-rg r2)))]
        [(list "if" if-cond if-true if-false) (Inst-if (parse-rg if-cond) (split-parse if-true) (split-parse if-false))]
        [else (raise ls)]
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

(: split-lines (String -> (Listof String)))
(define (split-lines str) (filter (lambda (s) (not (equal? s ""))) (map trim-space (string-split str ";"))))

(: trim-space (String -> String))
(define (trim-space str) (string-trim str))

(: rewrite-ext (String -> String))
(define (rewrite-ext str) 
    (string-join (flatten (map 
        (lambda ([line : String]) (map inst->string (preprocess-one (parse-inst-ext (split-line line)))))
        (split-lines str)
    )) "\n")
)

(: split-parse (String -> (Listof Inst-ext)))
(define (split-parse str)
    (map (lambda ([line : String]) (parse-inst-ext (split-line line))) (split-lines str))
)


(split-line "if ($3) then {$2 = $3 + 1; $3 + $1} else {$4 = $3 + $2}")
(split-lines "$1+$3; if ($3) then {$2 = $3 + $1} else {$4 = $3 + $2}")

(display (rewrite-ext "if ($3) then {$2 = $3 + $1} else {$4 = $3 + $2}"))

; (: split-line (String -> (Listof String)))
    
; (map inst->string (preprocess-one (parse-inst-ext (list "$1" "=" "$2" "+" "$3"))))


