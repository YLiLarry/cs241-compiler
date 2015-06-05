#lang typed/racket

(require "internal.rkt")
(require/typed "tokenize.rkt"
    [#:struct token ([kind : Symbol] [lexeme : (U (Listof Char) Integer)])]
    [scan (String -> (Listof token))]
)

(define-type Label-Table (Listof (Pair Symbol Natural)))

(provide fst-pass snd-pass print-label-table)

(: tokens->inst ((Listof token) Label-Table -> Inst))
(define (tokens->inst ls tb)
    (match ls
        ; .word with label
        [(list (token 'dotword _) (token 'id (? list? ls))) 
            (let* ([s (string->symbol (list->string ls))] [x (assoc s tb)])
                (if x (Word (cdr x)) (error 'ERROR "LABEL '~a' NOT FOUND IN THE TABLE ~a" s tb))
            )
        ]
        ; .word with integer
        [(list (token 'dotword _) (token _ (? exact-integer? v))) 
            (Word v)
        ]
        ; jr 
        [(list (token 'id '(#\j#\r)) (token 'register (? exact-nonnegative-integer? n)))
            (Inst-s 'jr n)
        ]
        [else (error 'ERROR "TOKEN NO PARSE ~a" ls)]
    )
)

(: snd-pass (String Label-Table -> (Listof Inst)))
(define (snd-pass str tb)
    (: remove-labels (token -> Boolean))
    (define (remove-labels x)
        (match x [(token 'label _) #f] [else #t])
    )
    
    (filter-map 
        (lambda ([line : String]) 
            (let ([leftover (filter remove-labels (scan line))])
                (cond [(empty? leftover) #f]
                      [else (tokens->inst leftover tb)]
                )
            )
        ) 
        (filter (lambda ([x : String]) (not (string=? "" x))) (string-split str "\n"))
    )
)

(: fst-pass (String -> Label-Table))
(define (fst-pass str) (label-table-from-input str))


(: label-table-from-input (String -> Label-Table))
(define (label-table-from-input in)
    (: label-table-from-lines ((Listof String) Natural -> Label-Table))
    (define (label-table-from-lines ls linenum)
        (cond 
            [(empty? ls) empty]
            [else (let ([x (scan (first ls))]) 
                (cond 
                    [(has-label x) 
                        (append (get-pairs x linenum)
                            (label-table-from-lines 
                                (rest ls) 
                                (next-linenum x linenum)
                            )
                        )
                    ]
                    [else 
                        (label-table-from-lines 
                            (rest ls) 
                            (next-linenum x linenum)
                    )]
                )
            )]
        )
    )
    (: has-label ((Listof token) -> Boolean))
    (define (has-label tokens)
        (match tokens
            [(list (token 'label _) ..1 _ ...) #t]
            [else #f]
        )
    )
    (: get-pairs ((Listof token) Natural -> Label-Table))
    (define (get-pairs tokens curr)
        (: make-pairs (Any -> (Pair Symbol Natural)))
        (define (make-pairs chars) 
            (cond 
                [(list? chars)
                    (cons 
                        (string->symbol 
                            (list->string 
                                (filter (lambda (x) (char? x))
                                    (drop-right chars 1)
                                )
                            )
                        ) 
                        curr
                    )
                ]
                [else (error 'ERROR "LABEL PARSE ERROR ~a" chars)]
            )
        )
        (match tokens
            [(list (token 'label x) ..1 _ ...) (map make-pairs x)]
            [else (error 'ERROR "LABEL NO PARSE ~a" tokens)]
        )
    )
    (: next-linenum ((Listof token) Natural -> Natural))
    (define (next-linenum tokens curr)
        (match tokens
            [(list (token 'label x) ..1) curr]
            [else (+ 4 curr)]
        )
    )
    (: normalize (String -> (U False String)))
    (define (normalize str)
        (let* ([x (string-normalize-spaces str)])
            (cond [(string=? x "") #f]
                  [else x]
            )
        )
    )
    (: guard-duplicate (Label-Table -> Void))
    (define (guard-duplicate tb)
        (cond 
            [(empty? tb) (void)]
            [(assoc (car (first tb)) (rest tb)) (error 'ERROR "DUPLICATED LABEL ~a IN ~a" (first tb) tb)]
            [else (guard-duplicate (rest tb))]
        )
    )
        
    (let* ([lines (filter-map normalize (string-split in #px"\n"))] 
           [result (label-table-from-lines lines 0)]) 
        (guard-duplicate (reverse result))
        result
    )
)

(: print-label-table (Label-Table -> Void))
(define (print-label-table tb) 
    (for-each (lambda ([x : (Pairof Symbol Natural)]) 
        (display (car x) (current-error-port))
        (display " " (current-error-port))
        (display (cdr x) (current-error-port))
        (newline (current-error-port))
    ) tb)
)


