#lang typed/racket

; The code in this file parses the assembly lanauge into a list of Inst structure.

(require "internal.rkt")
(require/typed "tokenize.rkt"
    [#:struct token ([kind : Symbol] [lexeme : Token-Lexeme])]
    [scan (String -> (Listof token))]
)

(define-type Label-Table (Listof (Pair Symbol Natural)))
(define-type Token-Lexeme (U (Listof Char) Integer))
(define-type Token token)
(define-type Tokenized-File (Listof (Listof Token)))

(provide 
    scan
    fst-pass 
    snd-pass
    lookup-label 
    print-label-table
    Label-Table
    tokenize-file
    remove-labels 
    (struct-out token))

(: lookup-label ((Listof Char) Label-Table -> Natural))
(define (lookup-label ls tb)
    (let* ([s (string->symbol (list->string ls))] [x (assoc s tb)])
        (if x (cdr x) (error 'ERROR "LABEL '~a' NOT FOUND IN THE TABLE ~a" s tb))
    )
)

(: tokens->inst (Integer (Listof token) Label-Table -> Inst))
(define (tokens->inst linenum ls tb)
    (: member-of ((Listof Symbol) -> (Token-Lexeme -> Boolean)))
    (define (member-of ls) (lambda (op) 
        (and (list? op) (not (equal? (member (list->symbol op) ls) #f))))
    )
    (: parse-op (Token-Lexeme -> Symbol))
    (define (parse-op ls)
        (cond [(list? ls) (list->symbol ls)]
              [else (error 'ERROR "OP ~a NO PARSE")]
        )
    )
    (match ls
        ; .word with label
        [(list 
            (token 'dotword _) 
            (token 'id (? list? ls)))
            (Word (lookup-label ls tb))
        ]
        ; .word with integer
        [(list 
            (token 'dotword _) 
            (token _ (? exact-integer? v)))
            (Word v)
        ]
        ; instruction with rd
        [(list 
            (token 'id (? (member-of d-list) op)) 
            (token 'register (? exact-nonnegative-integer? rd)))
            (Inst-d (parse-op op) rd)
        ]
        ; instruction with rs
        [(list 
            (token 'id (? (member-of s-list) op)) 
            (token 'register (? exact-nonnegative-integer? rs)))
            (Inst-s (parse-op op) rs)
        ]
        ; instruction with rs rt
        [(list 
            (token 'id (? (member-of st-list) op))
            (token 'register (? exact-nonnegative-integer? rs))
            (token 'comma _)
            (token 'register (? exact-nonnegative-integer? rt)))
            (Inst-st (parse-op op) rs rt)
        ]
        ; instruction with rs rt rd
        [(list 
            (token 'id (? (member-of std-list) op))
            (token 'register (? exact-nonnegative-integer? rs))
            (token 'comma _)
            (token 'register (? exact-nonnegative-integer? rt))
            (token 'comma _)
            (token 'register (? exact-nonnegative-integer? td)))
            (Inst-std (parse-op op) rs rt td)
        ]
        ; instruction with rs rt im
        [(list 
            (token 'id (? (member-of sti-list) op))
            (token 'register (? exact-nonnegative-integer? rs))
            (token 'comma _)
            (token 'register (? exact-nonnegative-integer? rt))
            (token 'comma _)
            (token (or 'int 'hexint) (? exact-integer? im)))
            (Inst-sti (parse-op op) rs rt im)
        ]
        ; instruction with rt im(rs)
        [(list 
            (token 'id (? (member-of sti-list) op))
            (token 'register (? exact-nonnegative-integer? rt))
            (token 'comma _)
            (token (or 'int 'hexint) (? exact-integer? im))
            (token 'lparen _)
            (token 'register (? exact-nonnegative-integer? rs))
            (token 'rparen _))
            (Inst-sti (parse-op op) rs rt im)
        ]
        ; instruction with rs rt label
        [(list 
            (token 'id (? (member-of sti-list) op))
            (token 'register (? exact-nonnegative-integer? rs))
            (token 'comma _)
            (token 'register (? exact-nonnegative-integer? rt))
            (token 'comma _)
            (token 'id (? list? ls)))
            (Inst-sti (parse-op op) rs rt (- (quotient (lookup-label ls tb) 4) linenum 1))
        ]
        [else (error 'ERROR "TOKEN NO PARSE ~a" ls)]
    )
)

(: snd-pass (Tokenized-File Label-Table Integer -> (Listof Inst)))
(define (snd-pass in tb startlocation)
    (: remove-labels (token -> Boolean))
    (define (remove-labels x)
        (match x [(token 'label _) #f] [else #t])
    )
    (define linenum (- (quotient startlocation 4) 1))
    (filter-map 
        (lambda ([line : (Listof Token)]) 
            (let ([leftover (filter remove-labels line)])
                (cond [(empty? leftover) #f]
                      [else (set! linenum (+ 1 linenum)) (tokens->inst linenum leftover tb)]
                )
            )
        ) 
        in
    )
)

(: remove-labels (Tokenized-File -> Tokenized-File))
(define (remove-labels file)
    (filter (lambda ([line : (Listof Token)]) (not (empty? line)))
        (map 
            (lambda ([line : (Listof Token)])
                (filter 
                    (lambda ([t : Token]) (match t [(token 'label _) #f] [else #t])) 
                    line
                )
            )
            file
        )
    )
)
      


(: fst-pass (Tokenized-File Natural -> Label-Table))
(define (fst-pass file startlocation)
    (: build-label-table (Tokenized-File Natural -> Label-Table))
    (define (build-label-table ls linenum)
        (cond 
            [(empty? ls) empty]
            [else (let ([x (first ls)]) 
                (cond 
                    [(has-label? x) 
                        (append (get-pairs x linenum)
                            (build-label-table 
                                (rest ls) 
                                (next-linenum x linenum)
                            )
                        )
                    ]
                    [else 
                        (build-label-table 
                            (rest ls) 
                            (next-linenum x linenum)
                    )]
                )
            )]
        )
    )
    (: has-label? ((Listof token) -> Boolean))
    (define (has-label? tokens)
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
    (: guard-duplicate (Label-Table -> Void))
    (define (guard-duplicate tb)
        (cond 
            [(empty? tb) (void)]
            [(assoc (car (first tb)) (rest tb)) (error 'ERROR "DUPLICATED LABEL ~a IN ~a" (first tb) tb)]
            [else (guard-duplicate (rest tb))]
        )
    )
        
    (let* ([result (build-label-table file startlocation)]) 
        (guard-duplicate (reverse result))
        result
    )
)

(: tokenize-file ((Listof String) -> Tokenized-File))
(define (tokenize-file str)
    ; (: normalize (String -> (U False String)))
    ; (define (normalize str)
    ;     (let* ([x (string-normalize-spaces str)])
    ;         (cond [(string=? x "") #f]
    ;               [else x]
    ;         )
    ;     )
    ; )
    ; (let ([lines (filter-map normalize (string-split str #px"\n"))])
        (map scan str)
    ; )
)

; (: tokenize-file ((Listof String) -> Tokenized-File))
; (define (tokenize ls)
;     (map scan ls)
; )

(: print-label-table (Label-Table -> Void))
(define (print-label-table tb) 
    (for-each (lambda ([x : (Pairof Symbol Natural)]) 
        (display (car x) (current-error-port))
        (display " " (current-error-port))
        (display (cdr x) (current-error-port))
        (newline (current-error-port))
    ) tb)
)


