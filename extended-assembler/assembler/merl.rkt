#lang typed/racket

(require "parse.rkt")
(require "mips.rkt")
(require "internal.rkt")
(provide merl)

(: build-footer ((Listof (Listof token)) -> (Listof Word)))
(define (build-footer ls)
    (define linenum 2)
    (reverse (foldl 
        (lambda ([xs : (Listof token)] [acc : (Listof Word)])
            (match xs
                [(list 
                    (token 'dotword _) 
                    (token 'id (? list? ls)))
                    (set! linenum (+ 1 linenum))
                    (append (list (Word (* 4 linenum)) (Word 1)) acc)
                ]
                [(list (token 'label _)) acc]
                [else (set! linenum (+ 1 linenum)) acc]
            )
        )
        empty
        (remove-labels ls)
    ))
)

(: build-header ((Listof Inst) (Listof Inst) -> (Listof Inst)))
(define (build-header body footer)
    (define bl (length body))
    (define fl (length footer))
    (list
        (Inst-sti 'beq 0 0 2)
        (Word (* 4 (+ 3 bl fl)))
        (Word (* 4 (+ 3 bl)))
    )
)


(: merl ((Listof String) -> (Values (Listof Bytes) Label-Table)))
(define (merl in)
    (let* ([file (tokenize-file in)]
           [tb (fst-pass file 12)]
           [body (snd-pass file tb 12)]
           [footer (build-footer file)]
           [header (build-header body footer)]
           [result (map (lambda ([x : Inst]) (inst->mips x)) (append header body footer))])
        ; (displayln header)
        ; (displayln body)
        ; (displayln footer)
        (values result tb)
    )
)
