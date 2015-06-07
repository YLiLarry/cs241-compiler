#lang typed/racket

(require "parse.rkt")
(require "internal.rkt")
(provide build-footer)

(: build-footer ((Listof (Listof token)) Label-Table -> (Listof Word)))
(define (build-footer ls tb)
    (define tb (fst-pass ls))
    (unique (foldl 
        (lambda ([xs : (Listof token)] [acc : (Listof Word)])
            (match xs
                [(list 
                    (token 'dotword _) 
                    (token 'id (? list? ls)))
                    (cons (Word (lookup-label ls tb)) acc)
                ]
                [else acc]
            )
        )
        empty
        ls
    ))
)
