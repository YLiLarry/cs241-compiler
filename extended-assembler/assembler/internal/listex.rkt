#lang typed/racket

(provide (all-defined-out))

(: group (All (a) Natural (Listof a) -> (Listof (Listof a)))) 
(define (group size ls) 
    (cond
        [(empty? ls) empty]
        [else (let-values ([(h t) (split-at ls size)]) (cons h (group size t)))]
    )
)

(: string-group (Natural String -> (Listof String)))
(define (string-group size str) (map list->string (group size (string->list str))))
