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

(: list->symbol ((Listof Char) -> Symbol))
(define (list->symbol ls) (string->symbol (list->string ls)))

(: iterate (All (a) (Natural (a -> a) a -> (Listof a))))
(define (iterate n f a) 
    (: rec (All (a) (Natural (a -> a) (Listof a) -> (Listof a))))
    (define (rec n f acc)
        (cond [(equal? n 0) acc]
              [else (rec (- n 1) f (cons (f (first acc)) acc))]
        )
    )
    (rec n f (list a))
)


(: unique (All (a) ((Listof a) -> (Listof a))))
(define (unique ls) (set->list (list->set ls)))
