#lang typed/racket

(provide (all-defined-out))

(: chars->nat ((Listof Char) -> Natural))
(define (chars->nat ls)
    (: rec ((Listof Char) Natural -> Natural))
    (define (rec ls e) 
        (cond 
            [(empty? ls) 0]
            [else (+ (* (expt 10 e) (char->digit (first ls))) (rec (rest ls) (+ e 1)))]
        )
    )
    
    (rec (reverse ls) 0)
)

(: char->digit (Char -> Natural))
(define (char->digit x)
    (match x
        [#\0 0]
        [#\1 1]
        [#\2 2]
        [#\3 3]
        [#\4 4]
        [#\5 5]
        [#\6 6]
        [#\7 7]
        [#\8 8]
        [#\9 9]
        [else 0]
    )
)

(: string->nat (String -> Natural))
(define (string->nat str) (chars->nat (string->list str)))
