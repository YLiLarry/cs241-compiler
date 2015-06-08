#lang typed/racket

(provide (all-defined-out))

(: read-input (-> (Listof String)))
(define (read-input)
    (let ([in (read-line)])
        ; (print in (current-error-port))
        (cond 
            [(eof-object? in) empty]
            [(string=? "" in) (read-input)]
            [else (cons in (read-input))]
        )
    )
)

(: split-lines (String -> (Listof String)))
(define (split-lines str)
    (filter 
        (lambda (elem) (not (equal? "" elem)))
        (map 
            (lambda ([x : String]) (string-trim x)) 
            (string-split str #px"\n") 
        )
    )
)
    