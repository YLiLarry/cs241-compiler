#lang typed/racket

(provide (all-defined-out))

(: read-input (-> String))
(define (read-input)
    (let ([in (read-line)])
        (cond 
            [(eof-object? in) ""]
            [else (string-append in "\n" (read-input))]
        )
    )
)

