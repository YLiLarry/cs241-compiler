#lang racket/base

(require "extended-assembler/parse.rkt")

(define (main)
    (define (rec str)
        (let ([x (read-line)])
            (cond 
                [(eof-object? x) (rewrite-ext str)]
                [else (rec (string-append str x))]
            )
        )
    )
    (displayln (rec ""))
)

(main)
