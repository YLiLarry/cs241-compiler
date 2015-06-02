#lang racket

(require "mips.rkt")
(require "internal.rkt")

(define (main) 
    (let* ([in (read-input)]
           [tb (fst-pass in)])
        (print-label-table tb)
        (for-each (lambda (x) (display x)) (snd-pass in tb))
    )
)

(main)
