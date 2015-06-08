#lang typed/racket

; This file is the 'main' program

(require "mips.rkt")
(require "parse.rkt")
(require "merl.rkt")
(require "internal.rkt")


(define (main) 
    (let*-values ([(in) (read-input)]
                  [(bytes table) (merl in)])
        (print-label-table table)
        (for-each (lambda ([x : Bytes]) (display x)) bytes)
    )
)

(main)
