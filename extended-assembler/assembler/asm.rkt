#lang racket

; This file is the 'main' program

(require "mips.rkt")
(require "parse.rkt")
(require "translate.rkt")
(require "internal.rkt")

(define (main) 
    (let* ([in (read-input)]
           [file (tokenize-file in)]
           [tb (fst-pass file)])
        (print-label-table tb)
        (for-each (lambda (x) (display (inst->mips x))) (snd-pass file tb))
    )
)

(main)
