#lang racket/base

(require "assembler.rkt")

(define (main) (
    (displayln "Waiting for input...")
    (let ([x (read-line)]) (
        (when (eof-object? x) (exit))
        (displayln (inst->bin (parse-inst (split-line x))))
        (main)
    ))
))

(main)
