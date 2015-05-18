#lang racket/base

(require "parse.rkt")
(require "translate.rkt")

(define (main) (
    (displayln "Waiting for input...")
    (let ([x (read-line)]) (
        (when (eof-object? x) (exit))
        (displayln (inst->bin (parse-inst (split-line x))))
        (main)
    ))
))

(main)

