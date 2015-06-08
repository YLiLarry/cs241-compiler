#lang racket

(require "../io.rkt")
(require test-engine/racket-tests)

(check-expect 
    (split-lines "
        .word label
               
        label:
    ")
    (list
        ".word label"
        "label:"
    )
)

(test)

