#lang racket

(require "../merl.rkt")
(require "../parse.rkt")
(require "../internal.rkt")
(require test-engine/racket-tests)

(check-expect
    (let* ([x (tokenize-file "
            .word label
            label:
    ")] [tb (fst-pass x)])
    (build-footer x tb))
    
    (list
        (Word 4)
    )
)


(check-expect
    (let* ([x (tokenize-file "
        .word label
        .word label
        .word label
        .word label
        label:
    ")] [tb (fst-pass x)])
    (build-footer x tb))
    
    (list
        (Word 16)
    )
)

(test)
