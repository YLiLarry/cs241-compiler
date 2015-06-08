#lang racket

(require "../parse.rkt")
(require "../internal.rkt")
(require test-engine/racket-tests)

(check-expect
    (fst-pass (tokenize-file (split-lines "
        .word 445
        a:
        b:
    ")) 0)
    '((a . 4) (b . 4))
)

(check-expect
    (fst-pass (tokenize-file (split-lines "
        A:
        add $1,$1,$2
        B: C:
        jr $31
        D:
    ")) 0)
    '((A . 0) (B . 4) (C . 4) (D . 8))
)

(check-expect 
    (fst-pass (tokenize-file (split-lines "
        \n\nlabel1: .word 12
        \n\n label2: \n
        .word 12
    ")) 0)
    '((label1 . 0) (label2 . 4))
)

(check-expect 
    (fst-pass (tokenize-file (split-lines "
        label1: label2: .word 12
        label3:
        .word 12
    ")) 0)
    '((label1 . 0) (label2 . 0) (label3 . 4))
)

(check-error
    (fst-pass (tokenize-file (split-lines "
        label1: label1:
    ")) 0)
)


(check-error
    (fst-pass (tokenize-file (split-lines "
        label1: 
        .word 2
        label1:
    ")) 0)
)


(test)
