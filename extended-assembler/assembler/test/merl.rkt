#lang racket

(require "../merl.rkt")
(require "../parse.rkt")
(require "../internal.rkt")
(require test-engine/racket-tests)

(define (revert ls)
    (map (lambda (x) (integer-bytes->integer x #f #t)) ls)
)

; one label

(check-expect 
    (let-values ([(a b) (merl (split-lines "
        .word label
        label:
    "))]) (revert a))
    (list
        ; header
        #x10000002
        #x00000018
        #x00000010 
        ; body
        #x00000010
        ; footer
        #x00000001
        #x0000000c
    )
)
    

; empty
(check-expect
    (let-values ([(a b) (merl (split-lines ""))]) (revert a))
    (list
        ; header
        #x10000002
        #x0000000c
        #x0000000c
    )
)

; many labels
(check-expect 
    (let-values ([(a b) (merl (split-lines "
        .word label
        .word label2
        .word label2
        .word label
        label:
        .word label ; 44 = 0x2c
        label2:
        .word label ; 48 = 0x30
    "))]) (revert a))
    (list
        ; header
        #x10000002
        #x00000054
        #x00000024
        ; body
        #x0000001c
        #x00000020
        #x00000020
        #x0000001c
        #x0000001c
        #x0000001c
        ; footer
        #x00000001 
        #x0000000c 
        #x00000001 
        #x00000010 
        #x00000001 
        #x00000014 
        #x00000001 
        #x00000018 
        #x00000001 
        #x0000001c 
        #x00000001 
        #x00000020
    )
)
; many labels
(check-expect 
    (let-values ([(a b) (merl (split-lines "
        label1: lis $3
        label2: .word label2
        jr $31
    "))]) (revert a))
    (list
        ; header
        #x10000002
        #x00000020
        #x00000018
        ; body
        #x00001814
        #x00000010
        #x03e00008
        ; footer
        #x00000001 
        #x00000010
    )
)

(test)
