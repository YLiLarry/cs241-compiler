#lang racket

(require "../word.rkt")
(require test-engine/racket-tests)

(check-expect (nat->binary #x80000000) "10000000000000000000000000000000")
(check-expect (int->word32 #x80000000) "10000000000000000000000000000000")
(check-expect (int->word 16 (- #x8000 #x80000)) "1000000000000000")
(check-expect (int->word 16 -1) "1111111111111111")
(check-expect (int->word 16 #x0) "0000000000000000")
(check-expect (int->word 16 #x7fff) "0111111111111111")
(check-expect (int->word 16 #x8000) "1000000000000000")
(check-expect (int->word 16 #xffff) "1111111111111111")

(test)
