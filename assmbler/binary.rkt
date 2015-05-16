#lang typed/racket

(require "../word.rkt")

(struct: Inst ([inst : Symbol] [vars : (Listof Natural)]))
; (struct Inst (inst vars))

(: translate-one (Inst -> String))
(define (translate-one inst)
    (match inst
        [(Inst 'slt (list rd rs rt)) (replace-r "0000 00ss ssst tttt dddd d000 0010 1010" rd rs rt)]
        [(Inst 'beq (list rs rt im)) (replace-i "0001 00ss ssst tttt iiii iiii iiii iiii" rs rt im)]
        [(Inst 'jr  (list rs))       (replace-j "0000 00ss sss0 0000 0000 0000 0000 1000" rs)]
    )
)

(: remove-space (String -> String))
(define (remove-space str) (string-replace str " " ""))

(: bin-5 (Natural -> String))
(define (bin-5 x) 
    (when (> x 31) (error "bin-5: the given number is greater than 31"))
    (fix-binary-length 5 (nat->binary x))
)
(: bin-16 (Natural -> String))
(define (bin-16 x) 
    (when (> x 131071) (error "bin-16: the given number is greater than 131071"))
    (fix-binary-length 16 (nat->binary x))
)

(: replace-r (String Natural Natural Natural -> String))
(define (replace-r x rd rs rt) 
    (string-replace
        (string-replace
            (string-replace (remove-space x) "ddddd" (bin-5 rt))
            "sssss" 
            (bin-5 rs))
        "ttttt"
    (bin-5 rt))
)

(: replace-i (String Natural Natural Natural -> String))
(define (replace-i x rs rt im) 
    (string-replace
        (string-replace
            (string-replace (remove-space x) "iiiiiiiiiiiiiiii" (bin-16 im))
            "sssss" 
            (bin-5 rs))
        "ttttt"
    (bin-5 rt))
)

(: replace-j (String Natural -> String))
(define (replace-j x rs) (string-replace (remove-space x) "sssss" (bin-5 rs)))

(translate-one (Inst 'slt (list 15 15 16)))
