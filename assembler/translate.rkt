#lang typed/racket

(require "../word.rkt")
(require "type.rkt")
(provide (all-defined-out))

(: inst->bin (Inst -> String))
(define (inst->bin inst)
    (word32->hex32 (match inst
        ; std type
        [(Inst-std 'add s t d)  (replace-std s t d (remove-space "0000 00ss ssst tttt dddd d000 0010 0000"))]
        [(Inst-std 'sub s t d)  (replace-std s t d (remove-space "0000 00ss ssst tttt dddd d000 0010 0010"))]
        [(Inst-std 'slt s t d)  (replace-std s t d (remove-space "0000 00ss ssst tttt dddd d000 0010 1010"))]
        [(Inst-std 'sltu s t d) (replace-std s t d (remove-space "0000 00ss ssst tttt dddd d000 0010 1011"))]
        ; st type
        [(Inst-st 'mult s t)  (replace-st s t (remove-space "0000 00ss ssst tttt 0000 0000 0001 1000"))]
        [(Inst-st 'multu s t) (replace-st s t (remove-space "0000 00ss ssst tttt 0000 0000 0001 1001"))]
        [(Inst-st 'div s t)   (replace-st s t (remove-space "0000 00ss ssst tttt 0000 0000 0001 1010"))]
        [(Inst-st 'divu s t)  (replace-st s t (remove-space "0000 00ss ssst tttt 0000 0000 0001 1011"))]
        ; s type
        [(Inst-s 'jr s)   (replace-s s (remove-space "0000 00ss sss0 0000 0000 0000 0000 1000"))]
        [(Inst-s 'jalr s) (replace-s s (remove-space "0000 00ss sss0 0000 0000 0000 0000 1001"))]
        ; sti type
        [(Inst-sti 'beq s t i) (replace-sti s t i (remove-space "0001 00ss ssst tttt iiii iiii iiii iiii"))]
        [(Inst-sti 'bne s t i) (replace-sti s t i (remove-space "0001 01ss ssst tttt iiii iiii iiii iiii"))]
        [(Inst-sti 'lw s t i)  (replace-sti s t i (remove-space "1000 11ss ssst tttt iiii iiii iiii iiii"))]
        [(Inst-sti 'sw s t i)  (replace-sti s t i (remove-space "1010 11ss ssst tttt iiii iiii iiii iiii"))]
        ; d type
        [(Inst-d 'mfhi d) (replace-d d (remove-space "0000 0000 0000 0000 dddd d000 0001 0000"))]
        [(Inst-d 'mfho d) (replace-d d (remove-space "0000 0000 0000 0000 dddd d000 0001 0010"))]
        [(Inst-d 'lis d)  (replace-d d (remove-space "0000 0000 0000 0000 dddd d000 0001 0100"))]
        ; other type
    ))
)

(: remove-space (String -> String))
(define (remove-space str) (string-replace str " " ""))

(: bin-5 (Natural -> String))
(define (bin-5 x) 
    (when (> x 31) (error "bin-5: the given number is greater than 31"))
    (fix-binary-length 5 (nat->binary x))
)

(: bin-16 (Integer -> String))
(define (bin-16 x) (int->word16 x))

(: replace-s (Reg String -> String))
(define (replace-s s str)
    (string-replace str "sssss" (bin-5 s))
)

(: replace-t (Reg String -> String))
(define (replace-t t str)
    (string-replace str "ttttt" (bin-5 t))
)

(: replace-d (Reg String -> String))
(define (replace-d d str)
    (string-replace str "ddddd" (bin-5 d))
)
    
(: replace-i (Val String -> String))
(define (replace-i i str)
    (string-replace str "iiiiiiiiiiiiiiii" (bin-16 i))
)
    
(: replace-std (Reg Reg Reg String -> String))
(define (replace-std d s t str) (replace-d d (replace-st s t str)))

(: replace-st (Reg Reg String -> String))
(define (replace-st s t str) (replace-s s (replace-t t str)))

(: replace-sti (Reg Reg Val String -> String))
(define (replace-sti s t i str) (replace-i i (replace-st s t str)))

