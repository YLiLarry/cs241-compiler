#lang typed/racket

(require "type.rkt")
(require "assembler.rkt")

(provide (all-defined-out))

#|
Stmt = Inst
     | Inst-ext
    
Inst-ext = if ( Reg ) then ( Stmt ... ) else ( Stmt ... )
         | def Reg def-val

def-val = Inst-assign-imm = Real-number
        | Inst-assign-result = Op Reg Reg
        
|#

(: preprocess-one (Stmt -> (Listof Inst)))
(define (preprocess-one stmt)
    (match stmt
        [(Inst-if reg if-true if-false) (let (
                [if-true-parsed (append-map preprocess-one if-true)]
                [if-false-parsed (append-map preprocess-one if-false)]
            )
            (append 
                (list (Inst-sti 'beq 0 reg (+ 1 (length if-true-parsed))))
                if-true-parsed
                (list (Inst-sti 'bne 0 reg (length if-false-parsed)))
                if-false-parsed
            )
        )]
        [(Inst-while reg while-true) (let* (
                [while-true-parsed (append-map preprocess-one while-true)]
                [dist (+ 1 (length while-true-parsed))]
            )
            (append 
                (list (Inst-sti 'beq 0 reg dist))
                while-true-parsed
                (list (Inst-sti 'bne 0 reg (- 0 dist)))
            )
        )]
        [(Inst-assign reg val) (match val
            [(Inst-assign-imm im) (list (Inst-d 'lis reg) (Word im))]
            [(Inst-assign-result op r1 r2) (match op
                ['+ (list (Inst-std 'add  reg r1 r2))]
                ['- (list (Inst-std 'sub  reg r1 r2))]
                ['* (list (Inst-st  'mult r1 r2) (Inst-s 'mfhi reg))]
                ['/ (list (Inst-std 'div  reg r1 r2))]
                ['< (list (Inst-std 'slt  reg r1 r2))]
            )]
        )]
        [(and x (Inst-std op s t d)) (list x)]
        [(and x (Inst-sti op s t i)) (list x)]
        [(and x (Inst-st op s t)) (list x)]
        [(and x (Inst-s op s)) (list x)]
        [(and x (Inst-d op d)) (list x)]
        [(and x (Word w)) (list x)]
        [else (error "preprocess-one" stmt)]
    )
)

(: inst->string (Inst -> String))
(define (inst->string inst)
    (match inst
        [(Inst-sti 'lw s t i) (string-append "lw " (reg->string s) ", " (val->string i) "(" (reg->string t) ")")]
        [(Inst-sti 'sw s t i) (string-append "sw " (reg->string s) ", " (val->string i) "(" (reg->string t) ")")]
        [(Inst-std op s t d) (string-append (op->string op) " " (reg->string s) ", " (reg->string t) ", " (reg->string d))]
        [(Inst-sti op s t i) (string-append (op->string op) " " (reg->string s) ", " (reg->string t) ", " (val->string i))]
        [(Inst-st op s t) (string-append (op->string op) " " (reg->string s) ", " (reg->string t))]
        [(Inst-s op s) (string-append (op->string op) " " (reg->string s))]
        [(Inst-d op d) (string-append (op->string op) " " (reg->string d))]
        [(Word val) (string-append ".word 0x" (int->hex32 val))]
    )
)
        
(: reg->string (Reg -> String))
(define (reg->string reg) (string-append "$" (number->string reg)))
        
(: op->string (Op -> String))
(define op->string symbol->string)
        
(: val->string (Val -> String))
(define val->string number->string)


