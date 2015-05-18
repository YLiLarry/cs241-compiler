#lang typed/racket

(require "../type.rkt")
(require "../../word.rkt")

#|
Stmt = Inst
     | Inst-ext
    
Inst-ext = if ( Reg ) then ( Stmt ... ) else ( Stmt ... )
         | def Reg def-val

def-val = Inst-assign-imm = Real-number
        | Inst-assign-result = Op Reg Reg
        
|#
       
(define-type Stmt (U Inst Inst-ext))
(define-type Inst-ext (U Inst-if Inst-assign))

(struct Inst-assign ([reg : Reg] [val : (U Inst-assign-imm Inst-assign-result)]))
(struct Inst-assign-imm ([val : Val]))
(struct Inst-assign-result ([op : Op] [r1 : Reg] [r2 : Reg]))

(struct Inst-if ([cond : Reg] [true : (Listof Stmt)] [false : (Listof Stmt)]))

(: preprocess-one (Stmt -> (Listof Inst)))
(define (preprocess-one stmt)
    (match stmt
        [(Inst-if reg if-true if-false) (let (
            [if-true-parsed (append-map preprocess-one if-true)]
            [if-false-parsed (append-map preprocess-one if-false)])
            (append 
                (list (Inst-sti 'beq 0 reg (+ 1 (length if-true-parsed))))
                if-true-parsed
                (list (Inst-sti 'ben 0 reg (length if-false-parsed)))
                if-false-parsed
            )
        )]
        [(Inst-assign reg val) (match val
            [(Inst-assign-imm im) (list (Inst-d 'lis reg) (Word im))]
            [(Inst-assign-result op r1 r2) (list (match op
                ['+ (Inst-std 'add  reg r1 r2)]
                ['- (Inst-std 'sub  reg r1 r2)]
                ['* (Inst-std 'mult reg r1 r2)]
                ['/ (Inst-std 'div  reg r1 r2)]
            ))]
        )]
        [(and x (Inst-std op s t d)) (list x)]
    )
)

(: inst->string (Inst -> String))
(define (inst->string inst)
    (match inst
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


(preprocess-one (Inst-if 4 (list (Inst-std 'add 3 2 1)) (list (Inst-std 'add 3 2 3))))

(preprocess-one (Inst-assign 3 (Inst-assign-result '+ 2 1)))
(map inst->string (preprocess-one (Inst-assign 3 (Inst-assign-imm 15))))
