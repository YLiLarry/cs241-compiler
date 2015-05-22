#lang typed/racket

(provide (all-defined-out))
    
(require "assembler.rkt")
       
(define-type Stmt (U Inst Inst-ext))
(define-type Inst-ext (U 
    Inst-if 
    Inst-while
    Inst-assign
))

(struct Inst-assign ([reg : Reg] [val : (U Inst-assign-imm Inst-assign-result)]) #:transparent)
(struct Inst-assign-imm ([val : Val]) #:transparent)
(struct Inst-assign-result ([op : Op] [r1 : Reg] [r2 : Reg]) #:transparent)

(struct Inst-if ([cond : Reg] [true : (Listof Stmt)] [false : (Listof Stmt)]) #:transparent)
(struct Inst-while ([cond : Reg] [true : (Listof Stmt)]) #:transparent)
