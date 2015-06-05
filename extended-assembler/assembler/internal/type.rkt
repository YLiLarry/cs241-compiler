#lang typed/racket

(provide (all-defined-out))

(define-type Val Integer)
(define-type Reg Natural)
(define-type Op Symbol)

(define-type Inst (U Word Inst-std Inst-st Inst-d Inst-sti Inst-s))

(struct: Inst-std ([op : Op] [rd : Reg] [rs : Reg] [rt : Reg]) #:transparent)
(struct: Inst-st ([op : Op] [rs : Reg] [rt : Reg]) #:transparent)
(struct: Inst-d ([op : Op] [rd : Reg]) #:transparent)
(struct: Inst-sti ([op : Op] [rs : Reg] [rt : Reg] [im : Val]) #:transparent)
(struct: Inst-s ([op : Op] [rd : Reg]) #:transparent)
(struct: Word ([val : Val]) #:transparent)


(define std-list (list
    'add
    'sub
    'slt
    'sltu
))

(define st-list (list
    'mult
    'multu
    'div
    'divu
))

(define s-list (list
    'jr
    'jalr
))

(define sti-list (list
    'lw
    'sw
    'beq
    'bne
))

(define d-list (list
    'mfhi
    'mflo
    'lis
))
