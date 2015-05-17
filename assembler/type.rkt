#lang typed/racket

(provide (all-defined-out))

(define-type Val Integer)
(define-type Reg Natural)
(define-type Op Symbol)
; (define-type Inst (U Inst-1 Inst-2 Inst-4 Inst-3))
; (define-type Inst-1 (List Op Reg))
; (define-type Inst-2 (List Op Reg Reg))
; (define-type Inst-4 (List Op (List Reg Reg) (List Val)))
; (define-type Inst-3 (List Op Reg Reg Reg))

(define-type Inst (U Inst-r Inst-i Inst-m Inst-j))

(struct Inst-r ([op : Op] [rd : Reg] [rs : Reg] [rt : Reg]))
(struct Inst-i ([op : Op] [rd : Reg] [rs : Reg] [vl : Val]))
(struct Inst-m ([op : Op] [rd : Reg] [rs : Reg]))
(struct Inst-j ([op : Op] [rd : Reg]))
