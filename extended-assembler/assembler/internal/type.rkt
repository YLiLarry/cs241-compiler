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

(define-type Inst (U Word Inst-std Inst-st Inst-d Inst-sti Inst-s))

(struct Inst-std ([op : Op] [rd : Reg] [rs : Reg] [rt : Reg]) #:transparent)
(struct Inst-st ([op : Op] [rs : Reg] [rt : Reg]) #:transparent)
(struct Inst-d ([op : Op] [rd : Reg]) #:transparent)
(struct Inst-sti ([op : Op] [rs : Reg] [rt : Reg] [im : Val]) #:transparent)
(struct Inst-s ([op : Op] [rd : Reg]) #:transparent)
(struct Word ([val : Val]) #:transparent)
