#lang racket

(provide scan scan-input
    (struct-out token)
)

;; scan is the main function provided, which uses the data definitions
;; and helper functions that follow. Sample tests are at the bottom of the file.
(define (scan str)
  (scan-func str transitions 'START final-states))

;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)

(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))

;; Next we specify the data definitions for tokens and the various components
;; of an FSM.

(define-struct token (kind lexeme) #:transparent)
(define (token-pprint t)
	(display (token-kind t)) (display " ") (display (token-lexeme t))
	(newline)
)

;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).

(define-struct transition (state charset next) #:transparent)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

;; The sample FSM provided is defined by (transitions, 'START, final-states).
;; Definitions of transitions and final-states follow.

;; functions used in defining sample transition table

(define (one-to-nine? ch)
  (and (char<=? #\1 ch) (char<=? ch #\9)))

(define (hex-digit? ch)
  (or
   (char-numeric? ch)
   (and (char<=? #\a ch) (char<=? ch #\f))
   (and (char<=? #\A ch) (char<=? ch #\F))))

(define (chartest ch)
  (lambda (x) (char=? x ch)))

(define (any x) true)

;; sample transition table

(define transitions
  (list
  	(make-transition 'START char-whitespace? 'WHITESPACE)
  	(make-transition 'START char-alphabetic? 'ID)
  	(make-transition 'ID char-alphabetic? 'ID)
  	(make-transition 'ID char-numeric? 'ID)
  	(make-transition 'START (chartest #\0) 'ZERO)
  	(make-transition 'ZERO char-numeric? 'ERROR)
  	(make-transition 'ZERO char-alphabetic? 'ERROR)
  	(make-transition 'START one-to-nine? 'NUM)
  	(make-transition 'NUM char-numeric? 'NUM)
  	(make-transition 'START (chartest #\() 'LPAREN)
  	(make-transition 'START (chartest #\)) 'RPAREN)
  	(make-transition 'START (chartest #\{) 'LBRACE)
  	(make-transition 'START (chartest #\}) 'RBRACE)
  	(make-transition 'START (chartest #\[) 'LBRACK)
  	(make-transition 'START (chartest #\]) 'RBRACK)
  	(make-transition 'START (chartest #\.) 'DOT)
  	(make-transition 'START (chartest #\,) 'COMMA)
  	(make-transition 'START (chartest #\;) 'SEMI)
  	(make-transition 'START (chartest #\&) 'AMP)
  	(make-transition 'START (chartest #\-) 'MINUS)
  	(make-transition 'START (chartest #\+) 'PLUS)
  	(make-transition 'START (chartest #\*) 'STAR)
  	(make-transition 'START (chartest #\/) 'SLASH)
  	(make-transition 'SLASH (chartest #\/) 'COMMENT)
  	(make-transition 'COMMENT any 'COMMENT)
  	(make-transition 'START (chartest #\%) 'PCT)
  	(make-transition 'START (chartest #\=) 'BECOMES)
  	(make-transition 'BECOMES (chartest #\=) 'EQ)
  	(make-transition 'START (chartest #\!) 'NOT)
  	(make-transition 'NOT (chartest #\=) 'NE)
  	(make-transition 'START (chartest #\<) 'LT)
  	(make-transition 'LT (chartest #\=) 'LE)
  	(make-transition 'START (chartest #\>) 'GT)
  	(make-transition 'GT (chartest #\=) 'GE)
  )
)

; sample list of final states

(define final-states
  (list
		'ID ; a string consisting of a letter (in the range a-z or A-Z) followed by zero or more letters and digits (in the range 0-9), but not equal to "wain", "int", "if", "else", "while", "println", "return", "NULL", "new" or "delete".
		'NUM ; a string consisting of a single digit (in the range 0-9) or two or more digits, the first of which is not 0; the numeric value of a NUM token cannot exceed 2^31-1
		'LPAREN ; the string "("
		'RPAREN ; the string ")"
		'LBRACE ; the string "{"
		'RBRACE ; the string "}"
		'RETURN ; the string "return" (in lower case)
		'IF ; the string "if"
		'ELSE ; the string "else"
		'WHILE ; the string "while"
		'PRINTLN ; the string "println"
		'WAIN ; the string "wain"
		'BECOMES ; the string "="
		'INT ; the string "int"
		'EQ ; the string "=="
		'NE ; the string "!="
		'LT ; the string "<"
		'GT ; the string ">"
		'LE ; the string "<="
		'GE ; the string ">="
		'PLUS ; the string "+"
		'MINUS ; the string "-"
		'STAR ; the string "*"
		'SLASH ; the string "/"
		'PCT ; the string "%"
		'COMMA ; the string ","
		'SEMI ; the string ";"
		'NEW ; the string "new"
		'DELETE ; the string "delete"
		'LBRACK ; the string "["
		'RBRACK ; the string "]"
		'AMP ; the string "&"
		'NULL ; the string "NULL"
		'ZERO
		'ERROR
		'WHITESPACE
		'COMMENT
    )
)

(define keywords (list
	'int 
	'wain
	'new 
	'delete 
	'if
	'else 
	'while 
	'return
	'println
	'NULL
))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)

(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
       (if (member state final)
           (if (or (symbol=? state 'WHITESPACE) (symbol=? state 'COMMENT))
               (reverse tacc)
               (reverse (cons (finalize-token state (reverse acc)) tacc)))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (symbol=? state 'WHITESPACE)
                 (scan-acc cl trans 'START final empty tacc)
                 (scan-acc cl trans 'START final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl)
             (error 'ERROR "NO PARSE ~a\n" (list->string cl))]
          [(symbol=? state 'COMMENT)
             (reverse tacc)]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
	(define s (string->symbol (list->string l)))
	(define upcase (compose string->symbol string-upcase list->string))
  (cond
  	[(symbol=? state 'ERROR) (error 'ERROR "NO PARSE ~a" s)]
  	[(symbol=? state 'ZERO) (make-token 'NUM 0)]
    [(symbol=? state 'NUM) (make-token 'NUM (guard-range (list->number l)))]
    [(member s keywords) (make-token (upcase l) s)]
    [else (make-token state s)]
  )
)

;; helper functions for finalize-token

(define (list->number lst) (string->number (list->string lst)))

(define (guard-range x)
	(if (> x 2147483647) (error 'ERROR "integer ~a is out of range" x) x)
)

; This file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
        (cond
          [(empty? scanned) (scan-input)]
          [else (for-each token-pprint scanned)(scan-input)])]
    [else (scan-input)]))

(scan-input)

