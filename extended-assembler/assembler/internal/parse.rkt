#lang typed/racket

(provide (all-defined-out))
    
(: find-complete-brackets (String Boolean -> (Values String String)))
(define (find-complete-brackets str include-before?)
    
    (: rec (Integer (Listof Char) -> (Listof Char)))
    (define (rec count ls)
        (cond
            [(equal? count 0) empty]
            [(empty? ls) (error "bracket mismatch" str)]
            [(equal? (first ls) #\{ ) (cons #\{ (rec (+ count 1) (rest ls)))]
            [(equal? (first ls) #\} ) (cons #\} (rec (- count 1) (rest ls)))]
            [else (cons (first ls) (rec count (rest ls)))]
        )
    )
    
    (match (regexp-match-positions #px"\\{" str)
        [(? list? pos) (let* (
                [p (first pos)]
                [i (cdr p)]
                [before (substring str 0 i)]
                [block (list->string (rec 1 (string->list (substring str i))))]
                [after (substring str (+ i (string-length block)))]
            )
            (values (string-append (if include-before? before "") block) after)
        )]
        [#f (values str "")]
    )
    
)

(: until-complete-brackets (String -> (Values String String)))
(define (until-complete-brackets str) (find-complete-brackets str #t));

(: complete-brackets (String -> (Values String String)))
(define (complete-brackets str) (find-complete-brackets str #f));


(: strict-regex-match (Regexp String -> String))
(define (strict-regex-match px str)
    (let ([x (regexp-match px str)])
        (cond 
            [(list? x) (first x)]
            [else (error "strict-regex-match" px str)]
        )
    )
)    


(: split-line (String -> (Listof String)))
(define (split-line str) 
    (: first-round-bracket (String -> String))
    (define (first-round-bracket str)
        (match (regexp-match "\\(.+?\\)" str)
            [#f (error "first-round-bracket")]
            [(? list? ls) (string-trim (first ls) #px"[()]")]
        )
    )
    
    (match (string-trim str)
        [(regexp #px"(^if)") (let*-values (
                [(after)  (strict-regex-match #px"\\{.*" str)]
                [(b1 b1r) (until-complete-brackets after)]
                [(b2 b2r) (until-complete-brackets b1r)] 
            )
            (list "if" 
                (first-round-bracket str) 
                (string-trim b1 #px"[{}]") 
                (string-trim (string-trim b2 #px"\\s*else.*\\{") #px"\\}")
            )
        )]
        [(regexp #px"(^while)") (let*-values (
                [(after)  (strict-regex-match #px"\\{.*" str)]
                [(b1 b1r) (until-complete-brackets after)]
            )
            (list "while" 
                (first-round-bracket str) 
                (string-trim b1 #px"[{}]") 
            )
        )]
        [else (string-split str " ")]
    )
)

