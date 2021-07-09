#lang racket
(provide a b simple-math-lexer)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         )

(define-lex-abbrevs
 (safe-string-char
  (:or "\\\"" (:& any-char (complement "\"")))
 )
 )

(define-lex-abbrevs
 (var-lex
  (:& (:+ alphabetic) (complement (:or "print" "do" "if" "then" "else" "while" "case" "switch" "default" "true" "false" "None" "return" "end" "pass" "break" "continue" "def" "for" "in" "or" "and" "not" "global")))
 )
 )

(define (string->var var)
 (list 'var (string->symbol (string-append "var-" var))))

(define simple-math-lexer
           (lexer
            ((:or "True" "False") (token-BOOL (if (equal? lexeme "True") #t #f)))
            ((:or (:+ numeric) (:: (:+ numeric) "." (:+ numeric))) (token-NUM (string->number lexeme)))
            (var-lex (token-VAR (string->var lexeme)))
            ((:or "==" ">" "<") (token-COMP lexeme))
            ("*" (token-mult))
            ("**" (token-pow))
            ("/" (token-div))
            ("+" (token-plus))
            ("-" (token-minus))
            ("=" (token-assign))
            ("if" (token-if))
            ("then" (token-then))
            ("while" (token-while))
            ("case" (token-case))
            ("switch" (token-switch))
            ("default" (token-default))
            ("else" (token-else))
            ("None" (token-None))
            ("true" (token-true))
            ("print" (token-print))
            ("false" (token-false))
            ("return" (token-return))
            ("end" (token-end))
            ("do" (token-do))
            ("," (token-comma))
            ("[" (token-ob))
            ("]" (token-cb))
            ("(" (token-op))
            (")" (token-cp))
            (";" (token-semicolon))
            (":" (token-colon))
            ("pass" (token-pass))
            ("break" (token-break))
            ("continue" (token-continue))
            ("def" (token-def))
            ("for" (token-for))
            ("in" (token-in))
            ("or" (token-or))
            ("and" (token-and))
            ("not" (token-not))
            ("global" (token-global))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))
            ))

(define-tokens a (NUM VAR COMP STR BOOL))
(define-empty-tokens b (EOF do print plus minus mult pow div assign if then while case switch default else None true false comma ob cb op cp semicolon colon return end pass break continue def for in or and not global))


;test
;(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this simple-math-lexer (open-input-string "if kiarash 9 8 9.8 == 8")))
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)
;(my-lexer)

