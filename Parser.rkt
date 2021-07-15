#lang racket
(require "Lexer.rkt"
  parser-tools/yacc
  )

(define (decode-comp x)
 (case x
  [("==") 'equals]
  [(">" )'gstrict]
  [("<" )'lstrict]
 )
 )


(define simple-math-parser
           (parser
            (start statements)
            (end EOF)
            (debug "debug.txt")
            (error void)
            (tokens a b)
            (grammar
             (
              statements
              ((statement semicolon) (list 'statement $1))
              ((statements statement semicolon)
               (list 'statements $1 (list 'statement $2)))
             )
             (
              statement
              ((compoundstm) $1)
              ((simplestm) $1)
             )
             (
              simplestm
              ((VAR assign exp) (list 'assign $1 $3))
              ((return exp) (list 'return $2))
              ((return) (list 'return))
              ((global VAR) (list 'global $2))
              ((pass) (list 'pass))
              ((break) (list 'break))
              ((continue) (list 'continue))
             )
             (
              compoundstm
              ((print op exp cp) (list 'print $3))
              ((def VAR op params cp colon statements) (list 'funcdef $2 $4 $7))
              ((def VAR op cp colon statements) (list 'funcdef $2 $6))
              ((if exp colon statements else colon statements) (list 'if $2 $4 $7))
              ((for VAR in exp colon statements) (list 'for $2 $4 $6))
             )
             (
              params
              ((VAR assign exp) (list 'params (list 'paramdef $1 $3)))
              ((params comma VAR assign exp) (list 'params $1 (list 'paramdef $3 $5)))
             )
             (
              exp
              ((disj) $1)
             )
             (
              disj
              ((conj) $1)
              ((disj or conj) (list 'or $1 $3))
             )
             (
              conj
              ((inversion) $1)
              ((conj and inversion) (list 'and $1 $3))
             )
             (
              inversion
              ((not inversion) (list 'not $2))
              ((comparison) $1)
             )
             (
              comparison
              ((sum compareopsumpairs) (list 'comp $1 $2))
              ((sum) $1)
             )
             (
              compareopsumpairs
              ((compareopsumpair) $1)
              ((compareopsumpairs compareopsumpair) (list 'comppairs $1 $2))
             )
             (
              compareopsumpair
              ((COMP sum) (list (decode-comp $1) $2))
             )
             (
              sum
              ((sum plus term) (list 'plus $1 $3))
              ((sum minus term) (list 'minus $1 $3))
              ((term) $1)
             )
             (
              term
              ((term mult factor) (list 'mult $1 $3))
              ((term div factor) (list 'div $1 $3))
              ((factor) $1)
             )
             (
              factor
              ((plus factor) (list 'plus2 $2))
              ((minus factor) (list 'minus2 $2))
              ((power) $1)
             )
             (
              power
              ((atom pow factor) (list 'pow $1 $3))
              ((primary) $1)
             )
             (
              primary
              ((atom) $1)
              ((primary ob exp cb) (list 'accesslist $1 $3))
              ((primary op cp) (list 'funccallwithoutarg $1))
              ((primary op args cp) (list 'funccall $1 $3))
             )
             (
              args
              ((exp) (list 'args $1))
              ((args comma exp) (list 'args $1 $3))
             )
             (
              atom
              ((VAR) $1)
              ((BOOL) (list 'bool $1))
              ((None) (list 'none))
              ((NUM) (list 'num $1))
              ((ob exps cb) (list 'list $2))  
              ((ob cb) (list 'listempty))  
             )
             (
              exps
              ((exps comma exp) (list 'exps $1 $3))
              ((exp) $1)
             )
            )
            )
            )

(define lex-this (lambda (lexer input) (lambda () (lexer input))))

(define (parse-file path) (parse-port (open-input-file path)))
(define (parse-port port) 
 (let
;  ((my-lexer (lex-this simple-math-lexer (open-input-string path))))
  ((my-lexer (lex-this simple-math-lexer port)))
  (simple-math-parser my-lexer)
 )
 )
(define parse-string
  (lambda (str)
    (let ((my-lexer(lex-this simple-math-lexer (open-input-string str))))
       (let ((parser-res (simple-math-parser my-lexer)))
         parser-res))))
(provide parse-string)
;(define str-to-parse "a=0;b=1; c= 10;")
;(parse-string str-to-parse)
;(define str-to-parse "a=0;b=1; c= 10;")
;(parse-string str-to-parse)
;(parse-file "test-code.txt")
;test
;(define my-lexer (lex-this simple-math-lexer (open-input-string "return [8,9,10];return x[9][10]; return (((8 * 9 - 5))) * 4 ")))
;(let ((parser-res (simple-math-parser my-lexer))) parser-res)

;(define str-to-parse "for iii in 3 : ahmad=4;;")
;(define str-to-parse "a= a/b+c * 9;")

;(define my-lexer (lex-this simple-math-lexer (open-input-string str-to-parse)))
;(define parsed (let ((parser-res (simple-math-parser my-lexer))) parser-res))
;parsed
;"-------------------"
;(eval-commands parsed env)
