#lang racket

(require 
  racket/format
  "Parser.rkt"
  "Env.rkt"
  "Translator.rkt"
  )

(define (to-string s)
 s)

(define (evaluate path)
(eval-cmd (parse-file path) empty-env)
 )
; if no results received use translator one
(define CODE_PATH "Sample.txt")
(define res (evaluate CODE_PATH))

res


;(define CODE_PATH "tests/test-list-member.txt")
;(define CODE_PATH "local-test.txt")
;
;(define parsed-code (parse-file CODE_PATH))
;
;
;parsed-code
;"---------------------"
;(define final-env (eval-commands parsed-code initial-env))
;"---------------------"
;final-env
;initial-env
