#lang racket
(provide empty-env env-assign env-get env-from-list in-env? initial-env
         has-returned extend-env)

(define empty-env (list 'empty))


(define (extend-env env var val)
 (list 'extend var val env))

(define (in-env? env var)
 (case (car env)
  [(empty) #f]
  [(extend)
  (or (eqv? (cadr env) var) (in-env? (cadddr env) var))
  ]
  [else (raise "enviroment is not valid")]
 )
 )

(define (replace-var env var val)
 (case (car env)
  [(extend)
   (if (eqv? (cadr env) var)
    (extend-env (cadddr env) var val)
    (extend-env (replace-var (cadddr env) var val) (cadr env) (caddr env))
   )]
  [(empty) (raise "variable is not defined")]
  [else (raise "enviroment is not valid")]
 )
 )

(define (env-assign env var val)
 (if (in-env? env var) (replace-var env var val) (extend-env env var val)))

(define (env-get env var)
 (case (car env)
  [(extend)
   (if (eqv? (cadr env) var)
    (caddr env)
    (env-get (cadddr env) var)
   )]
  [(empty) (raise "variable is not defined")]
  [else (raise "enviroment is not valid")]
 )
 )

(define (env-from-list li)
 (if (null? li) empty-env (extend-env (env-from-list (cddr li)) (car li) (cadr li)))
)
(define initial-env (env-assign empty-env 'ret 'None))
;(define initial-env empty-env)
(define (has-returned env)
 (and
  (in-env? env 'ret)
  (not (eqv? (env-get env 'ret) 'None))
  )
 )

;text
;(define env
;  empty-env)
;env
;(set! env (extend-env (extend-env empty-env 'a 9) 'b 10))
;env
;(set! env (replace-var env 'b 21))
;env
;(set! env (replace-var env 'a 201))
;env
;(set! env (env-assign env 'a 9))
;env
;(set! env (env-assign env 'c 3))
;env
;(in-env? env 'a)
;(in-env? env 'b)
;(in-env? env 'c)
;(in-env? env 'd)
;(env-get env 'a)
;(env-get env 'b)
;(env-get env 'c)
;(env-get env 'd)
;(env-get (env-from-list (list 'a 1 'b 2 'c 5 'd 11)) 'd)
;(env-from-list (list))
