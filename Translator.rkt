#lang racket

(require "Env.rkt"
         "Parser.rkt"
         )

(define (extend-env-with-param env paramdefs)
  (cond
    [(null? paramdefs) env]
    [else
     (let ((first (car paramdefs)))
       (let* ((lhs (cadr first))
              (rhs (caddr first)))
              (let ((new-env (env-assign env (cadr lhs) (eval-exp rhs env))))
                (extend-env-with-param new-env (cdr paramdefs))
                )
        )
      )
    ]
  )
)

(define (change-var lst-var src-env dst-env)
  (cond
    [(null? lst-var) dst-env]
    [else
     (let* ((var (car lst-var))
           (val (env-get src-env var))
           (new-dst-env (env-assign dst-env var val)))
           
       (change-var (cdr lst-var) src-env new-dst-env)
       )]
    )
  )

(define eval-exp
  (lambda (exp env)
    (case (car exp)
      [(num)
       (cadr exp)]
      [(bool)
       (cadr exp)]
      [(var)
       (env-get env (cadr exp))]
      [(plus)
       (let* ((rs (eval-exp (cadr exp) env))
               (ls (eval-exp (caddr exp) env)))
         (if (boolean? rs)
             (or rs ls)
             (if (number? rs)
                 (+ rs ls)
                 (append rs ls))))]
      [(minus)
       (let* ((rs (eval-exp (cadr exp) env))
               (ls (eval-exp (caddr exp) env)))
         (- rs ls))]
      [(mult)
       (let* ((rs (eval-exp (cadr exp) env))
               (ls (eval-exp (caddr exp) env)))
         (if (boolean? rs)
             (and rs ls)
             (* rs ls)))]
      [(div)
       (let* ((rs (eval-exp (cadr exp) env))
              (ls (eval-exp (caddr exp) env)))
         (/ rs ls))]
      [(pow)
       (let* ((rs (eval-exp (cadr exp) env))
              (ls (eval-exp (caddr exp) env)))
         (expt rs ls))]
      [(or)
       (let* ((rs (eval-exp (cadr exp) env))
              (ls (eval-exp (caddr exp) env)))
         (or rs ls))]
      [(and)
       (let* ((rs (eval-exp (cadr exp) env))
              (ls (eval-exp (caddr exp) env)))
         (and rs ls))]
      [(not)
       (let ((rs (eval-exp (cadr exp) env)))
         (not rs))]
      [(plus2)
       (let ((rs (eval-exp (cadr exp) env)))
         rs)]
      [(minus2)
       (let ((rs (eval-exp (cadr exp) env)))
         (- 0 rs))]
      [(comp)
       (let* ((rs (eval-exp (cadr exp) env))
               (ls (eval-exp (car (cdaddr exp)) env))
               (type (caaddr exp)))
         (case type
           [(lstrict)
            (< rs ls)]
           [(gstrict)
            (> rs ls)]
           [(equals)
            (eqv? rs ls)]
           )
         )]
      [(listempty)
       '()]
      [(list)
       (let ((rs (eval-exp (cadr exp) env)))
         (if (list? rs)
             rs
             (list rs)))]
      [(exps)
       (let* ((rs (eval-exp (cadr exp) env))
              (ls (eval-exp (caddr exp) env)))
         (if (list? rs)
             (append rs (list ls))
             (list rs ls)))]
      [(accesslist)
       (let* ((rs (eval-exp (cadr exp) env))
              (ls (eval-exp (caddr exp) env)))
         (list-ref rs ls))]
      [(funccallwithoutarg)
       (displayln "here we start")
       (let* (
              (func-var (cadr (cadr exp)))
              (whole-func (env-get env func-var))
              (func-param (cadr whole-func))
              (func-body (caddr whole-func))
              (param-env (extend-env-with-param empty-env (cdr func-param)))
              (func-env (extend-env-func param-env func-var func-param func-body))
              (func-res (eval-func-cmd func-body func-env env '()))
              (new-func-env (car func-res))
              (g-vars (cadr func-res))
              (new-env (change-var g-vars new-func-env env))
             )
         (begin
           (displayln "did i get here also?")
           (if (in-env? new-func-env "$")
               (env-get new-func-env "$")
               ('none)
           )
         
        )
         )
       ]
      [(none)
       'none]
      )))

(define break-flag #f)
(define main-env empty-env)
(define global-vars '() )

(define eval-cmd
  (lambda (cmd env)
    
    (case (car env)
      [(continue break)
         env]
      [else
    (case (car cmd)
      [(statements)
       (begin
         (define new-env (eval-cmd (cadr cmd) env))
         (eval-cmd (caddr cmd) new-env)
        )]
      [(statement)
       (let ((sub-cmd (cadr cmd)))
         (case (car sub-cmd)
           [(assign)
            (let* ((lhs (cadr sub-cmd))
                   (rhs (caddr sub-cmd)))
              (env-assign env (cadr lhs) (eval-exp rhs env)))]
           [(if)
            (let* ((comp (eval-exp (cadr sub-cmd) env))
                   (ts (caddr sub-cmd))
                   (fs (cadddr sub-cmd)))
              (if comp
                  (eval-cmd ts env)
                  (eval-cmd fs env)))]
           [(for)
            (let* ((for-var (cadadr sub-cmd))
                   (for-list (eval-exp(caddr sub-cmd) env))
                   (for-stat (cadddr sub-cmd)))
              (begin
                (define for-env env)
                (for ([i for-list])
                  (if (not break-flag)
                      (begin
                        (set! for-env (env-assign for-env for-var i))
                        (set! for-env (eval-cmd for-stat for-env))
                        (case (car for-env)
                          [(continue)
                           (set! for-env (cadr for-env))]
                          [(break)
                           (begin
                             (set! for-env (cadr for-env))
                             (set! break-flag #t))]
                          ))
                    for-env  
                    ))
                  (set! break-flag #f)
                   for-env
                   ))]
           [(funcdef)
            (if (eq? (length (cdr sub-cmd)) 2)
                (let* ((func-name (cadadr sub-cmd))
                       (func-stat (caddr sub-cmd)))
                  (begin
                    (extend-env-func env func-name empty-env func-stat)
                    )
                  )
                (let* ((func-name (cadadr sub-cmd))
                       (func-param (caddr sub-cmd))
                       (func-stat (cadddr sub-cmd)))
                  (begin
                    ;((func-env (extend-env-with-param empty-env (cdr func-param))))
                      (extend-env-func env func-name func-param func-stat)
                    
                  )
                )
              )
            ]
           [(continue)
            (list 'continue env)]
           [(break)
            (list 'break env)]
           [(pass)
            env]
        ))]
      )])))

(define eval-func-cmd
  (lambda (cmd env main-env g-vars)
    (displayln "here i got, yay")
    (displayln env)
    (displayln cmd)
    (displayln " ")
    (case (car env)
      [(continue break)
         (list env g-vars)]
      [else
    (case (car cmd)
      [(statements)
       (begin
         (define ret (eval-func-cmd (cadr cmd) env main-env g-vars))
         (display 'asgharAAAAAAAAAAAAAAAAA)
         (displayln (cadr cmd))
         (displayln 'beforeshit)
         (displayln ret)
         (displayln 'shit)
         (let ((new-env (car ret))
               (new-g-vars (cadr ret)))
         (eval-func-cmd (caddr cmd) new-env main-env new-g-vars)
         )
        )]
      [(statement)
       (let ((sub-cmd (cadr cmd)))
         (case (car sub-cmd)
           [(assign)
            (let* ((lhs (cadr sub-cmd))
                   (rhs (caddr sub-cmd)))
              (list (env-assign env (cadr lhs) (eval-exp rhs env)) g-vars))]
           [(if)
            (let* ((comp (eval-exp (cadr sub-cmd) env))
                   (ts (caddr sub-cmd))
                   (fs (cadddr sub-cmd)))
              (if comp
                  (eval-func-cmd ts env main-env g-vars)
                  (eval-func-cmd fs env main-env g-vars)))]
           [(for)
            (let* ((for-var (cadadr sub-cmd))
                   (for-list (eval-exp(caddr sub-cmd) env))
                   (for-stat (cadddr sub-cmd)))
              (begin
                (define for-env (list env g-vars))
                (for ([i for-list])
                  (if (not break-flag)
                      (begin
                        (set! for-env (list (env-assign (car for-env) for-var i) g-vars))
                        (set! for-env (eval-func-cmd for-stat for-env main-env g-vars))
                        (case (caar for-env)
                          [(continue)
                           (set! for-env (list (cadar for-env)  (cadr for-env)))]
                          [(break)
                           (begin
                             (set! for-env (list (cadar for-env)  (cadr for-env)))
                             (set! break-flag #t))]
                          ))
                    for-env  
                    ))
                  (set! break-flag #f)
                   for-env 
                   ))]
           [(return)
            
              (if (eq? (length sub-cmd) 2)
                  (let* (
                         (ret-exp (cadr sub-cmd))
                         (ret-val (eval-exp ret-exp env))
                        )
                    (begin
                      (list (env-assign env "$" ret-val) g-vars)
                     )
                   )
                  (list env g-vars) 
               )
             
            ]
           [(global)
            (let* (
                   (var (cadr (cadr sub-cmd)))
                   (val (env-get main-env var))
                   (new-env (env-assign env var val))
                  )
              (begin
                
                (list new-env (append (list var) g-vars))
               )
              
             )
            ]
           [(continue)
            (list (list 'continue env) g-vars)]
           [(break)
            (list (list 'break env) g-vars)]
           [(pass)
            (list env g-vars)]
        ))]
      )])))

;(define str-to-parse "a= 0; b= 0;for i in [1, 2, 3, 4, 5]:  a= a+i; if i < 3: break; else: pass;; b= b+ 2;;")
(define str-to-parse "def f(b=0,c=1): global a; a=a+1; return a;; a = 2; b = f();")
(define env empty-env)
(eval-cmd (parse-string str-to-parse) env)
(parse-string str-to-parse)

