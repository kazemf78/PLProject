#lang racket

(require "Env.rkt"
         "Parser.rkt"
         )


(provide eval-cmd)

(define (get-last lst)
  (cond
      [(null? (cdr lst)) (car lst)]
      [else (get-last (cdr lst))]
    )
  )

(define (modify-list lst keyword new-lst)
  (cond
    [(null? lst) new-lst]
    [(not (eq? (car lst) keyword)) new-lst]
    [else (modify-list (cadr lst) keyword (cons (get-last lst) new-lst))]
  )
)

(define (extend-env-with-param env paramdefs)
  (cond
    [(null? paramdefs) env]
    [else
     (let ((first (car paramdefs)))
       (let* ((lhs (cadr first))
              (rhs (caddr first)))
              (let ((new-env (env-assign env (cadr lhs) (eval-exp rhs global-env))))
                (extend-env-with-param new-env (cdr paramdefs))
                )
        )
      )
    ]
  )
)

(define (extend-env-with-param-args env paramdefs args)
  (cond
    [(null? paramdefs) env]
    [(null? args) (extend-env-with-param env paramdefs)]
    [else
     (let ((first (car paramdefs)))
       (let* (
              (lhs (cadr first))
              (argval (eval-exp (car args) global-env))
              (new-env (env-assign env (cadr lhs) argval))
             )
              (extend-env-with-param-args new-env (cdr paramdefs) (cdr args))
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
           (begin
       (change-var (cdr lst-var) src-env new-dst-env))
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
       (set! global-env env)
       (let* (
              (func-var (cadr (cadr exp)))
              (whole-func (env-get env func-var))
              (func-param (cadr whole-func))
              (func-body (caddr whole-func))
              (param-env (extend-env-with-param empty-env func-param))
              (func-env (extend-env-func param-env func-var func-param func-body))
              (func-res (eval-func-cmd func-body func-env global-env '()))
              (new-func-env (car func-res))
              (g-vars (cadr func-res))
              (new-env (change-var g-vars new-func-env global-env))
             )
         (begin
           (set! global-env new-env)
           (if (in-env? new-func-env "$")
               (env-get new-func-env "$")
               'none
           )
         )
        )
       ]
      [(funccall)
       (set! global-env env)
       (let* (
    
              (func-var (cadr (cadr exp)))
              (whole-func (env-get env func-var))
              (func-param (cadr whole-func))
              (func-body (caddr whole-func))
              (args-tmp (caddr exp))
              (args (modify-list args-tmp 'args '()))
              (param-env (extend-env-with-param-args empty-env func-param args))
              (func-env (extend-env-func param-env func-var func-param func-body))
              (func-res (eval-func-cmd func-body func-env global-env '()))
              (new-func-env (car func-res))
              (g-vars (cadr func-res))
              (new-env (change-var g-vars new-func-env global-env))
             )
         (begin
           (set! global-env new-env)
           (if (in-env? new-func-env "$")
               (env-get new-func-env "$")
               'none
           )
         )
        )
       ]
      [(none)
       'none]
      )))

(define break-flag #f)
(define global-env empty-env)
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
              (begin
                (set! global-env env)
                (define ret (eval-exp rhs env))
                (env-assign global-env (cadr lhs) ret)))]
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
                    (extend-env-func env func-name '() func-stat)
                    )
                  )
                (let* ((func-name (cadadr sub-cmd))
                       (func-param-tmp (caddr sub-cmd))
                       (func-stat (cadddr sub-cmd)))
                  (begin
                    ;((func-env (extend-env-with-param empty-env (cdr func-param))))
                      (extend-env-func env func-name (modify-list func-param-tmp 'params '()) func-stat)
                    
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
           [(print)
            (let* ((lhs (cadr sub-cmd)))
              (begin
                (displayln (eval-exp lhs env))
                env))
            ]
        ))]
      )])))

(define eval-func-cmd
  (lambda (cmd env main-env g-vars) 
    (case (car env)
      [(continue break)
         (list env g-vars)]
      [else
    (case (car cmd)
      [(statements)
       (begin
         (define ret (eval-func-cmd (cadr cmd) env main-env g-vars))
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
              (begin
                (set! global-env env)
                (define ret (eval-exp rhs env))
                (list (env-assign global-env (cadr lhs) ret) g-vars)
                )
              ;(list (env-assign env (cadr lhs) (eval-exp rhs env)) g-vars)
              )]
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
                        (set! for-env (eval-func-cmd for-stat (car for-env) main-env g-vars))
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
                         (ret-exp (cadr sub-cmd)))
                    (begin
                         (set! global-env env)
                         (define ret (eval-exp ret-exp env))
                         (list (env-assign global-env "$" ret) g-vars)
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
           [(print)
            (let* ((lhs (cadr sub-cmd)))
              (begin
                (displayln (eval-exp lhs env))
                (list env g-vars)))
            ]
        ))]
      )])))

(define (to-string s) s)

(define (evaluate path)
  (eval-cmd (parse-file path) empty-env)
  )
(define (eval-str cmd-str)
  (eval-cmd (parse-string cmd-str) empty-env)
)

;(define path "Sample.txt")
;(evaluate path)

;(define str-to-parse "a= 0; b= 0;for i in [1, 2, 3, 4, 5]:  a= a+i; if i < 3: break; else: pass;; b= b+ 2;;")
(define str-to-parse "
a = 4;

def f():
    global a;
    print(a);
    if a == 0:
    	return 5;
    else:
    	a = a - 1;
    	return f();
;;




b = f();
print(b);
b = f();
print(b);
")
(eval-str str-to-parse)
;(parse-string str-to-parse)

