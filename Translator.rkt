#lang racket

(require "Env.rkt"
         "Parser.rkt"
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
      [(none)
       'none]
      )))

(define break-flag #f)

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
           [(continue)
            (list 'continue env)]
           [(break)
            (list 'break env)]
           [(pass)
            env]
        ))]
      )])))
(define str-to-parse "a= 0; b= 0;for i in [1, 2, 3, 4, 5]:  a= a+i; if i < 3: break; else: pass;; b= b+ 2;;")
(define env empty-env)
(eval-cmd (parse-string str-to-parse) env)
;(parse-string str-to-parse)

