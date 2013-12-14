;;;; Carrot -> S Machine instruction ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;

(add-load-path "../lib/" :relative)

(define-module K-Compiler
  (export compile)
  (use srfi-1)
  (use Util)
  (extend Krivine)

  ;;; Compiler ;;;

  (define (proper-def? def)
    (and (pair? def) (eq? (car def) '=)))


  ;;compile :: typed-expr * {types} -> {'name => instruction}
  ;; (= (<name> <T> <T>) <expr>)
  (define (compile t-expr types)
    (ndk-closure (expand-expr t-expr '()  types) '()))

  ;; (^ x y z exp) -> (^ x (^ y (^ z exp)))
  (define (curry-lambda params expr)
    (if (null? params)
        expr
        `(,FN ,(car params) ,(curry-lambda (cdr params) expr))))


  (define (appv? ag env)
    (and (eq? (car ag) REF)
         (member (cadr ag) env)))


  ;; (f x y z) -> (((f x) y) z)
  (define (expand-app f args env types)
    (if (null? args)
        f
        (let ([ag (expand-expr (car args) env types)])
          (expand-app (list (if (appv? ag env) APPVAR APP) f ag)
                      (cdr args)
                      env
                      types))))

  (define (expand-expr tx env types)
    ;;(p (show-typed-expr tx))
    (let ([expr-type (if (typed-expr? tx) (tx-type tx) '())]
          [expr      (if (typed-expr? tx) (tx-expr tx) tx)])
      (cond
       [(and (symbol? expr) (member expr env))
        `(,REF ,expr)]

       [(and (list? expr-type) (symbol? expr))
        (if (null? expr-type)
            (expand-expr (car (ref types expr)) env types)
            (tx-expr (expand-expr (find
                                   (fn [fx] (equal? expr-type (tx-type fx)))
                                   (ref types expr))) env types))]

       [(atom? expr)
        `(,ATOM ,expr)]

       [(quote-expr? expr)
        `(,ATOM ,(cadr expr))]

       ;;(^ x M)
       [(lambda-expr? expr)
        (let ([params (drop-right (cdr expr) 1)])
          (curry-lambda params
                        (expand-expr (last expr)
                                     (append env params)
                                     types)))]

       ;;(** + M L)
       [(native-expr? expr)
        (expand-app `(,NATIVE ,(cadr expr)) (cddr expr) env types)]

       [else
        ;;(f a b c)
        (let ([exp (macroexpand expr)])
          (expand-app (expand-expr (car exp) env types)
                      (cdr exp) env types))]))))
