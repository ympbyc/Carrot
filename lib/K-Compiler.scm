;;;; Carrot -> S Machine instruction ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;

(add-load-path "../lib/" :relative)

(define-module K-Compiler
  (export compile)
  (use srfi-1)
  (use Util)
  (use Check)
  (use DataTypes)
  (extend Krivine)

  ;;; Compiler ;;;


  ;;compile :: {name => expr} -> {name => k-expr}
  ;; (= (<name> <T> <T>) <expr>)
  (define (compile exprs-ht)
    (alist->hash-table
     (hash-table-map exprs-ht
                     (fn [k expr] (cons k (make <nadeko-closure>
                                            :expr (expand-expr expr '())
                                            :env  '()
                                            :name k))))))


  ;; (^ x y z exp) -> (^ x (^ y (^ z exp)))
  (define (curry-lambda params expr)
    (if (null? params)
        expr
        `(,FN ,(car params) ,(curry-lambda (cdr params) expr))))


  (define (appv? ag env)
    (and (eq? (car ag) REF)
         (member (cadr ag) env)))


  ;; (f x y z) -> (((f x) y) z)
  (define (expand-app f args env)
    (if (null? args)
        f
        (let ([ag (expand-expr (car args) env)])
          (expand-app (list (if (appv? ag env) APPVAR APP) f ag)
                      (cdr args)
                      env))))

  (define (expand-expr expr env)
    ;;(p (show-typed-expr tx))
    (cond
     [(and (symbol? expr) (member expr env))
      `(,REF ,expr)]

     [(symbol? expr)
      `(,REFG ,expr)]

     [(atom? expr)
      `(,ATOM ,expr)]

     [(quote-expr? expr)
      `(,ATOM ,(cadr expr))]

     ;;(^ x M)
     [(lambda-expr? expr)
      (let ([params (drop-right (cdr expr) 1)])
        (curry-lambda params
                      (expand-expr (last expr)
                                   (append env params))))]

     ;;(** + M L)
     [(native-expr? expr)
      (expand-app `(,NATIVE ,(cadr expr)) (cddr expr) env)]

     [else
      ;;(f a b c)
      (let ([exp (macroexpand expr)])
        (expand-app (expand-expr (car exp) env)
                    (cdr exp) env))])))
