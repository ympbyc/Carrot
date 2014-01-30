;;;; Carrot -> CarrotVM instruction ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;

(add-load-path "../lib/" :relative)

(define-module to-carrot-vm
  (export compile)
  (use srfi-1)
  (use DataTypes)
  (use Util)

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
        (make <crt-inst-lambda>
          :parameter (car params)
          :expression (curry-lambda (cdr params) expr))))


  (define (appv? ag env)
    (and (eq? (class-of ag) <crt-inst-ref>)
         (member (var ag) env)))


  ;; (f x y z) -> (((f x) y) z)
  (define (expand-app f args env)
    (if (null? args)
        f
        (let ([ag (expand-expr (car args) env)])
          (expand-app (make (if (appv? ag env) <crt-inst-appvar> <crt-inst-app>)
                        :operator f :operand ag)
                      (cdr args)
                      env))))

  (define (expand-expr expr env)
    ;;(p (show-typed-expr tx))
    (cond
     [(and (symbol? expr) (member expr env))
      (make <crt-inst-ref> :var expr)]

     [(symbol? expr)
      (make <crt-inst-refg> :var expr)]

     [(string? expr)
      (make <crt-inst-atom> :val expr)]

     [(atom? expr)
      (make <crt-inst-atom> :val expr)]

     [(quote-expr? expr)
      (make <crt-inst-atom> :val expr)]

     ;;(^ x M)
     [(lambda-expr? expr)
      (let ([params (drop-right (cdr expr) 1)])
        (curry-lambda params
                      (expand-expr (last expr)
                                   (append env params))))]

     ;;(** + M L)
     [(native-expr? expr)
      (expand-app (make <crt-inst-native> :procedure (cadr expr))
                  (cddr expr) env)]

     [else
      ;;(f a b c)
      (let ([exp (macroexpand expr)])
        (expand-app (expand-expr (car exp) env)
                    (cdr exp) env))])))
