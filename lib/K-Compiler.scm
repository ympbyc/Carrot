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


  ;;compile :: [expr] -> {'name => [closure]}
  ;; (= (<name> <T> ...) <expr>)
  (define (compile program)
    (fold (fn [def binding]
              (let* (;;[def    (if (proper-def? def) def `(= main ,def))]
                     [name   (caadr def)]
                     [sign   (cdadr def)]
                     [params (drop-right (cddr def) 1)]
                     [expr   (last def)]
                     [body   `(^ ,@params ,expr)])
                (hash-table-put! binding name
                                 (cons-anyway
                                  (ndk-closure (expand-expr body '()) '() sign)
                                  (ref binding name)))
                binding))
          (make-hash-table 'eq?)
          program))

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

  (define (expand-expr exp env)
    (cond
     [(and (symbol? exp) (member exp env))
      `(,REF ,exp)]

     [(symbol? exp)
      `(,REFG ,exp)]

     [(atom? exp)
      `(,ATOM ,exp)]

     [(quote-expr? exp)
      `(,ATOM ,(cadr exp))]

     ;;(^ x M)
     [(lambda-expr? exp)
      (let ([params (drop-right (cdr exp) 1)])
        (curry-lambda params (expand-expr (last exp) (append env params))))]

     ;;(** + M L)
     [(native-expr? exp)
      (expand-app `(,NATIVE ,(cadr exp)) (cddr exp) env)]

     [else
      ;;(f a b c)
      (let ([exp (macroexpand exp)])
        (expand-app (expand-expr (car exp) env) (cdr exp) env))])))
