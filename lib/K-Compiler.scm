;;;; Nadeko -> Krivine's Machine instruction ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;

(add-load-path "../lib/" :relative)

(define-module K-Compiler
  (export compile)
  (use srfi-1)
  (use Util)

  ;;Helper
  (define (constant-instruction? x)
    (eq? (car x) CONSTANT))

  ;;; Compiler ;;;

  ;;compile :: [expr] -> {'name => instruction}
  (define (compile program)
    (alist->hash-table
     (fold (fn [def binding]
               (if (and (pair? def) (eq? (car def) '=))
                   (let* ([name   (cadr def)]
                          [params (drop-right (cddr def) 1)]
                          [expr   (last def)]
                          [body   `(^ ,@params ,expr)])
                     (alist-cons name
                                 (ndk-closure (expand-expr body) '())
                                 binding))
                   (alist-cons 'main
                               (ndk-closure (p (expand-expr `(^ ,def))) '())
                               binding)))
           '()
           program)))

  ;; (^ x y z exp) -> (^ x (^ y (^ z exp)))
  (define (curry-lambda params expr)
    (if (null? params)
        expr
        `(^ ,(car params) ,(curry-lambda (cdr params) expr))))


  ;; (f x y z) -> (((f x) y) z)
  (define (expand-app f args)
    (if (null? args)
        f
        (expand-app (list f (expand-expr (car args)))
                    (cdr args))))

  (define (expand-expr exp)
    (cond
     [(or (symbol? exp) (atom? exp))
      exp]

     ;;(^ x M)
     [(lambda-expr? exp)
      (curry-lambda (drop-right (cdr exp) 1) (expand-expr (last exp)))]

     ;;(** + M L)
     [(native-expr? exp)
      `(** ,(cadr exp) ,@(map expand-expr (cddr exp)))]

     [else
      ;;(f a b c)
      (let ([exp (macroexpand exp)])
        (expand-app (expand-expr (car exp)) (cdr exp)))])))
