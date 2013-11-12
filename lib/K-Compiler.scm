;;;; Nadeko -> Krivine's Machine instruction ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;

(add-load-path "../lib/" :relative)

(define-module K-Compiler
  (extend nadeko-primitive)
  (export compile)
  (use srfi-1)

  ;;Helper
  (define (atom? x)
    (or (string? x)
        (number? x)
        (boolean? x)
        (char? x)
        (keyword? x)))

  (define-syntax fn
    (syntax-rules ()
      ((_ (arg ...) exp ...)
       (lambda (arg ...) exp ...))))

  (define (constant-instruction? x)
    (eq? (car x) CONSTANT))

  (define (quote-expr? x)
    (eq? (car x) 'quote))

  (define (lambda-expr? exp)
    (eq? (car exp) '^))

  ;;; Compiler ;;;

  ;;compile :: [expr] -> {'name => instruction}
  (define (compile program)
    (alist->hash-table
     (fold (fn [def binding]
               (if (eq? (car def) '=)
                   (let* ([name   (cadr def)]
                          [params (drop-right (cddr def) 1)]
                          [expr   (last def)]
                          [body   `(^ ,@params ,expr)])
                     (alist-cons name (car (compile-expr body)) binding))
                   (raise "Only definitions are allowed at the top level")))
           '()
           program)))

  (define (compile-expr exp)
    (cond
     [(atom? exp)
      `((,CONSTANT ,exp))]

     [(symbol? exp)
      `((,ACCESS ,exp))]

     [(quote-expr? exp)
      `((,CONSTANT ,exp))]

     [(lambda-expr? exp)
      ;;(^ a b f (f a b))
      `((,CLOSURE ,(append (map (fn [x] `(,GRAB ,x)) (drop-right (cdr exp) 1))
                          (compile-expr (last exp)))))]

     [else
      ;;(f a b c)
      (append
       (map (fn [arg]
                (let ([inst (car (compile-expr arg))])
                  (if (constant-instruction? inst)
                      inst
                      `(,CLOSURE (,inst)))))
            (reverse (cdr exp)))
       (compile-expr (car exp))
       `((,CONTINUE))
       '())])))
