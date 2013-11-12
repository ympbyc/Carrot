;;;; Nadeko -> Krivine's Machine instruction ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;

(add-load-path "../lib/" :relative)

(define-module K-Compiler
  (extend Krivine)
  (export compile)
  (use srfi-1)
  (use Util)

  ;;Helper
  (define (constant-instruction? x)
    (eq? (car x) CONSTANT))

  (define (quote-expr? x)
    (eq? (car x) 'quote))

  (define (lambda-expr? exp)
    (eq? (car exp) '^))

  (define (native-expr? exp)
    (eq? (car exp) '**))

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
                                 (nadeko-closure (append (compile-expr body) `((,CONTINUE))) '())
                                 binding))
                   (alist-cons 'main
                               (nadeko-closure (append (compile-expr `(^ ,def)) `((,CONTINUE))) '())
                               binding)))
           '()
           program)))

  (define (compile-expr exp)
    (cond
     [(symbol? exp)
      `((,ACCESS ,exp))]

     [(atom? exp)
      `((,CONSTANT ,exp))]

     [(quote-expr? exp)
      `((,CONSTANT ,exp))]

     [(lambda-expr? exp)
      ;;(^ a b f (f a b))
      (append (map (fn [x] `(,GRAB ,x)) (drop-right (cdr exp) 1))
              (compile-expr (last exp)))]

     [(native-expr? exp)
      (let ([f (cadr exp)]
            [args (cddr exp)])
        (append (flatmap compile-expr args)
                `((,NATIVE ,f ,(length args)))))]

     [else
      ;;(f a b c)
      (let ([exp (macroexpand exp)])
        (append
         (map (fn [arg]
                  (let ([insts (append (compile-expr arg))])
                    (if (and (= 1 (length insts))
                             (constant-instruction? (car insts)))
                       (car insts) ;;dont close const
                       `(,CLOSURE ,(append insts `((,CONTINUE)))))))
              (reverse (cdr exp)))
         (compile-expr (car exp))))])))
