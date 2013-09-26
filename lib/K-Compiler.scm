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
    (cond
      [(string?  x) #t]
      [(number?  x) #t]
      [(boolean? x) #t]
      [(char?    x) #t]
      [else #f]))

  (define-syntax fn
    (syntax-rules ()
      ((_ (arg ...) exp ...)
      (lambda (arg ...) exp ...))))

  ;;; Compiler ;;;

  ;;compile :: Nadeko -> Krivine
  (define (compile program)
    (let ([code (cons `(,CLOSURE ((,STOP))) (concatenate (map (fn (x) (compile- `(,x))) program)))])
      (print code)
      (append code `((,CONTINUE)))))

  (define (compile- program)
    (if (null? program) '()
      (let ([exp (car program)] [code-r (cdr program)])

        (cond
          [(atom? exp)
           (cons `(,CONSTANT ,exp) (compile- code-r))]

          [(symbol? exp)
           (cons `(,ACCESS ,exp) (compile- code-r))]

          [(eq? (car exp) 'quote)
           (cons `(,CONSTANT ,exp) (compile- code-r))]

          [(eq? (car exp) '**)
           ;(** + 2 3)
           (append `((,PMARK) (,CLOSURE ((,PRIMITIVE ,(cadr exp)) (,CONTINUE))))
                   (compile- (reverse (cddr exp))))]

          [(eq? (car exp) '=)
           ;(= f a b a)
           (append
            (cons `(,CLOSURE ,(append (compile- `((^ ,@(drop-right (cddr exp) 1) ,(last exp))))
                                      `((,CONTINUE))))
                   `((,DEFINE ,(cadr exp))))
             (compile- code-r))]

          [(eq? (car exp) '^)
           ;;(^ a b f (f a b))
           (append (map (fn (x) `(,GRAB ,x)) (drop-right (cdr exp) 1))
                   (compile- (list (last exp))))]
          ;;doesn't have to take care of the rest because lambdas always end with CONTINUE

          [else
           ;(f a b c)
           (append
            (map (fn (x) `(,CLOSURE ,(append (compile- `(,x)) `((,CONTINUE)))))
                 (reverse (cdr exp)))
              (compile- `(,(car exp))))])))))
