;;;; Nadeko -> Krivine's Machine instruction ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;

(define-module Compiler
  (extend Krivine)
  (export compile)
  
  ;;Helper
  (define (atom? x)
    (cond
      [(string?  x) #t]
      [(number?  x) #t]
      [(boolean? x) #t]
      [(char?    x) #t]
      [else #f]))
  
  ;;curry
  (define (curry-grabs params)
    (map (lambda (p) `(,GRAB ,p)) params))
  
  ;;partial-application
  (define (partial-arg args continuation)
    (map (lambda (arg)
      `(,CLOSURE ,(compile- arg continuation))) (reverse args)))
  
  ;;stack all the arguments to the primitive procedure
  ;;and apply the procedure
  (define (primitive-compile args prim)
    (if (null? args)
      prim
      (compile- (car args) (primitive-compile (cdr args) prim))))
  
  
  ;;; Compiler ;;;
  
  ;;compile :: Nadeko -> Krivine
  (define (compile program)
    (fold-right compile- `((,STOP)) program))
  
  ;;compile- :: Nadeko -> code -> code
  (define (compile- exp code)
    ;(print (format "exp : ~S" exp))
    ;(print (format "code: ~S" code))
    ;(newline)
    (cond
      [(atom? exp)
       (cons `(,CONSTANT ,exp) code)]
  
      [(symbol? exp)
       `((,ACCESS ,exp) (,CONTINUE))] ;;CONTINUE dumps the remaining code
  
      [(eq? (car exp) 'quote)
       (cons `(,CONSTANT ,(cadr exp)) code)]
  
      [(eq? (car exp) '**)
       ; (** / 3 2)
       (cons 
        (append `(,PRIMITIVE ,(cadr exp)) (map (lambda (x) (compile- x `((,STOP)))) (cddr exp)))
        code)]
  
      [(eq? (car exp) ':=)
       (let ([body (compile- (caddr exp) `((,STOP)))]) ;;if no CONTINUE it must be STOP
         (cons 
           `(,CLOSURE ,(append (curry-grabs (cdadr exp)) body)) 
            (cons `(,DEFINE ,(caadr exp)) code)))]
  
      [(eq? (car exp) '->)
       (append (curry-grabs (cadr exp)) (compile- (caddr exp) code))]
  
      [else
       (append (partial-arg (cdr exp) code)
        (compile- (car exp) code))])))