;;;; S-expression to SECD instruction Compiler ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://www.geocities.jp/m_hiroi/func/abcscm33.html
;;;

(load "./SECD.scm")

;;; Helpers ;;;
(define (atom? x)
  (cond
    [(string?  x) #t]
    [(number?  x) #t]
    [(boolean? x) #t]
    [(char?    x) #t]
    [else #f]))

;;uncurry function applications
(define (complis exp code)
  (if (null? exp)
    code
    (compile- `(delay ,(car exp)) (complis (cdr exp) code))))

;;curry
(define (curry params body) 
  (if (null? params)
    body
    `(,stack-closure ,(car params) (,(curry (cdr params) body) (,restore)))))

;;stack all the arguments to the primitive procedure
;;and apply the procedure
(define (primitive-compile args prim)
  (if (null? args)
    prim
    (compile- (car args) (primitive-compile (cdr args) prim))))
  
;;compile :: Lisp -> SECD
(define (compile program)
  (fold-right compile- `((,stop)) program))

;;compile- :: Lisp -> code -> code
(define (compile- exp code)
  ;(print (format "exp : ~S" exp))
  ;(print (format "code: ~S" code))
  ;(newline)
  (cond
    [(atom? exp)
      ;;(stack-constant const)
      (cons `(,stack-constant ,exp) code)]
    
    [(symbol? exp)
      ;;(ref-arg symbol) (thaw) 
      (cons `(,ref-arg ,exp) (cons `(,thaw) code))]
    
    [(eq? (car exp) 'quote)
      ;;(stack-constant symbol)
      (cons `(,stack-constant ,(cadr exp)) code)]

    [(eq? (car exp) '**) 
     ;; call primitive procedures
     (append (primitive-compile (cddr exp) `((,primitive ,(cadr exp)))) code)]
     
    
    [(eq? (car exp) ':=)
      ;;(:= (foo bar baz) (bar baz))
      ;;bound (def symbol)
      (if (< (length (cadr exp)) 2)
       (compile- `(delay ,(caddr exp)) (cons `(,def ,(caadr exp)) code)) ;no param
       (compile- `(delay (-> ,(cdadr exp) ,(caddr exp))) (cons `(,def ,(caadr exp)) code)))]
    
    [(eq? (car exp) '->)
      ;;(-> (x y z) (x y z)) = (-> (x) (-> (y) (-> (z) (x y z)))) ;auto-currying
      ;;(stack-closure symbol ((code) (restore)))
      (let ((params (cadr exp))
            (body (caddr exp)))
        (if (null? (cdr params))
          (cons `(,stack-closure ,(car params) ,(compile- body `((,restore)))) code)
          (cons `(,stack-closure ,(car params) ,(compile- `(-> ,(cdr params) ,body) `((,restore)))) code)))]
    
    [(eq? (car exp) 'delay)
     ;;(freeze ((code) (restore)))
     (cons `(,freeze ,(compile- (cadr exp) `((,restore)))) code)]
    
    [else
      ;;(foo 1 2 3) = (((foo 1) 2) 3)
      ;;arg arg ... closure (app) (app) ...
      (complis (reverse (cdr exp))
        (compile- 
          (car exp) 
          (append (map (lambda (arg) `(,app)) (cdr exp)) code)))
    ]))

